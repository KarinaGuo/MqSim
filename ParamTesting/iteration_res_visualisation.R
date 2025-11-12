## Analysis of parameter runs
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/")
library(tidyverse)


# Read in
#data_data <- "2025-09-05"
data_data <- "2025-09-09"

param_sets <- read.csv(file=paste0("ParamTesting/param_sets_",data_data,".csv")); colnames(param_sets)[ncol(param_sets)] <- "param_iteration"
run_status <- read.csv(file=paste0("ParamTesting/run_status_",data_data,".csv"))
run_res <- read.csv(file=paste0("ParamTesting/run_res_",data_data,".csv"))
run_res_LS <- read.csv(file=paste0("ParamTesting/run_res_LS_",data_data,".csv"))

## Did any runs fail
unique(run_status$status)

## Run res across all params
run_res$timeperiod <- factor(run_res$timeperiod, levels = c("Before", "Soon", "After"))
run_res <- run_res %>% mutate(timeperiod_num = case_when(
  timeperiod == "Before" ~ 1,
  timeperiod == "Soon" ~ 2,
  timeperiod == "After" ~ 3
)) 
run_res_param <- left_join(run_res, param_sets)

# Filtering parameters to use
## Filtering we want the population structure to re/establish before and after MR - pop_size + pop_growth_trend + pop_growth_R2 
pop_struct_comped <- run_res %>% 
  filter(timeperiod %in% c("Before", "After")) %>% 
  group_by(param_iteration) %>% 
  summarise(valid_pop_struct = all(between(pop_struct, 75000, 150000))) %>% 
  ungroup()

pop_size_comped <- run_res %>% 
  filter(timeperiod %in% c("Before", "After")) %>% 
  group_by(param_iteration) %>% 
  summarise(valid_pop_size = all(between(pop_size, 215000, 240000))) %>% 
  ungroup()

pop_trend_comped <- run_res %>% 
  group_by(param_iteration) %>% 
  summarise(
    valid_pop_trend = 
      all(
        (pop_growth_trend[timeperiod %in% c("Before", "After")] >= 4 & 
          #  pop_growth_trend[timeperiod %in% c("Before", "After")] <= 8) |
          # (pop_growth_trend[timeperiod %in% c("Before", "After")] <= -4 & 
             pop_growth_trend[timeperiod %in% c("Before", "After")] >= -8)
      ) #&
      #all(pop_growth_trend[timeperiod == "Soon"] > 0),
    #.groups = "drop"
  )

#####
## We want the soon and after to match empirical data
## Compare mean_MR - whether the seedlings in "After" is lower than in "Soon.
run_res_LS_comped_LS <- run_res_LS %>%
  filter(Lifestage == "Seedling") %>%    
  dplyr::select(param_iteration, timeperiod, mean_MR) %>%
  pivot_wider(names_from = timeperiod, values_from = mean_MR) %>% 
  group_by(param_iteration) %>% 
  summarise(After_lower = After < Soon)

## whether seedlings mean_MR is > subadult mean_MR

run_res_LS_comped_LS2 <- run_res_LS %>%
  ungroup() %>%
  filter(timeperiod == "Soon") %>%
  dplyr::select(param_iteration, timeperiod, Lifestage, mean_MR) %>%
  pivot_wider(names_from = Lifestage, values_from = mean_MR) %>%
  mutate(
    seedling_grt_subadult = Seedling > Subadult,
    adult_grt_subadult   = Adult > Subadult
  ) %>%
  dplyr::select (param_iteration, seedling_grt_subadult, adult_grt_subadult)



## Final parameter scores
pop_struct_comped # population age structure between valid values
pop_size_comped # population size between valid values
pop_trend_comped # population trend between valid values
run_res_LS_comped_LS ## Compare mean_MR - whether the seedlings in "After" is lower than in "Soon.
run_res_LS_comped_LS2 ## whether seedlings mean_MR is > subadult mean_MR & whether adult mean_MR is > subadult mean_MR - for 'soon'


final_list <- pop_struct_comped %>%
  left_join(pop_size_comped, by = "param_iteration") %>%
  left_join(pop_trend_comped, by = "param_iteration") %>%
  left_join(run_res_LS_comped_LS, by = "param_iteration") %>%
  left_join(run_res_LS_comped_LS2, by = "param_iteration")

final_list <- final_list %>% 
  mutate(score = rowSums(across(-param_iteration, as.numeric)))

param_sets_scored <- left_join(param_sets, final_list)

############ Visualisation
########################## Against each other
ggplot(run_res_param, aes(y=pop_struct, x=age_impact)) +
  geom_point() +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)-1))), 
             aes(y=pop_struct, x=age_impact), 
             colour="magenta", size=2, shape=1) +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)))), 
             aes(y=pop_struct, x=age_impact), 
             colour="green", size=2, shape=1) +
  geom_hline(yintercept=150000, colour="brown", linetype="dashed") +
  geom_hline(yintercept=75000, colour="brown", linetype="dashed") +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="Population structure (age)")

ggplot(run_res_param, aes(y=pop_size, x=age_impact)) +
  geom_point() +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)-1))), 
             aes(y=pop_size, x=age_impact), 
             colour="magenta", size=3, shape=1) +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)))), 
             aes(y=pop_size, x=age_impact), 
             colour="green", size=3, shape=1) +
  geom_hline(yintercept=240000, colour="brown", linetype="dashed") +
  geom_hline(yintercept=215000, colour="brown", linetype="dashed") +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="Population structure (size absolute)")

ggplot(run_res_param, aes(y=pop_growth_trend, x=MR_death_impact, colour=age_impact)) +
  geom_point() +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)-1))), 
             aes(y=pop_growth_trend, x=MR_death_impact, colour=age_impact), 
             colour="magenta", size=2, shape=1) +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)))), 
             aes(y=pop_growth_trend, x=MR_death_impact, colour=age_impact), 
             colour="green", size=2, shape=1) +
  geom_hline(yintercept=4, colour="brown", linetype="dashed") +
  geom_hline(yintercept=8, colour="brown", linetype="dashed") +
  geom_hline(yintercept=-4, colour="orange", linetype="dashed") +
  geom_hline(yintercept=-8, colour="orange", linetype="dashed") +
  geom_hline(yintercept=0, colour="purple", linetype="dashed") +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="Population structure (size trend)")

ggplot(run_res_param, aes(y=pop_growth_R2, x=dist_imp)) +
  geom_point() +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)-1))), 
             aes(y=pop_growth_R2, x=dist_imp), 
             colour="magenta", size=2, shape=1) +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)))), 
             aes(y=pop_growth_R2, x=dist_imp), 
             colour="green", size=2, shape=1) +
  facet_wrap (~timeperiod) +
  geom_hline(yintercept=0.02, colour="brown", linetype="dashed") +
  theme_bw() +
  labs(title="Population structure (size stability)")

ggplot() +
  geom_point(data=run_res_param, aes(y=mean_MR, x=MR_death_impact, colour=age_impact)) +
  geom_errorbar(data=run_res_param, aes(ymin=mean_MR-sd_MR, ymax=mean_MR+sd_MR, x=MR_death_impact, colour=age_impact)) +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)-1))), 
             aes(y=mean_MR, x=MR_death_impact, colour=age_impact), 
             colour="magenta", size=2, shape=1) +
  geom_point(data = (run_res_param %>% filter(param_iteration==(nrow(param_sets)))), 
             aes(y=mean_MR, x=MR_death_impact, colour=age_impact), 
             colour="green", size=2, shape=1) +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="MR (mean +/- sd)")


########################## Against scores

bin_cont_meanscore_plot <- function(dataframe, variable, n_bins = 10) {
  dataframe_binned <- dataframe %>% 
    mutate(variable_bin = cut(.data[[variable]], breaks = n_bins)) %>%
    group_by(variable_bin) %>%
    summarise(
      mean_score = mean(score, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  print(
    ggplot(dataframe_binned, aes(x = variable_bin, y = mean_score)) +
      geom_col() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = variable)
  )
}
variables <- c("MR_death_impact", "age_impact", "recruitment_const", "MR_age_impact", "age_recruit_impact_value", "MR_recruit_impact")

plots <- lapply(variables, function(v) bin_cont_meanscore_plot(param_sets_scored, v))
combined_plot <- patchwork::wrap_plots(plots, ncol =3 )  # 2 columns grid
combined_plot

##### Comb imp

plotly::ggplotly(ggplot(param_sets_scored, aes(x=MR_death_impact, y=recruitment_const, colour=score)) + 
  geom_point() +
  theme_bw())

plotly::plot_ly(param_sets_scored, x=~MR_death_impact, y=~age_impact, z=~score, type="scatter3d", mode="markers", marker=list(size=3, color=~param_iteration, colorscale="Viridis", opacity=0.7)) %>% plotly::layout(scene=list(xaxis=list(title="MR Death Impact"), yaxis=list(title="Age Impact"), zaxis=list(title="Score")))
plotly::plot_ly(param_sets_scored, x=~recruitment_const, y=~age_impact, z=~score, type="scatter3d", mode="markers", marker=list(size=3, color=~param_iteration, colorscale="Viridis", opacity=0.7)) %>% plotly::layout(scene=list(xaxis=list(title="Recruitment Const"), yaxis=list(title="Age Impact"), zaxis=list(title="Score")))
plotly::plot_ly(param_sets_scored, x=~recruitment_const, y=~MR_death_impact, z=~score, type="scatter3d", mode="markers", marker=list(size=3, color=~param_iteration, colorscale="Viridis", opacity=0.7)) %>% plotly::layout(scene=list(xaxis=list(title="Recruitment Const"), yaxis=list(title="MR Death Impact"), zaxis=list(title="Score")))


#### Fixed bins across params for better comparisons
bin_split_vars <- function(dataframe, variables, n_bins = 10) {
  dataframe_binned <- dataframe
  
  for (var in variables) {
    new_col <- paste0(var, "_bin")
    dataframe_binned <- dataframe_binned %>%
      mutate(!!new_col := cut(.data[[var]], breaks = n_bins))
  }
  
  return(dataframe_binned)
}

variables <- c("MR_death_impact", "age_impact", "recruitment_const", "MR_age_impact", "age_recruit_impact_value", "MR_recruit_impact")
number_bins = 5
param_sets_binned <- bin_split_vars(param_sets_scored, variables, n_bins = number_bins)


variable_bins <- c("MR_death_impact_bin", "age_impact_bin", "recruitment_const_bin", "MR_age_impact_bin", "age_recruit_impact_value_bin", "MR_recruit_impact_bin")

combo_counts <- param_sets_binned %>%
  count(across(all_of(variable_bins)), name = "n")

for (i in length(variable_bins)){
  
  # iterate through i
  for (j in 1:number_bins){
    fix_1 <- unique(param_sets_binned[[variable_bins[1]]])[j]
    fix_2 <- unique(param_sets_binned[[variable_bins[2]]])[j]
    fix_3 <- unique(param_sets_binned[[variable_bins[3]]])[j]
    fix_4 <- unique(param_sets_binned[[variable_bins[4]]])[j]
    fix_5 <- unique(param_sets_binned[[variable_bins[5]]])[j]
    fix_6 <- unique(param_sets_binned[[variable_bins[6]]])[j]
    
    param_sets_binned_filt <- param_sets_binned %>% 
      filter(.data[[ variable_bins[4] ]] == fix_4,
             .data[[ variable_bins[5] ]] == fix_5,
             .data[[ variable_bins[6] ]] == fix_6)
    
    print(plotly::plot_ly(param_sets_binned_filt, x=~MR_death_impact, y=~age_impact, z=~recruitment_const, type="scatter3d", mode="markers", marker = list(size = 3, color = ~score, colorscale = "Viridis", opacity = 0.7, showscale = TRUE)) %>% 
      plotly::layout(scene=list(xaxis=list(title="MR Death Impact"), yaxis=list(title="Age Impact"), zaxis=list(title="Recruitment Const"))) %>% 
      plotly::layout(title = paste("Running", fix_4, fix_5, fix_6)))
  }
}

bin_counts <- param_sets_binned %>%
  filter(score > 4) %>%
  pivot_longer(cols = all_of(variable_bins), 
               names_to = "variable", 
               values_to = "bin") %>% 
  group_by(variable, bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(variable, desc(count))

write.csv(bin_counts, file="ParamTesting/bin_counts.csv")
