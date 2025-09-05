## Analysis of parameter runs
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")
library(tidyverse)


# Read in
data_data <- "2025-09-05"

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
  summarise(valid_pop_struct = all(between(pop_struct, 100000, 200000))) %>% 
  ungroup()

pop_size_comped <- run_res %>% 
  filter(timeperiod %in% c("Before", "After")) %>% 
  group_by(param_iteration) %>% 
  summarise(valid_pop_size = all(between(pop_size, 235000, 255000))) %>% 
  ungroup()

pop_trend_comped <- run_res %>% 
  group_by(param_iteration) %>% 
  summarise(
    valid_pop_trend = 
      all(pop_growth_trend[timeperiod %in% c("Before", "After")] %in% -3:3) &
      all(pop_growth_trend[timeperiod == "Soon"] > 0),
    .groups = "drop"
  )

#####
## We want the soon and after to match empirical data
## Compare mean_MR - whether the seedlings in "After" is lower than in "Soon.
run_res_LS_comped_LS <- run_res_LS %>%
  filter(Lifestage == "Seedling") %>%    
  select(param_iteration, timeperiod, mean_MR) %>%
  pivot_wider(names_from = timeperiod, values_from = mean_MR) %>% 
  group_by(param_iteration) %>% 
  summarise(After_lower = After < Soon)

## whether seedlings mean_MR is > subadult mean_MR

run_res_LS_comped_LS2 <- run_res_LS %>%
  ungroup() %>%
  filter(timeperiod == "Soon") %>%
  select(param_iteration, timeperiod, Lifestage, mean_MR) %>%
  pivot_wider(names_from = Lifestage, values_from = mean_MR) %>%
  mutate(
    seedling_grt_subadult = Seedling > Subadult,
    adult_grt_subadult   = Adult > Subadult
  ) %>%
  select (param_iteration, seedling_grt_subadult, adult_grt_subadult)



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
  geom_hline(yintercept=200000, colour="brown", linetype="dashed") +
  geom_hline(yintercept=100000, colour="brown", linetype="dashed") +
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
  geom_hline(yintercept=255000, colour="brown", linetype="dashed") +
  geom_hline(yintercept=235000, colour="brown", linetype="dashed") +
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
  geom_hline(yintercept=3, colour="brown", linetype="dashed") +
  geom_hline(yintercept=-3, colour="brown", linetype="dashed") +
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

run_res_param_before <- run_res_param %>% filter(timeperiod=="Soon")
plotly::plot_ly(x=run_res_param_before$MR_death_impact, y=run_res_param_before$age_impact, z=run_res_param_before$pop_size, type="scatter3d", mode="markers", size = 1)

########################## Against scores

bin_cont_meanscore_plot <- function(dataframe, variable) {
  dataframe_binned <- dataframe %>% 
    mutate(variable_bin = cut({{variable}}, breaks = 10)) %>%
    group_by(variable_bin) %>%
    summarise(mean_score = mean(score, na.rm = TRUE), n = n(), .groups = "drop" )
  
  print(ggplot(dataframe_binned, aes(x = variable_bin, y = mean_score)) +
         geom_col() +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         labs(x=deparse(substitute(variable))))
  }


### MR death impact - bin then plot
g
