## Plotting ensemble runs

# Read files
setwd("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation")
death_df <- read.csv("Run_results/ensemble_death_df.csv")
live_size_df <- read.csv("Run_results/ensemble_live_size_df.csv")
age_df <- read.csv("Run_results/ensemble_age_df.csv")
MR_df <- read.csv("Run_results/ensemble_MR_df.csv")
pop_timepoints <- read.csv("Run_results/ensemble_pop_timepoints.csv")
AF_comparison <- read.csv("Run_results/ensemble_AF_comparison.csv")
source("Configuration_10.txt")

library(patchwork)


################### Analyses for post-run plots
## Post-run plots

mean_MR_time_death <- death_df %>% group_by(time, run_id) %>% summarise(mean_MR=mean(MR), sd_MR=sd(MR))

plot_livesize <- ggplot() + 
  #geom_point(data=live_size_df, aes(x=time, y=sum_size, colour = run_id)) + 
  stat_smooth(data=live_size_df, aes(x=time, y = sum_size, group = as.character(run_id), colour = as.character(run_id)), linewidth = 0.85, se=F, linetype="dashed", span=10) +
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_vline(xintercept=915, linewidth = 0.75, linetype="dashed", colour="red") +
  ggforce::facet_zoom(xlim=c(1000,1050)) +
  labs(title="Live population size") 

plot_deadMR   <- ggplot(mean_MR_time_death |> filter (time >900), aes(x=time, y=mean_MR, group = as.character(run_id), colour = as.character(run_id))) + 
  #geom_point() + 
  labs(title="Death MR") +
  stat_smooth(linewidth = 0.75, linetype="dashed", se=F, span=10) 

plot_deadMR 
plot_livesize


################# LS shifts
### Exported time point analyses
pop_timepoints_orig <- pop_timepoints
pop_timepoints <- pop_timepoints[-c(1)]

pop_timepoints$time <- pop_timepoints$time-1; final_df <- pop_timepoints

final_df <- final_df %>% 
  mutate(Lifestage = case_when(
    age <= 2 ~ "Seedling",
    age > 2 & age <= 7 ~ "Subadult",
    age > 7 ~ "Adult"
  ))

final_df$Lifestage <- factor(final_df$Lifestage, levels = c("Seedling", "Subadult", "Adult")) # Reorder Sev factor levels

final_df_summ <- final_df %>% 
  group_by(time, run_id) %>% 
  summarise(MR_mean_summ=mean(MR, na.rm=TRUE), MR_sd_summ=sd(MR, na.rm=TRUE), pop_size=n())

unique(final_df_summ$time)

plot_liveMR <- ggplot() +
  geom_point(data=final_df_summ, aes(x=time, y = MR_mean_summ)) +
  geom_errorbar(data=final_df_summ, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ)) + 
  stat_smooth(data=final_df_summ, aes(x=time, y = MR_mean_summ), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  labs(title="Live MR")
plot_liveMR

plot_liveage <- ggplot() +
  geom_point(data=final_df_summ, aes(x=time, y = pop_size)) +
  stat_smooth(data=final_df_summ, aes(x=time, y = pop_size), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  labs(title="Live pop size", y= "Myrtle rust", x="Life stage")
plot_liveage


LS_MR_pre <- ggplot() + 
  geom_boxplot(data = final_df %>% dplyr::filter(time > 990, time < 1000), aes(x=Lifestage, y=MR)) +  
  #geom_point(data = final_df %>% dplyr::filter(time > 2010 & time < 2020), aes(x=age, y=MR), size = 0.05) + 
  theme_bw() +
  labs(title=paste0("Impact ",MR_death_impact,"; time pre"),  y= "Myrtle rust", x="Life stage")+
  scale_x_discrete(limits = c("Adult", "Subadult", "Seedling"))
LS_MR_pre

LS_MR <- ggplot() + 
  geom_boxplot(data = final_df %>% dplyr::filter(time > 1010 & time < 1020), aes(x=Lifestage, y=MR)) +  
  #geom_point(data = final_df %>% dplyr::filter(time > 2010 & time < 2020), aes(x=age, y=MR), size = 0.05) + 
  theme_bw() +
  labs(title=paste0("Impact ",MR_death_impact,"; time +10-20"),  y= "Myrtle rust", x="Life stage") +
  scale_x_discrete(limits = c("Adult", "Subadult", "Seedling"))
LS_MR


LS_MR_2 <- ggplot() + 
  geom_boxplot(data = final_df %>% dplyr::filter(time > 1030 & time < 1050), aes(x=Lifestage, y=MR)) +  
  #geom_point(data = final_df %>% dplyr::filter(time > 2010 & time < 2020), aes(x=age, y=MR), size = 0.05) + 
  theme_bw() +
  labs(title=paste0("Impact ",MR_death_impact,"; time +30-50"),  y= "Myrtle rust", x="Life stage")+
  scale_x_discrete(limits = c("Adult", "Subadult", "Seedling"))
LS_MR_2

LS_MR_pre + LS_MR + LS_MR_2 + plot_layout(axes = "collect") & 
  scale_y_continuous(limits = c(min((final_df |> dplyr::filter(time > 990 & time < 1050))$MR), max((final_df |> dplyr::filter(time > 990 & time < 1050))$MR)))
