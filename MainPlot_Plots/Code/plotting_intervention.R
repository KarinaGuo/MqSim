## Plotting intervention difference
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/")
library(tidyverse); library(patchwork)
source("configurations_fromMultRun")
theme_set(theme_bw())

MR_base <- read.csv("MR_tests/MR_base_0.4_Intro1000.csv") %>% mutate(Run="Base")
size_base <- read.csv("MR_tests/SIZE_base_0.4_Intro1000.csv") %>% mutate(Run="Base")
MR_int <- read.csv("Intervention/MR_int_MR_0.4_bad_Intro1000.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("Intervention/SIZE_int_MR_0.4_bad_Intro1000.csv") %>% mutate(Run="Resistance Intervention")

MR_df <- rbind(MR_base, MR_int)
live_size_df <- rbind(size_base, size_int)

Size_plot <- ggplot() + 
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
  
  geom_point(data=live_size_df, aes(x=time, y=sum_size, colour=Run), alpha=0.3, size=0.5) + 
  stat_smooth(data=(live_size_df %>% filter(Run=="Base")), aes(x=time, y = sum_size), colour="darkgoldenrod3", linetype="dashed", linewidth = 0.5, span=0.1, method="loess") +
  stat_smooth(data=(live_size_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = sum_size), colour="steelblue2", linetype="dashed", linewidth = 0.5, span=0.1, method="loess") +
  scale_colour_manual(values=c("Base"="goldenrod", "Resistance Intervention"="deepskyblue4")) +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  labs(x="Timepoint", y="Live population size") +
  theme(legend.position = "none")


MR_plot <- ggplot() +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
  geom_point(data=MR_df, aes(x=time, y = MR_mean_summ, colour=Run), alpha=0.3, size=0.5) +
  stat_smooth(data=(MR_df %>% filter(Run=="Base")), aes(x=time, y = MR_mean_summ), colour="darkgoldenrod3", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  stat_smooth(data=(MR_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = MR_mean_summ), colour="steelblue2", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  scale_colour_manual(values=c("Base"="goldenrod", "Resistance Intervention"="deepskyblue4")) +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  labs(x="Timepoint", y="Population mean myrtle rust")

Size_plot + MR_plot + plot_layout(guides = "collect")
#ggsave(Size_plot, file="Plots/Intervention_lowMRInt_Size_plot.jpg", limitsize = F, width=2000, height=3000, units='px')
#ggsave(MR_plot, file="Plots/Intervention_lowMRInt_MR_plot.jpg", limitsize = F, width=2000, height=3000, units='px')

### At x windows, calculate average for each run
MR_df_sort <- MR_df[order(MR_df$time), ]
MR_df_sort$wind <- ceiling(seq_len(nrow(MR_df_sort)) / 40) # 2 rows per time

MR_wind_mean <- MR_df_sort %>%
  group_by(Run, wind) %>%
  filter(wind>10) %>% # remove stabilisation period
  summarise(mean_wind_MR = mean(MR_mean_summ), mean_sd_MR = mean(MR_sd_summ)) %>% 
  ungroup()

live_size_df_sort <- live_size_df[order(live_size_df$time), ]
live_size_df_sort$wind <- ceiling(seq_len(nrow(live_size_df_sort)) / 40) # 2 rows per time

live_size_wind_mean <- live_size_df_sort %>%
  group_by(Run, wind) %>%
  filter(wind>10) %>% # remove stabilisation period
  summarise(mean_wind_size = mean(sum_size)) %>% 
  ungroup()

plot_mean_size <- ggplot(live_size_wind_mean, aes(x = Run, y = mean_wind_size)) +
  geom_boxplot() 

plot_mean_MR <- ggplot(MR_wind_mean, aes(x = Run, y = mean_wind_MR)) +
  geom_boxplot()

plot_mean_size + plot_mean_MR + plot_annotation(title="Removed window 10, window size = 20 tp")

####### Minimum population drop post-MR introduction:
live_size_wind_mean %>% 
  group_by(Run) %>% 
  summarise(min_pop_size = min(mean_wind_size))

###### GAM test
#https://stats.stackexchange.com/questions/576499/how-to-compare-two-time-series-with-a-gam
library(mgcv)

live_size_df$Run <- as.factor(live_size_df$Run)
live_size_df_trajectory <- live_size_df %>% 
  filter (time %in% seq(from=1, to=1500))

m1 <- gam(sum_size ~ s(time), data=live_size_df, method="REML")   # one smooth across all runs
m2 <- gam(sum_size ~ s(time, Run, bs="fs"), data=live_size_df, method="REML")   # separate smooths
anova(m1, m2, test="Chisq")

library(mgcv)
library(gratia)

# Fit with by = Run coding (needed for difference smooths)
fit_gam_by <- gam(sum_size ~ Run + s(time, by = Run, k = 20),
                  data = live_size_df,
                  method = "REML")

diff_smooth <- difference_smooths(fit_gam_by, smooth = "s(time)")

draw(diff_smooth) +
  ggplot2::labs(title = "Difference between Run smooths",
                y = "Difference (Run B - Run A)")


#############################################################################

## Plotting intervention difference - ensemble N=5000 

setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/")
library(tidyverse); library(patchwork)
source("MainPlot_Intervention/Rerun_mult_iter/configurations_int_esrun")
theme_set(theme_bw())

MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")

MR_int_low <- read.csv("MainPlot_Intervention/Rerun_mult_iter/MR_int_MR_0.4_Intro1000_summ.csv") %>% mutate(Run=paste0("Resistance Intervention")) %>%
  filter (intercept_indiv_original == 2500, intercept_MR_mean == 0.1) %>% 
  select (-c(intercept_indiv_original, intercept_MR_mean))
size_int_low <- read.csv("MainPlot_Intervention/Rerun_mult_iter/SIZE_int_MR_0.4_Intro1000_summ.csv") %>% mutate(Run=paste0("Resistance Intervention")) %>%
  filter (intercept_indiv_original == 2500, intercept_MR_mean == 0.1) %>% 
  select (-c(intercept_indiv_original, intercept_MR_mean))

MR_int_high <- read.csv("MainPlot_Intervention/Rerun_mult_iter/MR_int_MR_0.4_Intro1000_summ.csv") %>% mutate(Run=paste0("Resistance Intervention")) %>%
  filter (intercept_indiv_original == 2500, intercept_MR_mean == 0.9) %>% 
  select (-c(intercept_indiv_original, intercept_MR_mean))
size_int_high <- read.csv("MainPlot_Intervention/Rerun_mult_iter/SIZE_int_MR_0.4_Intro1000_summ.csv") %>% mutate(Run=paste0("Resistance Intervention")) %>%
  filter (intercept_indiv_original == 2500, intercept_MR_mean == 0.9) %>% 
  select (-c(intercept_indiv_original, intercept_MR_mean))

MR_df <- rbind(MR_base, MR_int_low)
live_size_df <- rbind(size_base, size_int_low)

Size_plot <- ggplot() + 
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
  
  geom_point(data=live_size_df, aes(x=time, y=mean_sumsize, colour=Run), alpha=0.3, size=0.5) + 
  #stat_smooth(data=(live_size_df %>% filter(Run=="Base")), aes(x=time, y = mean_sumsize), colour="darkgoldenrod3", linetype="dashed", linewidth = 0.5, span=0.1, method="loess") +
  #stat_smooth(data=(live_size_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = mean_sumsize), colour="steelblue2", linetype="dashed", linewidth = 0.5, span=0.1, method="loess") +
  scale_colour_manual(values=c("Base"="goldenrod", "Resistance Intervention"="deepskyblue4")) +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  xlim(c(500,2000)) +
  labs(x="Timepoint", y="Live population size") +
  theme(legend.position = "none")

Size_plot

MR_plot <- ggplot() +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
  geom_point(data=MR_df, aes(x=time, y = mean_MR_mean_summ, colour=Run), alpha=0.3, size=0.5) +
  stat_smooth(data=(MR_df %>% filter(Run=="Base")), aes(x=time, y = mean_MR_mean_summ), colour="darkgoldenrod3", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  stat_smooth(data=(MR_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = mean_MR_mean_summ), colour="steelblue2", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  scale_colour_manual(values=c("Base"="goldenrod", "Resistance Intervention"="deepskyblue4")) +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  xlim(c(500,2000)) +
  labs(x="Timepoint", y="Population mean myrtle rust")

MR_plot


### At x windows, calculate average for each run
MR_df_sort <- MR_df[order(MR_df$time), ]
MR_df_sort$wind <- ceiling(seq_len(nrow(MR_df_sort)) / 40) # 2 rows per time

MR_wind_mean <- MR_df_sort %>%
  group_by(Run, wind) %>%
  filter(wind>10) %>% # remove stabilisation period
  summarise(mean_wind_MR = mean(mean_MR_mean_summ), mean_sd_MR = mean(mean_MR_sd_summ)) %>% 
  ungroup()

live_size_df_sort <- live_size_df[order(live_size_df$time), ]
live_size_df_sort$wind <- ceiling(seq_len(nrow(live_size_df_sort)) / 40) # 2 rows per time

live_size_wind_mean <- live_size_df_sort %>%
  group_by(Run, wind) %>%
  filter(wind>10) %>% # remove stabilisation period
  summarise(mean_wind_size = mean(mean_sumsize)) %>% 
  ungroup()

plot_mean_size <- ggplot(live_size_wind_mean, aes(x = Run, y = mean_wind_size)) +
  geom_boxplot() 

plot_mean_MR <- ggplot(MR_wind_mean, aes(x = Run, y = mean_wind_MR)) +
  geom_boxplot()

plot_mean_size + plot_mean_MR + plot_annotation(title="Removed window 10, window size = 20 tp")

####### Minimum population drop post-MR introduction:
live_size_wind_mean %>% 
  group_by(Run) %>% 
  summarise(min_pop_size = min(mean_wind_size))

###### GAM test
#https://stats.stackexchange.com/questions/576499/how-to-compare-two-time-series-with-a-gam
library(mgcv)

live_size_df$Run <- as.factor(live_size_df$Run)
live_size_df_trajectory <- live_size_df %>% 
  filter (time %in% seq(from=1, to=1500))

m1 <- gam(mean_sumsize ~ s(time), data=live_size_df, method="REML")   # one smooth across all runs
m2 <- gam(mean_sumsize ~ s(time, Run, bs="fs"), data=live_size_df, method="REML")   # separate smooths
anova(m1, m2, test="Chisq")

library(mgcv)
library(gratia)

# Fit with by = Run coding (needed for difference smooths)
fit_gam_by <- gam(mean_sumsize ~ Run + s(time, by = Run, k = 20),
                  data = live_size_df,
                  method = "REML")

diff_smooth <- difference_smooths(fit_gam_by, smooth = "s(time)")

inter_diff_plot <- draw(diff_smooth) +
  ggplot2::labs(title = "Difference between Run smooths",
                y = "Difference (Base Run - Intervention Run)", x="Timepoint")

ggsave(Size_plot, file="MainPlot_Plots/Intervention_lowMRInt_Size_plot.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/Intervention_lowMRInt_MR_plot.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(inter_diff_plot, file="MainPlot_Plots/Intervention_lowMRInt_difference_plot.jpg", limitsize = F, width=2000, height=2000, units='px')
