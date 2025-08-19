## Plotting intervention difference
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")
theme_set(theme_bw())
library(tidyverse)

MR_base <- read.csv("Intercept/MR_base_0.2_Intro1000.csv") %>% mutate(Run="Base")
size_base <- read.csv("Intercept/SIZE_base_0.2_Intro1000.csv") %>% mutate(Run="Base")
MR_int <- read.csv("Intercept/MR_int_MR_0.2_Intro1000.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("Intercept/SIZE_int_MR_0.2_Intro1000.csv") %>% mutate(Run="Resistance Intervention")

MR_df <- rbind(MR_base, MR_int)
live_size_df <- rbind(size_base, size_int)

Size_plot <- ggplot() + 
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
  geom_point(data=live_size_df, aes(x=time, y=sum_size, colour=Run), alpha=0.3, size=0.5) + 
  geom_point(data=(live_size_df %>% filter(time %in% seq(from=intercept_timepoint-10, to=intercept_timepoint+10))), aes(x=time, y=sum_size, colour=Run), alpha=1, size=2, shape=17) + 
  stat_smooth(data=(live_size_df %>% filter(Run=="Base")), aes(x=time, y = sum_size), colour="red", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  stat_smooth(data=(live_size_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = sum_size), colour="blue", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  ggforce::facet_zoom(xlim=c(1000,1300)) 
  #ggforce::facet_zoom(xlim=c(1000,1100)) +


MR_plot <- ggplot() +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
  geom_point(data=MR_df, aes(x=time, y = MR_mean_summ, colour=Run), alpha=0.3, size=0.5) +
  geom_point(data=(MR_df %>% filter(time %in% seq(from=intercept_timepoint-1, to=intercept_timepoint+1))), aes(x=time, y = MR_mean_summ, colour=Run), alpha=1, size=2, shape=17) +
  stat_smooth(data=(MR_df %>% filter(Run=="Base")), aes(x=time, y = MR_mean_summ), colour="red", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  stat_smooth(data=(MR_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = MR_mean_summ), colour="blue", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  #ggforce::facet_zoom(xlim=c(1000,1100)) +
  labs(title="Live MR")

Size_plot + MR_plot + plot_layout(guides = "collect")

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

