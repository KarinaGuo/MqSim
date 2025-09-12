## Plotting multiple MR impacts for same run
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")
library(tidyverse); library(patchwork)
source("configurations_fromMultRun")
theme_set(theme_bw())

MR_base_1 <- read.csv("MR_tests/MR_base_0.4_Intro1000.csv") %>% mutate(Run="MR_0.4")
size_base_1 <- read.csv("MR_tests/SIZE_base_0.4_Intro1000.csv") %>% mutate(Run="MR_0.4")

MR_base_2 <- read.csv("MR_tests/MR_base_0.2_Intro1000.csv") %>% mutate(Run="MR_0.2")
size_base_2 <- read.csv("MR_tests/SIZE_base_0.2_Intro1000.csv") %>% mutate(Run="MR_0.2")

MR_base_3 <- read.csv("MR_tests/MR_base_0.9_Intro1000.csv") %>% mutate(Run="MR_0.9")
size_base_3 <- read.csv("MR_tests/SIZE_base_0.9_Intro1000.csv") %>% mutate(Run="MR_0.9")

MR_df <- rbind(MR_base_1, MR_base_2, MR_base_3)
live_size_df <- rbind(size_base_1, size_base_2, size_base_3)

Size_plot <- ggplot() + 
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_point(data=live_size_df, aes(x=time, y=sum_size, colour=Run), alpha=0.3, size=0.2) + 
  stat_smooth(data=live_size_df, aes(x=time, y=sum_size, colour=Run), linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") + 
  labs(x="Timepoint", y="Population size", title="Live population size") +
  scale_colour_manual(values=c("MR_0.2"="darkolivegreen4", "MR_0.4"="darkgoldenrod2", "MR_0.9"="coral1")) +
  ggforce::facet_zoom(xlim=c(1000,1300)) 

  
  MR_plot <- ggplot() +
    geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
    geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
    geom_point(data=MR_df, aes(x=time, y = MR_mean_summ, colour=Run), alpha=0.3, size=0.2) +
    stat_smooth(data=MR_df, aes(x=time, y=MR_mean_summ, colour=Run), linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") + 
    ggforce::facet_zoom(xlim=c(1000,1300)) +
    scale_colour_manual(values=c("MR_0.2"="darkolivegreen4", "MR_0.4"="darkgoldenrod2", "MR_0.9"="coral1")) +
    labs(x="Timepoint", y="Population mean myrtle rust", title="Live myrtle rust score")
  
plot <- Size_plot + MR_plot + plot_layout(guides = "collect")

ggsave(Size_plot, file="Plots/Multiple_MR_Size_plot.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="Plots/Multiple_MR_MR_plot.jpg", limitsize = F, width=2000, height=3000, units='px')

