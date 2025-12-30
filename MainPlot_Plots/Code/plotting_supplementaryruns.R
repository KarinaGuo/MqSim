## Plotting supplementary information

plot_size <- function(population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df){
  Size_plot <- ggplot() + 
    geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
    geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
    geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
    geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
    geom_point(data=live_size_df, aes(x=time, y=sum_size, colour=Run), alpha=0.3, size=0.5) + 
    stat_smooth(data=(live_size_df %>% filter(Run=="Base")), aes(x=time, y = sum_size), colour="darkgoldenrod3", linetype="dashed", linewidth = 0.5, span=0.1, method="loess") +
    stat_smooth(data=(live_size_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = sum_size), colour="steelblue2", linetype="dashed", linewidth = 0.5, span=0.1, method="loess") +
    scale_colour_manual(values=c("Base"="goldenrod", "Resistance Intervention"="deepskyblue4")) +
    ggforce::facet_zoom(xlim=c(MR_timepoint-25,MR_timepoint+275)) +
    labs(x="Timepoint", y="Live population size") +
    theme(legend.position = "none")
  
  return(Size_plot)
}

plot_MR <- function(MR_timepoint, intercept_timepoint, MR_df){
  MR_plot <- ggplot() +
    geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
    geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen", alpha=0.5) +
    geom_point(data=MR_df, aes(x=time, y = MR_mean_summ, colour=Run), alpha=0.3, size=0.5) +
    stat_smooth(data=(MR_df %>% filter(Run=="Base")), aes(x=time, y = MR_mean_summ), colour="darkgoldenrod3", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
    stat_smooth(data=(MR_df %>% filter(Run=="Resistance Intervention")), aes(x=time, y = MR_mean_summ), colour="steelblue2", linetype="dashed", linewidth = 0.5, span=0.1, se=F, method="loess") +
    scale_colour_manual(values=c("Base"="goldenrod", "Resistance Intervention"="deepskyblue4")) +
    ggforce::facet_zoom(xlim=c(MR_timepoint-25,MR_timepoint+275)) +
    labs(x="Timepoint", y="Population mean myrtle rust")
  
  return(MR_plot)
}

###########################################################

setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/")
library(tidyverse); library(patchwork)
source("configurations_fromMultRun")
theme_set(theme_bw())

############################################################
# Varying Myrtle rust pressure for restoration
## 1. MR Impact score of 0.2
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_1.txt")
MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.2_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.2_Intro1000_summ.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_1.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_1.csv") %>% mutate(Run="Resistance Intervention")

colnames(size_base)[which(colnames(size_base)=="mean_sumsize")] <- "sum_size"
colnames(MR_base)[which(colnames(MR_base)=="mean_MR_mean_summ")] <- "MR_mean_summ"
colnames(MR_base)[which(colnames(MR_base)=="mean_MR_sd_summ")] <- "MR_sd_summ"

MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, as.numeric(MR_timepoint), intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_1a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_1b.jpg", limitsize = F, width=2000, height=3000, units='px')


## 2. MR Impact score of 0.9
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_2.txt")
MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.9_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.9_Intro1000_summ.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_2.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_2.csv") %>% mutate(Run="Resistance Intervention")

colnames(MR_base)[which(colnames(MR_base) == "mean_MR_mean_summ")] = "MR_mean_summ"
colnames(MR_base)[which(colnames(MR_base) == "mean_MR_sd_summ")] = "MR_sd_summ"
colnames(size_base)[which(colnames(size_base) == "mean_sumsize")] = "sum_size"
MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_2a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_2b.jpg", limitsize = F, width=2000, height=3000, units='px')


############################################################
# Varying restoration individuals introduction time
## 3. Introduction at 1010
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_3.txt")
MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_3.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_3.csv") %>% mutate(Run="Resistance Intervention")

colnames(MR_base)[which(colnames(MR_base) == "mean_MR_mean_summ")] = "MR_mean_summ"
colnames(MR_base)[which(colnames(MR_base) == "mean_MR_sd_summ")] = "MR_sd_summ"
colnames(size_base)[which(colnames(size_base) == "mean_sumsize")] = "sum_size"
MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_3a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_3b.jpg", limitsize = F, width=2000, height=3000, units='px')

## 4. Introduction at 1100
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_4.txt")
MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_4.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_4.csv") %>% mutate(Run="Resistance Intervention")

colnames(MR_base)[which(colnames(MR_base) == "mean_MR_mean_summ")] = "MR_mean_summ"
colnames(MR_base)[which(colnames(MR_base) == "mean_MR_sd_summ")] = "MR_sd_summ"
colnames(size_base)[which(colnames(size_base) == "mean_sumsize")] = "sum_size"

MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_4a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_4b.jpg", limitsize = F, width=2000, height=3000, units='px')


############################################################
# Varying restoration individuals introduction size
## 5. Introduction with 1000 indivs
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_5.txt")
MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_5.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_5.csv") %>% mutate(Run="Resistance Intervention")

colnames(MR_base)[which(colnames(MR_base) == "mean_MR_mean_summ")] = "MR_mean_summ"
colnames(MR_base)[which(colnames(MR_base) == "mean_MR_sd_summ")] = "MR_sd_summ"
colnames(size_base)[which(colnames(size_base) == "mean_sumsize")] = "sum_size"

MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_5a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_5b.jpg", limitsize = F, width=2000, height=3000, units='px')


## 6. Introduction with 5000 indivs
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_6.txt")
MR_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
size_base <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.4_Intro1000_summ.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_6.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_6.csv") %>% mutate(Run="Resistance Intervention")


colnames(MR_base)[which(colnames(MR_base) == "mean_MR_mean_summ")] = "MR_mean_summ"
colnames(MR_base)[which(colnames(MR_base) == "mean_MR_sd_summ")] = "MR_sd_summ"
colnames(size_base)[which(colnames(size_base) == "mean_sumsize")] = "sum_size"


MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_6a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_6b.jpg", limitsize = F, width=2000, height=3000, units='px')

############################################################
# Varying population recruitment chance
## 7. Recruitment chance of 0.002
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_7.txt")
MR_base <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_7_base.csv") %>% mutate(Run="Base")
size_base <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_7_base.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_7.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_7.csv") %>% mutate(Run="Resistance Intervention")

MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_7a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_7b.jpg", limitsize = F, width=2000, height=3000, units='px')

## 8. Recruitment chance of 0.004
source("Final_Configs_for_Publ/Supplementary_Runs/Configuration_Supple_8.txt")
MR_base <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_8_base.csv") %>% mutate(Run="Base")
size_base <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_8_base.csv") %>% mutate(Run="Base")
MR_int <- read.csv("SupplPlot_SupplementaryRuns/MR_int_MR_8.csv") %>% mutate(Run="Resistance Intervention")
size_int <- read.csv("SupplPlot_SupplementaryRuns/SIZE_int_MR_8.csv") %>% mutate(Run="Resistance Intervention")

MR_df <- rbind(MR_base, MR_int); live_size_df <- rbind(size_base, size_int)

Size_plot <- plot_size (population_carrying_capacity, population_minimum_size, MR_timepoint, intercept_timepoint, live_size_df)
MR_plot <- plot_MR (MR_timepoint, intercept_timepoint, MR_df)

ggsave(Size_plot, file="MainPlot_Plots/SI plots/Size_Simulation_SI_8a.jpg", limitsize = F, width=2000, height=3000, units='px')
ggsave(MR_plot, file="MainPlot_Plots/SI plots/MR_Simulation_SI_8b.jpg", limitsize = F, width=2000, height=3000, units='px')
