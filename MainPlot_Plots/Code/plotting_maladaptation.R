library(tidyverse); library(patchwork)
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/")
source("MainPlot_Maladaptation/Rerun_mult_iter/configurations_maladaptation")

# Plot both runs on top
Size_MaladaptationRunPop1 <- read.csv("MainPlot_Maladaptation/Rerun_mult_iter/SIZE_MaladaptationRunPop1_summ.csv"); Size_MaladaptationRunPop1$Run <- "MaladaptationRunPop1"
Size_MaladaptationRunPop2 <- read.csv("MainPlot_Maladaptation/Rerun_mult_iter/SIZE_MaladaptationRunPop2_summ.csv"); Size_MaladaptationRunPop2$Run <- "MaladaptationRunPop2"
Size_SoloRunPop1 <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.2_Intro1000_summ.csv"); Size_SoloRunPop1$Run <- "SoloRunPop1"
Size_SoloRunPop2 <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_0.9_Intro1000_summ.csv"); Size_SoloRunPop2$Run <- "SoloRunPop2"

Size_Pop1_res <- data.frame(rbind(Size_MaladaptationRunPop1, Size_SoloRunPop1))
Size_Pop2_res <- data.frame(rbind(Size_MaladaptationRunPop2, Size_SoloRunPop2))


Size_Pop1_plot <- ggplot() + 
  geom_point(data=Size_Pop1_res, aes(x=time,  y=mean_sumsize, colour=Run), alpha = 0.3, size=0.2) +
  stat_smooth(data=Size_Pop1_res, aes(x=time, y = mean_sumsize, group=Run, colour=Run), linewidth = 1, linetype="dashed", method="loess", span=0.1) +
  labs(title="Population 1 - MR=0.2 (weak)", y="Population size", x="Timepoint") +
  scale_colour_manual(values=c("MaladaptationRunPop1"="seagreen", "SoloRunPop1"="powderblue")) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  xlim(c(500,2000)) +
  theme_bw()

Size_Pop2_plot <- ggplot() + 
  geom_point(data=Size_Pop2_res, aes(x=time,  y=mean_sumsize, colour=Run), alpha = 0.3, size=0.2) +
  stat_smooth(data=Size_Pop2_res, aes(x=time, y = mean_sumsize, group=Run, colour=Run), linewidth = 1, linetype="dashed", method="loess", span=0.01) +
  labs(title="Population 2 - MR=0.9 (strong)", y="Population size", x="Timepoint") +
  scale_colour_manual(values=c("MaladaptationRunPop2"="tomato3", "SoloRunPop2"="lightsalmon")) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  ggforce::facet_zoom(xlim=c(1000,1300)) +
  xlim(c(500,2000)) +
  theme_bw()  

Size_plot <- Size_Pop1_plot | Size_Pop2_plot
Size_plot

##
MR_MaladaptationRunPop1 <- read.csv("MainPlot_Maladaptation/Rerun_mult_iter/MR_MaladaptationRunPop1_summ.csv"); MR_MaladaptationRunPop1$Run <- "MaladaptationRunPop1"
MR_MaladaptationRunPop2 <- read.csv("MainPlot_Maladaptation/Rerun_mult_iter/MR_MaladaptationRunPop2_summ.csv"); MR_MaladaptationRunPop2$Run <- "MaladaptationRunPop2"
MR_SoloRunPop1 <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.2_Intro1000_summ.csv"); MR_SoloRunPop1$Run <- "SoloRunPop1"; MR_SoloRunPop1$mean_maladaptation_time <- 0
MR_SoloRunPop2 <- read.csv("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_0.9_Intro1000_summ.csv"); MR_SoloRunPop2$Run <- "SoloRunPop2"; MR_SoloRunPop2$mean_maladaptation_time <- 0

MR_Pop1_res <- data.frame(rbind(MR_MaladaptationRunPop1, MR_SoloRunPop1))
MR_Pop2_res <- data.frame(rbind(MR_MaladaptationRunPop2, MR_SoloRunPop2))

MR_Pop1_plot <- ggplot() + 
  geom_point(data=MR_Pop1_res, aes(x=time,  y=mean_MR_mean_summ, shape=Run), colour="grey80", alpha = 0.3, size=0.2, show.legend = FALSE) +
  geom_point(data=(MR_Pop1_res %>% filter(mean_maladaptation_time>=0.5)), aes(x=time,  y=mean_MR_mean_summ,  colour = "Maladaptation event"), shape=17, alpha = 0.3, size=1) +
  stat_smooth(data=MR_Pop1_res, aes(x=time, y = mean_MR_mean_summ, group=Run, colour=Run), linewidth = 1, linetype="dashed", span=10, show.legend = TRUE) +
  ylim(c(0,1)) +
  labs(title="Population 1 - MR=0.2 (weak)", y="Population mean myrtle rust", x="Timepoint") +
  scale_colour_manual(values=c("MaladaptationRunPop1"="seagreen", "SoloRunPop1"="powderblue", "Maladaptation event" ="darkolivegreen"),
                      name = "",
                      labels = c(
                        "Maladaptation run",
                        "Solo run",
                        "Maladaptation event"
                      )) +
  guides(shape = "none") + 
  xlim(c(500,2000)) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  theme_bw()

MR_Pop1_plot


MR_Pop2_plot <- ggplot() + 
  geom_point(data=MR_Pop1_res, aes(x=time,  y=mean_MR_mean_summ, shape=Run), colour="grey80", alpha = 0.3, size=0.2, show.legend = FALSE) +
  #geom_point(data=(MR_Pop1_res %>% filter(mean_maladaptation_time>=0.5)), aes(x=time,  y=mean_MR_mean_summ,  colour = "Maladaptation event"), shape=17, alpha = 0.3, size=1) +
  stat_smooth(data=MR_Pop1_res, aes(x=time, y = mean_MR_mean_summ, group=Run, colour=Run), linewidth = 1, linetype="dashed", span=10, show.legend = TRUE) +
  ylim(c(0,1)) +
  labs(title="Population 1 - MR=0.2 (weak)", y="Population mean myrtle rust", x="Timepoint") +
  scale_colour_manual(values=c("MaladaptationRunPop1"="tomato3", "SoloRunPop1"="lightsalmon", "Maladaptation event" ="red3"),
                      name = "",
                      labels = c(
                        "Maladaptation run",
                        "Solo run",
                        "Maladaptation event"
                      )) +
 # xlim(c(500,2000)) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate", alpha=0.5) +
  guides(shape = "none") +
  theme_bw()

MR_Pop2_plot 

MR_plot <- MR_Pop1_plot | MR_Pop2_plot

ggsave(Size_Pop1_plot, file="MainPlot_Plots/Maladaptation_size_plot_Pop1.jpg", limitsize = F, width=2000, height=2500, units='px')
ggsave(Size_Pop2_plot, file="MainPlot_Plots/Maladaptation_size_plot_Pop2.jpg", limitsize = F, width=2000, height=2500, units='px')
ggsave(MR_Pop1_plot, file="MainPlot_Plots/Maladaptation_MR_plot_Pop1.jpg", limitsize = F, width=2000, height=2500, units='px')
ggsave(MR_Pop2_plot, file="MainPlot_Plots/Maladaptation_MR_plot_Pop2.jpg", limitsize = F, width=2000, height=2500, units='px')
