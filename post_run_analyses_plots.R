#source("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/data_sim_5.R")
load("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Run_results/20082026_2_GAPIT_testparam.Rdata")

library(tidyverse)
theme_set(theme_bw())

################### Analyses for post-run plots
## Post-run plots

mean_MR_time_death <- death_df %>% group_by(time) %>% summarise(mean_MR=mean(MR), sd_MR=sd(MR))

plot_livesize <- ggplot() + 
  geom_point(data=live_size_df, aes(x=time, y=sum_size)) + 
  stat_smooth(data=live_size_df, aes(x=time, y = sum_size), linewidth = 0.85, linetype="dashed", colour="grey40", span=10) +
  geom_hline(yintercept=population_carrying_capacity, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_hline(yintercept=population_minimum_size, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_vline(xintercept=915, linewidth = 0.75, linetype="dashed", colour="red") +
  #ggforce::facet_zoom(xlim=c(900,920)) +
  labs(title="Live population size") 
if(intercept_togg){plot_livesize <- plot_livesize + geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen")}

plot_liveage  <- ggplot() + 
  geom_point(data=age_df, aes(x=time, y=age_mean_summ)) +
  geom_errorbar(data=age_df, aes(x=time, ymax = age_mean_summ + age_sd_summ, ymin = age_mean_summ - age_sd_summ)) + 
  labs(title="Live age") 

plot_deadMR   <- ggplot(mean_MR_time_death |> filter (time >990), aes(x=time, y=mean_MR)) + geom_point() + 
  labs(title="Death MR") +
  stat_smooth(linewidth = 0.75, linetype="dashed", colour="grey40", span=10) 
plot_liveMR   <- ggplot() +
  geom_point(data=MR_df, aes(x=time, y = MR_mean_summ)) +
  geom_errorbar(data=MR_df, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ)) + 
  stat_smooth(data=MR_df, aes(x=time, y = MR_mean_summ), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  geom_vline(xintercept=915, linewidth = 0.75, linetype="dashed", colour="red") +
  #ggforce::facet_zoom(xlim=c(900,920)) +
  labs(title="Live MR")
if(intercept_togg){plot_liveMR <- plot_liveMR + geom_vline(xintercept=intercept_timepoint, linewidth = 0.75, linetype="dashed", colour="forestgreen")}

library(patchwork)
plot_deadMR / plot_liveMR + plot_layout(heights = c(1,3))
plot_livesize / plot_liveage + plot_layout(heights = c(3,1))

################################################################
################## Sanity check - Calculate AF for empirical year
library(dartR)

TP_before_AF <- AF_timepoints[[2]] 
TP_before_curr <- pop_timepoints[[2]] 
young_indvs <- TP_before_curr$age < 5

# check time
unique(TP_before_curr$time) 

TP_before_AF_young <- TP_before_AF[young_indvs]
TP_before_AF_downsampled <- TP_before_AF_young[sample(length(TP_before_AF_young), 124)] # Number of seedlings sampled in HS experiment for Historical dataset

## Empirical current AF

SNPs_tested <- effect_size$V1
SNPs_effsize <- effect_size$V2

SNP_AF_empirical <- read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Data_AlleleFrequency/SNP_AF.csv") 
SNP_AF_empirical <- SNP_AF_empirical%>% 
  filter(locus %in% SNPs_tested)


## Simulation AF
total_alt_alleles <- Reduce(`+`, TP_before_AF_downsampled) # Sum the alternate alleles across all individuals for every SNP
total_alleles <- 2 * length(TP_before_AF_downsampled) # Calculate the total number of alleles in the population (2 alleles per individual)
allele_frequencies_sim <- total_alt_alleles / total_alleles # Calculate the frequencies

## check order
SNPs_tested_ord <- match(SNP_AF_empirical$locus, SNPs_tested)
identical(SNPs_tested[SNPs_tested_ord], as.character(SNP_AF_empirical$locus))

SNPs_effsize_reord <- SNPs_effsize[SNPs_tested_ord]

AF_comparison <- data.frame(locID = SNP_AF_empirical$locus, empirical_AF = SNP_AF_empirical$frequency, sim_AF = allele_frequencies_sim, eff_size = SNPs_effsize_reord) 
AF_comparison <- AF_comparison %>% 
  mutate(AF_diff = empirical_AF - sim_AF,
         SNP_Type = str_extract(AF_comparison$locID, "[^_]+$"))


ggplot(AF_comparison, aes(x=empirical_AF, y=sim_AF, colour = eff_size)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ggtitle(paste("AF comparisons at time", unique(TP_before_curr$time))) +
  facet_wrap(~SNP_Type)

summary(lm (sim_AF ~ empirical_AF, data = AF_comparison))

################################################################################################################
################## Calculate AF for +15 years after MR introduction

# C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Functions_PostRunAnalyses/Bootstrapped_AF_SNPplot.R