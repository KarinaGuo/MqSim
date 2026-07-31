#source("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/data_sim_5.R")
load("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Run_results/07072026_GAPIT_failed.RData")

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

plot_deadMR   <- ggplot(mean_MR_time_death, aes(x=time, y=mean_MR)) + geom_point() + labs(title="Death MR")
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
unique(TP_before_curr$time) # should be 1016

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
library(dartR)

TP_after_AF <- AF_timepoints[[19]]
TP_after_curr <- pop_timepoints[[19]] 
young_indvs <- TP_after_curr$age < 5

  # check time
unique(TP_after_curr$time) # should be 1016

TP_after_AF_young <- TP_after_AF[young_indvs]
TP_after_AF_downsampled <- TP_after_AF_young[sample(length(TP_after_AF_young), 130)] # Number of seedlings sampled in HS experiment for Contemporary dataset

## Empirical
dartgl_1row <- gl.read.dart(filename="~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/DMela2510229_merge_DMela2611752_1row.csv", nas = "-", lastmetric = "RatioAvgCountRefAvgCountSnp", ind.metafile="~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/ind_meta.csv")

empirical_postMR_AF <- gl.keep.ind(dartgl_1row, dartgl_1row@ind.names[grepl("Contemporary", dartgl_1row@pop)])
empirical_postMR_AF <- gl.keep.loc(empirical_postMR_AF, loc.list = SNPs_tested)
SNP_AF_empirical <- gl.allele.freq(empirical_postMR_AF, by="loc", verbose = 5)


total_alt_alleles <- Reduce(`+`, TP_after_AF_downsampled) # Sum the alternate alleles across all individuals for every SNP
total_alleles <- 2 * length(TP_after_AF_downsampled) # Calculate the total number of alleles in the population (2 alleles per individual)
allele_frequencies_sim <- total_alt_alleles / total_alleles # Calculate the frequencies

## check order
SNPs_tested_ord <- match(SNP_AF_empirical$locus, SNPs_tested)
identical(SNPs_tested[SNPs_tested_ord], as.character(SNP_AF_empirical$locus))

SNPs_effsize_reord <- SNPs_effsize[SNPs_tested_ord]


#### Pull out outlier SNPs
filter_sig <- function(data){
  data_sig <- data %>% 
    filter(is.na(filt), holm_Lifestage < 0.05, !(snp_type=="climate")) 
  return(data_sig)
}

pop_glm_HS_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_pop_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "Pop_HS")
HS_glm_FB_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_FernBay_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "FernBay_HS")
HS_glm_HN_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_HawksNest_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "HawksNest_HS")
HS_glm_WC_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_WarrellCreek_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "WarrellCreek_HS")
HS_glm_LJ_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_LongJetty_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "LongJetty_HS")

all_results <- bind_rows(pop_glm_HS_sig, HS_glm_FB_sig, HS_glm_HN_sig, HS_glm_WC_sig, HS_glm_LJ_sig)
union_results <- all_results %>% group_by(Set) %>% distinct(locID, .keep_all = TRUE) %>% ungroup()


####

AF_comparison <- data.frame(locID = SNP_AF_empirical$locus, empirical_AF = SNP_AF_empirical$frequency, sim_AF = allele_frequencies_sim, eff_size = SNPs_effsize_reord) 
AF_comparison <- AF_comparison %>% 
  mutate(AF_diff = empirical_AF - sim_AF,
         SNP_Type = str_extract(AF_comparison$locID, "[^_]+$"),
         outlier_SNP = case_when(
           locID %in% union_results$locID ~ TRUE, 
           TRUE ~ FALSE
         ))


ggplot(AF_comparison, aes(x=empirical_AF, y=sim_AF, colour = eff_size)) +
  geom_point() +
  geom_point(data=(AF_comparison |> dplyr::filter(outlier_SNP == TRUE)), colour="red") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ggtitle(paste("AF comparisons at time", unique(TP_after_curr$time))) +
  facet_wrap(~SNP_Type)


################################################################################################################
#### Run FsT outlier test on the two allele matrices

TP_before_AF_matrix <- as.data.frame(lapply(TP_before_AF_downsampled, I)) # Convert AF list of lists to df
TP_after_AF_matrix <- as.data.frame(lapply(TP_after_AF_downsampled, I))
 
TP_AF_matrix <- as.matrix(cbind(TP_after_AF_matrix, TP_before_AF_matrix))
popID <- c(rep("post", ncol(TP_after_AF_matrix)), rep("pre", ncol(TP_before_AF_matrix)))

allele_counts_matrix <- (2 * (TP_AF_matrix == 0)) + (1 * (TP_AF_matrix == 1))
allele_counts_matrix[is.na(allele_counts_matrix)] <- 0

total_counts_matrix <- matrix(2, nrow = nrow(TP_AF_matrix), ncol = ncol(TP_AF_matrix))
total_counts_matrix[is.na(TP_AF_matrix)] <- 0

  ### Run QB GLM
run_quasibinom_glm <- function(snp_index, allele_count, total_count, popID) {
  ac <- allele_count[snp_index,]
  tc <- total_count[snp_index,]
  
  # initiate for NA
  pval_Population  <- NA
  
  # Check for zero variance
  if(var(ac) == 0) return(c(pval_TP=pval_TP))
  
  # Fit the model
  model <- glm(cbind(ac, tc - ac) ~ popID, family = "quasibinomial")
  
  if(!is.null(model)) {
    model_anova <- tryCatch({
      car::Anova(model, type = 3, test.statistic = "F")
    }, error = function(e) {
      return(NULL)
    })
    
    if(!is.null(model_anova)) {
      # Extract the respective p-values
      pval_TP   <- model_anova["popID", "Pr(>F)"]
    }
  }
  return(c(pval_TP=pval_TP))
}


pval_TP = NA; final_results_df = NULL
results_list <- lapply(1:nrow(allele_counts_matrix), function(i) {
  if (i%%100==0){cat("Running, ",i,"\n")}
  run_quasibinom_glm(snp_index = i, allele_count = allele_counts_matrix, total_count = total_counts_matrix, popID = popID )
})

final_results_df <- as.data.frame(do.call(rbind, results_list))
colnames(final_results_df) <- c("Timepoint")
final_results_df$holm_Timepoint <- p.adjust(final_results_df$Timepoint, method = "holm")

final_results_df <- cbind(locID = AF_comparison$locID, final_results_df) 
final_results_df <- final_results_df %>% 
  tidyr::extract(
    col = locID, 
    into = c("chr", "start_pos", "end_pos", "snp_type"),
    regex = "^(.+):(\\d+)-(\\d+)_(.+)$",
    remove = FALSE
  )


final_results_df_sim <- final_results_df %>% 
    filter(holm_Timepoint < 0.05, !(snp_type=="climate")) 

AF_comparison <- AF_comparison %>% 
  mutate(outlier_SNP_sim = case_when(
           locID %in% final_results_df_sim$locID ~ TRUE, 
           TRUE ~ FALSE
         ))

## Plot onto plot
ggplot(AF_comparison, aes(x=empirical_AF, y=sim_AF, colour = eff_size)) +
  geom_point(alpha = 0.5) +
  geom_point(data=(AF_comparison |> dplyr::filter(outlier_SNP_sim == TRUE)), colour="blue", shape=1, fill = "white" ) +
  geom_point(data=(AF_comparison |> dplyr::filter(outlier_SNP == TRUE)), colour="red", shape=1, fill = "white") +
  geom_point(data=(AF_comparison |> dplyr::filter(outlier_SNP == TRUE & outlier_SNP_sim == TRUE)), colour="green") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ggtitle(paste("AF comparisons at time", unique(TP_after_curr$time))) +
  labs(caption = "Red = HS outlier SNPs\nBlue = simulated outlier SNPs") +
  facet_wrap(~SNP_Type)  

## Any overlapping snps?
AF_comparison |> dplyr::filter(outlier_SNP == TRUE & outlier_SNP_sim == TRUE)
