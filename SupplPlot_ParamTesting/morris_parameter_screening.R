# Install necessary packages if missing
#docker run -it --rm -v /data/karina/Simulation/MqSim:/data -w /data mqsim R

## Load in parameters
source("Configuration_10.txt")
source("Functions/mortality_functions_MRintro_hill.R")
source("Functions/mortality_functions_hill.R")
source("Functions/recruitment_functions_4.R")
source("Functions/disturbance_functions.R")
source("Functions/genotype_phenotype_v1.R")
source("gapit_functions_080425.txt")
source("GP_functs.R")
source("SupplPlot_ParamTesting/parameter_testing_res_calc_functs.R")

library(sensitivity)
library(tidyverse)
library(scales)

set.seed(123)

# Redirect console output and messages to a log file
log_con <- file("SupplPlot_ParamTesting/morris_screening_log.txt", open = "a")
sink(log_con, append = TRUE, split = TRUE)
sink(log_con, append = TRUE, type = "message")


## GAPIT pre-processing (run once outside the loop to save time)
effect_size <- read.csv("Data_AlleleFrequency/SNP_eff_dom_size.csv", header = F) 
effect_size <- effect_size[!grepl("climate", effect_size$V1), ] %>% 
  arrange(V2, desc = T) %>% 
  slice_head(n=n_snps)
SNPs_tested <- effect_size$V1

SNP_AF_Histset <- read.csv("Data_AlleleFrequency/SNP_AF.csv", header = T) %>%
  filter(locus %in% effect_size$V1)

effect_size$V2 <- effect_size$V2 * 2

log_file="Data_SimGAPITS/pred_GAPIT_log.txt"
out_dir="Data_SimGAPITS"
gt_datafile = "Data_SimGAPITS/Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping/Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt"
input_phenodatafile="Data_SimGAPITS/mq_phenotypes.csv"

df_base <- read.csv(gt_datafile, sep = "\t", header = T) %>% filter(rs. %in% SNPs_tested)
hapmap_meta <- df_base[,1:11]
training_hapmap <- df_base[, append(1, grep("S_", colnames(df_base)))]
snp_refalt_identity <- df_base %>% separate(col = alleles, into = c("REF", "ALT"), sep = "/", fill = "right", remove = FALSE) %>% dplyr::select(rs., REF, ALT) 

SNPs_tested_ord <- match(SNPs_tested, snp_refalt_identity$rs.)
snp_refalt_identity_reord <- snp_refalt_identity[SNPs_tested_ord,]
ref_alleles <- snp_refalt_identity_reord$REF
alt_alleles <- snp_refalt_identity_reord$ALT

iupac_dict <- c("AG" = "R", "GA" = "R", "CT" = "Y", "TC" = "Y", "GC" = "S", "CG" = "S", "AT" = "W", "TA" = "W", "GT" = "K", "TG" = "K", "AC" = "M", "CA" = "M")
het_codes <- iupac_dict[paste0(ref_alleles, alt_alleles)]


# Define Morris design
factors <- c("MR_mean", "MR_sd", "age_impact", "MR_death_impact", "MR_age_impact", "age_recruit_impact_value", "MR_recruit_impact", "recruitment_const", "dist_imp")
binfs <- c(0, 0, 0.1, 0, 5, 0.01, 0.01, 0.002, 0)
bsups <- c(1, 1, 1.0, 1, 20, 1.0, 1.0, 0.007, 1) # dist_imp is boolean, we'll treat >0.5 as TRUE

r <- 20 # trajectories (20 * (9+1) = 200 runs)
x <- morris(model = NULL, factors = factors, r = r, design = list(type = "oat", levels = 5, grid.jump = 3), binf = binfs, bsup = bsups)
param_matrix <- x$X

checkpoint_file <- "/data/SupplPlot_ParamTesting/morris_checkpoint.RData"
start_iter <- 1

if (file.exists(checkpoint_file)) {
  load(checkpoint_file)
  start_iter <- length(results_list) + 1
  cat(sprintf("Resuming from checkpoint at iteration %d...\n", start_iter))
} else {
  results_list <- list()
  LS_results_list <- list()
}

cat("Running simulations sequentially...\n")

for (iter in start_iter:nrow(param_matrix)) {
  cat(sprintf("Running iteration %d...\n", iter))
  
  p <- param_matrix[iter, ]
  
  MR_mean <- p[1]
  MR_sd <- p[2]
  age_impact <- p[3]
  MR_death_impact <- p[4]
  MR_age_impact <- p[5]
  age_recruit_impact_value <- p[6]
  MR_recruit_impact <- p[7]
  recruitment_const <- p[8]
  dist_imp <- p[9] > 0.5
  
  # Run sim
  tryCatch({
    env <- environment()
    suppressWarnings(suppressMessages(source("SupplPlot_ParamTesting/data_sim_5_versParamTest.R", local = env)))
    
    # Calculate metrics
    res <- calculate_timepoint_vals(pop_timepoints)
    LS_res <- calculate_timepoint_LSvals(pop_timepoints)
    
    res$iteration <- iter
    LS_res$iteration <- iter
    
    results_list[[iter]] <- res
    LS_results_list[[iter]] <- LS_res
    
  }, error = function(e) {
    # Print error to console during sequential runs for easy debugging
    message(sprintf("Error in iteration %d: %s", iter, e$message))
    
    res_err <- data.frame(timeperiod = c("Before", "Soon", "After"), pop_struct = NA, pop_size = NA, mean_MR = NA, sd_MR = NA, pop_growth_trend = NA, pop_growth_R2 = NA, iteration = iter)
    LS_res_err <- data.frame(timeperiod = NA, Lifestage = NA, mean_MR = NA, sd_MR = NA, iteration = iter)
    
    results_list[[iter]] <- res_err
    LS_results_list[[iter]] <- LS_res_err
  })
  
  # Checkpoint every 10 runs
  if (iter %% 10 == 0) {
    cat(sprintf("Checkpointing at iteration %d...\n", iter))
    save(results_list, LS_results_list, file = checkpoint_file)
    
    temp_results <- do.call(rbind, results_list)
    temp_LS_results <- do.call(rbind, LS_results_list)
    
    param_df <- as.data.frame(param_matrix[1:iter, , drop=FALSE])
    colnames(param_df) <- factors
    param_df$iteration <- 1:iter
    
    temp_df <- merge(param_df, temp_results, by = "iteration")
    temp_LS_df <- merge(param_df, temp_LS_results, by = "iteration")
    
    write.csv(temp_df, sprintf("/data/SupplPlot_ParamTesting/morris_checkpoint_iter_%d.csv", iter), row.names = FALSE)
    write.csv(temp_LS_df, sprintf("/data/SupplPlot_ParamTesting/morris_checkpoint_LS_iter_%d.csv", iter), row.names = FALSE)
  }
}

results <- do.call(rbind, results_list)
LS_results <- do.call(rbind, LS_results_list)

param_df_full <- as.data.frame(param_matrix)
colnames(param_df_full) <- factors
param_df_full$iteration <- 1:nrow(param_matrix)

# Save the raw simulation output metrics to a dataframe
results_df <- merge(param_df_full, results, by = "iteration")
LS_results_df <- merge(param_df_full, LS_results, by = "iteration")

write.csv(results_df, "/data/SupplPlot_ParamTesting/morris_simulation_results.csv", row.names = FALSE)
write.csv(LS_results_df, "/data/SupplPlot_ParamTesting/morris_simulation_LS_results.csv", row.names = FALSE)
cat("Simulation metrics written to SupplPlot_ParamTesting/\n")

cat("Simulations complete. Analyzing sensitivity...\n")

# 3. Analyze Results
metrics_to_plot <- c("pop_struct_after", "pop_size_after", "pop_growth_trend_after", "mean_MR_seedling_soon", "mean_MR_subadult_soon")

morris_sensitivity_list <- list()

# Reconstruct the flat results format for sensitivity analysis
flat_results_list <- lapply(1:nrow(param_matrix), function(i) {
  r <- results_list[[i]]
  ls <- LS_results_list[[i]]
  
  after_r <- r[r$timeperiod == "After", ]
  seedling_ls <- ls[ls$timeperiod == "Soon" & ls$Lifestage == "Seedling", ]
  subadult_ls <- ls[ls$timeperiod == "Soon" & ls$Lifestage == "Subadult", ]
  
  c(
    pop_struct_after = if(nrow(after_r) > 0) as.numeric(after_r$pop_struct)[1] else 0,
    pop_size_after = if(nrow(after_r) > 0) as.numeric(after_r$pop_size)[1] else 0,
    pop_growth_trend_after = if(nrow(after_r) > 0) as.numeric(after_r$pop_growth_trend)[1] else 0,
    mean_MR_seedling_soon = if(nrow(seedling_ls) > 0) as.numeric(seedling_ls$mean_MR)[1] else NA,
    mean_MR_subadult_soon = if(nrow(subadult_ls) > 0) as.numeric(subadult_ls$mean_MR)[1] else NA
  )
})
flat_results <- do.call(rbind, flat_results_list)

for (metric in metrics_to_plot) {
  
  metric_res <- flat_results[, metric]
  metric_res[is.na(metric_res)] <- 0 # Treat NAs as 0 for analysis stability
  
  x_metric <- x
  tell(x_metric, metric_res)
  
  cat("\n==========================================\n")
  cat(sprintf("Sensitivity for %s:\n", metric))
  print(x_metric)
  
  # Extract Morris sensitivity metrics (mu, mu*, sigma) to a dataframe
  mu <- apply(x_metric$ee, 2, mean)
  mu.star <- apply(abs(x_metric$ee), 2, mean)
  sigma <- apply(x_metric$ee, 2, sd)
  
  morris_sensitivity_list[[metric]] <- data.frame(
    Output_Metric = metric,
    Parameter = factors,
    mu = mu,
    mu.star = mu.star,
    sigma = sigma
  )
  
  #png(sprintf("SupplPlot_ParamTesting/Morris_Sensitivity_%s.png", metric), width=800, height=600)
  plot(x_metric, main=sprintf("Morris Method: Sensitivity of %s", metric))
  #dev.off()
}

# Write sensitivity metrics to a dataframe
morris_sensitivity_df <- do.call(rbind, morris_sensitivity_list)
write.csv(morris_sensitivity_df, "/data/SupplPlot_ParamTesting/morris_sensitivity_metrics.csv", row.names = FALSE)

cat("\nMorris Method screening complete. Plots and metrics saved to SupplPlot_ParamTesting/ \n")

# Close log file connections
sink(type = "message")
sink()
close(log_con)
