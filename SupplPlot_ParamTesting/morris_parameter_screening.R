# Install necessary packages if missing
#docker run -it --rm -v /data/karina/Simulation/MqSim:/data -w /data mqsim R

library(sensitivity)
library(tidyverse)
library(doParallel)
library(foreach)

set.seed(123)

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

# 2. Parallel Evaluation
cores <- max(1, parallel::detectCores() - 1)
cl <- makeCluster(cores)
registerDoParallel(cl)

cat(sprintf("Running simulations in parallel across %d cores...\n", cores))

results <- foreach(i = 1:nrow(param_matrix), .packages = c("tidyverse", "scales"), .export = c("effect_size", "SNP_AF_Histset", "SNPs_tested"), .combine = rbind) %dopar% {
  
  # Source functions and config inside worker
  source("Configuration_10.txt")
  source("Functions/mortality_functions_MRintro_hill.R")
  source("Functions/mortality_functions_hill.R")
  source("Functions/recruitment_functions_4.R")
  source("Functions/disturbance_functions.R")
  source("Functions/genotype_phenotype_v1.R")
  source("gapit_functions_080425.txt")
  source("GP_functs.R")
  source("SupplPlot_ParamTesting/parameter_testing_res_calc_functs.R")
  
  # Map parameters
  p <- param_matrix[i, ]
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
    source("SupplPlot_ParamTesting/data_sim_5_versParamTest.R", local = TRUE)
    
    # Calculate metrics
    res <- calculate_timepoint_vals(pop_timepoints)
    LS_res <- calculate_timepoint_LSvals(pop_timepoints)
    
    after_res <- res %>% filter(timeperiod == "After")
    seedling_soon <- LS_res %>% filter(timeperiod == "Soon", Lifestage == "Seedling")
    subadult_soon <- LS_res %>% filter(timeperiod == "Soon", Lifestage == "Subadult")
    
    c(pop_struct_after = as.numeric(after_res$pop_struct), 
      pop_size_after = as.numeric(after_res$pop_size),
      pop_growth_trend_after = as.numeric(after_res$pop_growth_trend),
      mean_MR_seedling_soon = as.numeric(seedling_soon$mean_MR),
      mean_MR_subadult_soon = as.numeric(subadult_soon$mean_MR))
  }, error = function(e) {
    # If population died
    c(pop_struct_after = 0, pop_size_after = 0, pop_growth_trend_after = 0, mean_MR_seedling_soon = NA, mean_MR_subadult_soon = NA)
  })
}
stopCluster(cl)

# Save the raw simulation output metrics to a dataframe
results_df <- cbind(as.data.frame(param_matrix), as.data.frame(results))
colnames(results_df)[1:length(factors)] <- factors
write.csv(results_df, "SupplPlot_ParamTesting/morris_simulation_results.csv", row.names = FALSE)
cat("Simulation metrics written to SupplPlot_ParamTesting/morris_simulation_results.csv\n")

cat("Simulations complete. Analyzing sensitivity...\n")

# 3. Analyze Results
metrics_to_plot <- c("pop_struct_after", "pop_size_after", "pop_growth_trend_after", "mean_MR_seedling_soon", "mean_MR_subadult_soon")

morris_sensitivity_list <- list()

for (metric in metrics_to_plot) {
  
  metric_res <- results[, metric]
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
write.csv(morris_sensitivity_df, "SupplPlot_ParamTesting/morris_sensitivity_metrics.csv", row.names = FALSE)

cat("\nMorris Method screening complete. Plots and metrics saved to SupplPlot_ParamTesting/ \n")
