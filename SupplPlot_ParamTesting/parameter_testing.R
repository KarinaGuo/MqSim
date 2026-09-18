# Radial Expansion Parameter Sensitivity Testing
# Iterates across 100 simulations, sampling parameter combinations on 
# concentric shells radiating outward from a center point.
#
# Parameters tested:
#   MR_death_impact:    0.5 - 1.0   (center: 0.75)
#   age_impact:         0.1 - 0.4   (center: 0.25)
#   recruitment_const:  0.003 - 0.007 (center: 0.005)
#
# Fixed parameters (from Configuration_10.txt baseline):
#   age_recruit_impact_value = 1
#
# Design: 10 concentric shells x 10 points per shell = 100 iterations
#   Shell radii at 10%, 20%, ..., 100% of the maximum normalised radius.
#   Points on each shell placed via Fibonacci sphere sampling for even coverage.

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

library(tidyverse)
library(scales)

set.seed(42)

# Redirect console output and messages to a log file
log_con <- file("SupplPlot_ParamTesting/parameter_testing_log.txt", open = "a")
sink(log_con, append = TRUE, split = TRUE)
sink(log_con, append = TRUE, type = "message")

## ─── Parameter ranges and center ────────────────────────────────────────────

# Ranges for each parameter
param_ranges <- data.frame(
  param = c("MR_death_impact", "age_impact", "recruitment_const"),
  lower = c(0.5, 0.1, 0.003),
  upper = c(1.0, 0.4, 0.007),
  stringsAsFactors = FALSE
)

# Center point (midpoint of each range)
center <- c(
  MR_death_impact   = 0.75,
  age_impact        = 0.25,
  recruitment_const = 0.005
)

# Half-widths for normalisation (center to boundary)
half_widths <- c(
  MR_death_impact   = 0.25,
  age_impact        = 0.15,
  recruitment_const = 0.002
)

# Fixed parameters from the chosen configuration
fixed_age_recruit_impact_value <- 1
fixed_MR_age_impact            <- 9      # from Configuration_10.txt
fixed_MR_recruit_impact        <- 0.2575 # from Configuration_10.txt

## ─── Generate Radial Expansion Design ───────────────────────────────────────

n_shells     <- 10   # number of concentric shells
pts_per_shell <- 10  # points per shell
n_total      <- n_shells * pts_per_shell  # 100 iterations

# Fibonacci sphere: distribute n points approximately evenly on a unit sphere
fibonacci_sphere <- function(n) {
  golden_ratio <- (1 + sqrt(5)) / 2
  indices <- 0:(n - 1)
  theta <- acos(1 - 2 * (indices + 0.5) / n)     # polar angle

  phi   <- 2 * pi * indices / golden_ratio         # azimuthal angle
  
  x <- sin(theta) * cos(phi)
  y <- sin(theta) * sin(phi)
  z <- cos(theta)
  
  cbind(x, y, z)
}

# Build the parameter matrix
param_matrix <- matrix(NA, nrow = n_total, ncol = 3)
colnames(param_matrix) <- names(center)

row_idx <- 1
for (s in 1:n_shells) {
  # Radius fraction: 10%, 20%, ..., 100% of max
  r_frac <- s / n_shells
  
  # Get unit-sphere directions for this shell
  dirs <- fibonacci_sphere(pts_per_shell)
  
  for (p in 1:pts_per_shell) {
    # Scale: normalised offset = radius_fraction * direction
    normalised_offset <- r_frac * dirs[p, ]
    
    # Map back to real parameter space
    real_values <- center + normalised_offset * half_widths
    
    # Clamp to the valid ranges
    real_values[1] <- max(param_ranges$lower[1], min(param_ranges$upper[1], real_values[1]))
    real_values[2] <- max(param_ranges$lower[2], min(param_ranges$upper[2], real_values[2]))
    real_values[3] <- max(param_ranges$lower[3], min(param_ranges$upper[3], real_values[3]))
    
    param_matrix[row_idx, ] <- real_values
    row_idx <- row_idx + 1
  }
}

param_matrix <- as.data.frame(param_matrix)

cat(sprintf("Radial expansion design: %d shells x %d pts = %d iterations\n",
            n_shells, pts_per_shell, n_total))
cat("Parameter ranges:\n")
print(param_ranges)
cat("\nCenter point:\n")
print(center)
cat("\nFirst 10 rows of design matrix:\n")
print(head(param_matrix, 10))

## ─── GAPIT pre-processing (run once) ────────────────────────────────────────

effect_size <- read.csv("Data_AlleleFrequency/SNP_eff_dom_size.csv", header = F)
effect_size <- effect_size[!grepl("climate", effect_size$V1), ] %>%
  arrange(V2, desc = T) %>%
  slice_head(n = n_snps)
SNPs_tested <- effect_size$V1

SNP_AF_Histset <- read.csv("Data_AlleleFrequency/SNP_AF.csv", header = T) %>%
  filter(locus %in% effect_size$V1)

effect_size$V2 <- effect_size$V2 * 2

log_file <- "Data_SimGAPITS/pred_GAPIT_log.txt"
out_dir  <- "Data_SimGAPITS"
gt_datafile <- "Data_SimGAPITS/Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping/Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt"
input_phenodatafile <- "Data_SimGAPITS/mq_phenotypes.csv"

df_base <- read.csv(gt_datafile, sep = "\t", header = T) %>%
  filter(rs. %in% SNPs_tested)
hapmap_meta <- df_base[, 1:11]
training_hapmap <- df_base[, append(1, grep("S_", colnames(df_base)))]
snp_refalt_identity <- df_base %>%
  separate(col = alleles, into = c("REF", "ALT"), sep = "/", fill = "right", remove = FALSE) %>%
  dplyr::select(rs., REF, ALT)

SNPs_tested_ord <- match(SNPs_tested, snp_refalt_identity$rs.)
snp_refalt_identity_reord <- snp_refalt_identity[SNPs_tested_ord, ]
ref_alleles <- snp_refalt_identity_reord$REF
alt_alleles <- snp_refalt_identity_reord$ALT

iupac_dict <- c("AG" = "R", "GA" = "R", "CT" = "Y", "TC" = "Y",
                "GC" = "S", "CG" = "S", "AT" = "W", "TA" = "W",
                "GT" = "K", "TG" = "K", "AC" = "M", "CA" = "M")
het_codes <- iupac_dict[paste0(ref_alleles, alt_alleles)]


## ─── Checkpointing ──────────────────────────────────────────────────────────

checkpoint_file <- "/data/SupplPlot_ParamTesting/radial_checkpoint.RData"
start_iter <- 1

if (file.exists(checkpoint_file)) {
  load(checkpoint_file)
  start_iter <- length(results_list) + 1
  cat(sprintf("Resuming from checkpoint at iteration %d...\n", start_iter))
} else {
  results_list    <- list()
  LS_results_list <- list()
}


## ─── Run Simulations ────────────────────────────────────────────────────────

factors <- c("MR_death_impact", "age_impact", "recruitment_const")

cat("Running simulations sequentially...\n")

for (iter in start_iter:nrow(param_matrix)) {
  cat(sprintf("Running iteration %d / %d  (shell %d, point %d)...\n",
              iter, n_total,
              ceiling(iter / pts_per_shell),
              ((iter - 1) %% pts_per_shell) + 1))
  
  p <- param_matrix[iter, ]
  
  # Set the swept parameters
  MR_death_impact          <- p$MR_death_impact
  age_impact               <- p$age_impact
  recruitment_const        <- p$recruitment_const
  
  # Set the fixed parameters
  age_recruit_impact_value <- fixed_age_recruit_impact_value
  MR_age_impact            <- fixed_MR_age_impact
  MR_recruit_impact        <- fixed_MR_recruit_impact
  
  # Run simulation
  tryCatch({
    suppressWarnings(suppressMessages(
      source("SupplPlot_ParamTesting/data_sim_5_versParamTest.R", local = TRUE)
    ))
    
    # Calculate metrics
    res    <- calculate_timepoint_vals(pop_timepoints)
    LS_res <- calculate_timepoint_LSvals(pop_timepoints)
    
    res$iteration    <- iter
    LS_res$iteration <- iter
    
    results_list[[iter]]    <- res
    LS_results_list[[iter]] <- LS_res
    
  }, error = function(e) {
    message(sprintf("Error in iteration %d: %s", iter, e$message))
    
    res_err <- data.frame(
      timeperiod       = c("Before", "Soon", "After"),
      pop_struct       = NA,
      pop_size         = NA,
      mean_MR          = NA,
      sd_MR            = NA,
      pop_growth_trend = NA,
      pop_growth_R2    = NA,
      iteration        = iter
    )
    LS_res_err <- data.frame(
      timeperiod = NA,
      Lifestage  = NA,
      mean_MR    = NA,
      sd_MR      = NA,
      iteration  = iter
    )
    
    results_list[[iter]]    <<- res_err
    LS_results_list[[iter]] <<- LS_res_err
  })
  
  # Checkpoint every 10 runs
  if (iter %% 10 == 0) {
    cat(sprintf("Checkpointing at iteration %d...\n", iter))
    save(results_list, LS_results_list, file = checkpoint_file)
    
    temp_results    <- do.call(rbind, results_list)
    temp_LS_results <- do.call(rbind, LS_results_list)
    
    param_df <- param_matrix[1:iter, , drop = FALSE]
    param_df$iteration <- 1:iter
    
    # Add shell and point metadata
    param_df$shell <- ceiling(1:iter / pts_per_shell)
    param_df$radius_frac <- param_df$shell / n_shells
    
    temp_df    <- merge(param_df, temp_results, by = "iteration")
    temp_LS_df <- merge(param_df, temp_LS_results, by = "iteration")
    
    write.csv(temp_df,
              sprintf("/data/SupplPlot_ParamTesting/radial_checkpoint_iter_%d.csv", iter),
              row.names = FALSE)
    write.csv(temp_LS_df,
              sprintf("/data/SupplPlot_ParamTesting/radial_checkpoint_LS_iter_%d.csv", iter),
              row.names = FALSE)
  }
}


## ─── Combine and Save Final Results ─────────────────────────────────────────

results    <- do.call(rbind, results_list)
LS_results <- do.call(rbind, LS_results_list)

param_df_full <- param_matrix
param_df_full$iteration   <- 1:nrow(param_matrix)
param_df_full$shell       <- ceiling(1:nrow(param_matrix) / pts_per_shell)
param_df_full$radius_frac <- param_df_full$shell / n_shells

# Compute Euclidean distance from center in normalised space for each point
param_df_full$norm_distance <- sqrt(
  ((param_df_full$MR_death_impact - center["MR_death_impact"]) / half_widths["MR_death_impact"])^2 +
  ((param_df_full$age_impact - center["age_impact"]) / half_widths["age_impact"])^2 +
  ((param_df_full$recruitment_const - center["recruitment_const"]) / half_widths["recruitment_const"])^2
)

results_df    <- merge(param_df_full, results, by = "iteration")
LS_results_df <- merge(param_df_full, LS_results, by = "iteration")

write.csv(results_df,
          "/data/SupplPlot_ParamTesting/radial_simulation_results.csv",
          row.names = FALSE)
write.csv(LS_results_df,
          "/data/SupplPlot_ParamTesting/radial_simulation_LS_results.csv",
          row.names = FALSE)

# Save the design matrix separately for reference
write.csv(param_df_full,
          "/data/SupplPlot_ParamTesting/radial_design_matrix.csv",
          row.names = FALSE)

cat("Simulation results written to SupplPlot_ParamTesting/\n")
cat("  - radial_simulation_results.csv\n")
cat("  - radial_simulation_LS_results.csv\n")
cat("  - radial_design_matrix.csv\n")


## ─── Summary Statistics ─────────────────────────────────────────────────────

cat("\n=== Radial Expansion Sensitivity Summary ===\n")

# Summarise by shell (distance from center)
shell_summary <- results_df %>%
  filter(timeperiod == "After") %>%
  group_by(shell, radius_frac) %>%
  summarise(
    n_valid     = sum(!is.na(pop_size)),
    mean_pop_size   = mean(as.numeric(pop_size), na.rm = TRUE),
    sd_pop_size     = sd(as.numeric(pop_size), na.rm = TRUE),
    mean_pop_struct = mean(as.numeric(pop_struct), na.rm = TRUE),
    mean_pop_trend  = mean(as.numeric(pop_growth_trend), na.rm = TRUE),
    mean_MR         = mean(as.numeric(mean_MR), na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMetrics by shell (After period):\n")
print(as.data.frame(shell_summary))

write.csv(shell_summary,
          "/data/SupplPlot_ParamTesting/radial_shell_summary.csv",
          row.names = FALSE)


# Close log file connections
sink(type = "message")
sink()
close(log_con)
