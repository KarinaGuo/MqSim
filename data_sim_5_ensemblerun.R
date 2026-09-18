# Setup
rm(list = ls())

# Load configurations
source("Configuration_10.txt")


# Read simulation script and strip environment-clearing code

sim_script <- readLines("data_sim_5.R")
# Remove `rm(list = ls())` which wipes our ensemble variables
sim_script <- sim_script[!grepl("rm\\(list\\s*=\\s*ls\\(\\)\\)", sim_script)]
# Remove the save call at the end to avoid overwriting the same Rdata repeatedly
sim_script <- sim_script[!grepl("save\\(", sim_script)]
# Remove set.seed() to ensure runs are actually different
sim_script <- sim_script[!grepl("set\\.seed\\(", sim_script)]
sim_expr <- parse(text = paste(sim_script, collapse = "\n"))

# Initialize structures to hold results across runs
all_death_df <- list()
all_live_size_df <- list()
all_age_df <- list()
all_MR_df <- list()
all_pop_timepoints <- list()
all_AF_comparison <- list()

for (run_id in 1:ensemble_repeat_times) {
  cat(sprintf("\n=== Starting run %d of %d ===\n", run_id, ensemble_repeat_times))
  
  # Evaluate the modified script
  # We use eval in the current environment so it shares variables and output variables here.
  eval(sim_expr)
  
  # After run, extract the dataframes and add run_id
  if (exists("death_df") && !is.null(death_df) && nrow(death_df) > 0) {
    death_df$run_id <- run_id
    all_death_df[[run_id]] <- death_df
  }
  
  if (exists("live_size_df") && !is.null(live_size_df) && nrow(live_size_df) > 0) {
    live_size_df$run_id <- run_id
    all_live_size_df[[run_id]] <- live_size_df
  }
  
  if (exists("age_df") && !is.null(age_df) && nrow(age_df) > 0) {
    age_df$run_id <- run_id
    all_age_df[[run_id]] <- age_df
  }
  
  if (exists("MR_df") && !is.null(MR_df) && nrow(MR_df) > 0) {
    MR_df$run_id <- run_id
    all_MR_df[[run_id]] <- MR_df
  }
  
  # For pop_timepoints
  if (exists("pop_timepoints") && length(pop_timepoints) > 0) {
    df_list <- lapply(seq_along(pop_timepoints), function(i) {
      pop <- pop_timepoints[[i]]
      if (!is.null(pop)) {
        data.frame(
          run_id = run_id,
          time = pop$time,
          age = pop$age,
          MR = pop$MR
        )
      }
    })
    all_pop_timepoints[[run_id]] <- do.call(rbind, df_list)
  }
  
  # For allele frequencies (mimicking post_run_analyses_plots.R for downstream SNP plots)
  if (exists("AF_timepoints") && length(AF_timepoints) >= 2 && exists("pop_timepoints") && length(pop_timepoints) >= 2) {
    # It assumes index 2 is timepoint 990, just like post_run_analyses_plots.R
    TP_before_AF <- AF_timepoints[[2]] 
    TP_before_curr <- pop_timepoints[[2]] 
    
    if (!is.null(TP_before_curr) && !is.null(TP_before_AF)) {
      young_indvs <- TP_before_curr$age < 5
      
      if (sum(young_indvs) > 0) {
          TP_before_AF_young <- TP_before_AF[young_indvs]
          sample_size <- min(length(TP_before_AF_young), 124)
          TP_before_AF_downsampled <- TP_before_AF_young[sample(length(TP_before_AF_young), sample_size)]
          
          total_alt_alleles <- Reduce(`+`, TP_before_AF_downsampled) 
          total_alleles <- 2 * length(TP_before_AF_downsampled)
          allele_frequencies_sim <- total_alt_alleles / total_alleles 
          
          if (exists("effect_size")) {
              SNPs_tested <- effect_size$V1
              SNPs_effsize <- effect_size$V2
              af_df <- data.frame(
                  run_id = run_id,
                  locID = SNPs_tested,
                  sim_AF = allele_frequencies_sim,
                  eff_size = SNPs_effsize
              )
              all_AF_comparison[[run_id]] <- af_df
          }
      }
    }
  }
}

cat("\nAggregating results...\n")

# Combine results
final_death_df <- do.call(rbind, all_death_df)
final_live_size_df <- do.call(rbind, all_live_size_df)
final_age_df <- do.call(rbind, all_age_df)
final_MR_df <- do.call(rbind, all_MR_df)
final_pop_timepoints <- do.call(rbind, all_pop_timepoints)
final_AF_comparison <- do.call(rbind, all_AF_comparison)

# Create Run_results dir if it doesn't exist
if (!dir.exists("Run_results")) {
  dir.create("Run_results")
}

# Write CSV files
if (!is.null(final_death_df)) write.csv(final_death_df, "Run_results/ensemble_death_df.csv", row.names = FALSE)
if (!is.null(final_live_size_df)) write.csv(final_live_size_df, "Run_results/ensemble_live_size_df.csv", row.names = FALSE)
if (!is.null(final_age_df)) write.csv(final_age_df, "Run_results/ensemble_age_df.csv", row.names = FALSE)
if (!is.null(final_MR_df)) write.csv(final_MR_df, "Run_results/ensemble_MR_df.csv", row.names = FALSE)
if (!is.null(final_pop_timepoints)) write.csv(final_pop_timepoints, "Run_results/ensemble_pop_timepoints.csv", row.names = FALSE)
if (!is.null(final_AF_comparison)) write.csv(final_AF_comparison, "Run_results/ensemble_AF_comparison.csv", row.names = FALSE)

