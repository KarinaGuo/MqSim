library(tidyverse)

set.seed(123)

output_logfile = paste0("SupplPlot_ParamTesting/run_log_", Sys.Date(),".txt")
sink(output_logfile)

n_t=1000 # How many times to run iteration

param_sets <- data.frame(
  MR_mean = runif(n_t, min = 0, max = 1),
  MR_sd = runif(n_t, min = 0, max = 1),
  age_impact = runif(n_t, min = 0.1, max = 1),
  MR_death_impact = runif(n_t, min = 0, max = 1),
  MR_age_impact = runif(n_t, min = 5, max = 20),
  age_recruit_impact_value = runif(n_t, min = 0.01, max = 1),
  MR_recruit_impact = runif(n_t, min = 0.01, max = 1),
  recruitment_const = runif(n_t, min = 0.002, max = 0.007),
  dist_imp = sample(c(TRUE, FALSE), n_t, replace = TRUE)
)

# Final strong MR pres selection
param_sets <- rbind(param_sets, cbind(MR_mean=0.787, MR_sd = 0.385, age_impact = 1.0, MR_death_impact = 0.12, MR_age_impact = 10, age_recruit_impact_value=0.25, MR_recruit_impact=0.75, recruitment_const = 0.003, dist_imp = F))
# Final weak MR pres selection
param_sets <- rbind(param_sets, cbind(MR_mean=0.787, MR_sd = 0.385, age_impact = 1.0, MR_death_impact = 0.2, MR_age_impact = 10, age_recruit_impact_value=0.25, MR_recruit_impact=0.75, recruitment_const = 0.003, dist_imp = F))
param_sets <- param_sets %>% mutate(i=row_number())

## Libraries
## Load in libraries
library(tidyverse)
library(scales)

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

df_base <- read.csv(gt_datafile, sep = "\t", header = T) %>% 
  filter(rs. %in% SNPs_tested)
hapmap_meta <- df_base[,1:11]
training_hapmap <- df_base[, append(1, grep("S_", colnames(df_base)))]
snp_refalt_identity <- df_base %>%
  separate(col = alleles, into = c("REF", "ALT"), sep = "/", fill = "right", remove = FALSE) %>% 
  dplyr::select(rs., REF, ALT) 

SNPs_tested_ord <- match(SNPs_tested, snp_refalt_identity$rs.)
snp_refalt_identity_reord <- snp_refalt_identity[SNPs_tested_ord,]
ref_alleles <- snp_refalt_identity_reord$REF
alt_alleles <- snp_refalt_identity_reord$ALT

iupac_dict <- c(
  "AG" = "R", "GA" = "R", "CT" = "Y", "TC" = "Y",
  "GC" = "S", "CG" = "S", "AT" = "W", "TA" = "W",
  "GT" = "K", "TG" = "K", "AC" = "M", "CA" = "M"
)
het_codes <- iupac_dict[paste0(ref_alleles, alt_alleles)]
###

run_status <- data.frame()
run_res <- data.frame()
run_res_LS <- data.frame()

# Load into environment
for (param_iter in 1:nrow(param_sets)){
  
  # Checkpoints at every 1/5th of the run sets
  if (param_iter %% n_t/5 == 0){
    write.csv(param_sets, file=paste0("SupplPlot_ParamTesting/Checkpoints/ckpnt_param_sets.csv"), row.names=F)
    write.csv(run_status, file=paste0("SupplPlot_ParamTesting/Checkpoints/ckpnt_run_status.csv"), row.names=F)
    write.csv(run_res, file=paste0("SupplPlot_ParamTesting/Checkpoints/ckpnt_run_res.csv"), row.names=F)
    write.csv(run_res_LS, file=paste0("SupplPlot_ParamTesting/Checkpoints/ckpntrun_res_LS.csv"), row.names=F)
  }
  
  # Load in params
  source("MainPlot_Intervention/configurations_int")
  list2env(as.list((param_sets)[param_iter,]), envir = .GlobalEnv)

  # Run sim
  tryCatch({ 
    source("SupplPlot_ParamTesting/data_sim_5_versParamTest.R", local = TRUE)  
  
    # Save res if population survives
    run_status <- rbind(run_status, data.frame(param_iteration = param_iter, status = "success"))
    
    run_res <- data.frame(rbind(run_res, cbind(param_iteration = param_iter, calculate_timepoint_vals(pop_timepoints))))
    run_res_LS <- data.frame(rbind(run_res_LS, cbind(param_iteration = param_iter, calculate_timepoint_LSvals(pop_timepoints))))
    
    cat(paste(Sys.time(), "\n"))
    cat(paste(param_iter, "ran till end (survived) 🐵 \n"))
  }, error = function(e) {
    
    # Write death pop
    run_status <- rbind(run_status, data.frame(param_iteration = param_iter, status = "population dead"))
    
    cat(paste(param_iter, "died 👵 \n"))
  })
  
}

write.csv(param_sets, file=paste0("SupplPlot_ParamTesting/param_sets_", Sys.Date(),".csv"), row.names=F)
write.csv(run_status, file=paste0("SupplPlot_ParamTesting/run_status_", Sys.Date(),".csv"), row.names=F)
write.csv(run_res, file=paste0("SupplPlot_ParamTesting/run_res_", Sys.Date(),".csv"), row.names=F)
write.csv(run_res_LS, file=paste0("SupplPlot_ParamTesting/run_res_LS_", Sys.Date(),".csv"), row.names=F)



sink()
