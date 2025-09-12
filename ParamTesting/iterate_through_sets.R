library(tidyverse)

set.seed(123)

setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")
output_logfile = paste0("ParamTesting/run_log_", Sys.Date(),".txt")
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

## Load in parameters
source("Functions/mortality_functions_MRintro_hill.R")
source("Functions/mortality_functions_hill.R")
source("Functions/recruitment_functions_3.R")
source("Functions/disturbance_functions.R")
source("ParamTesting/parameter_testing_res_calc_functs.R")
###

run_status <- data.frame()
run_res <- data.frame()
run_res_LS <- data.frame()

# Load into environment
for (param_iter in 1:nrow(param_sets)){
  
  # Checkpoints at every 1/5th of the run sets
  if (param_iter %% n_t/5 == 0){
    write.csv(param_sets, file=paste0("ParamTesting/Checkpoints/ckpnt_param_sets.csv"), row.names=F)
    write.csv(run_status, file=paste0("ParamTesting/Checkpoints/ckpnt_run_status.csv"), row.names=F)
    write.csv(run_res, file=paste0("ParamTesting/Checkpoints/ckpnt_run_res.csv"), row.names=F)
    write.csv(run_res_LS, file=paste0("ParamTesting/Checkpoints/ckpntrun_res_LS.csv"), row.names=F)
  }
  
  # Load in params
  source("Intervention/configurations_int")
  list2env(as.list((param_sets)[param_iter,]), envir = .GlobalEnv)

  # Run sim
  tryCatch({ 
    source("ParamTesting/data_sim_3_versParamTest.R", local = TRUE)  
  
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

write.csv(param_sets, file=paste0("ParamTesting/param_sets_", Sys.Date(),".csv"), row.names=F)
write.csv(run_status, file=paste0("ParamTesting/run_status_", Sys.Date(),".csv"), row.names=F)
write.csv(run_res, file=paste0("ParamTesting/run_res_", Sys.Date(),".csv"), row.names=F)
write.csv(run_res_LS, file=paste0("ParamTesting/run_res_LS_", Sys.Date(),".csv"), row.names=F)



sink()
