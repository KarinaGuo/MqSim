set.seed(123)

output_logfile = "ParamTesting/run_log.txt"
sink(output_logfile)

n_t=100 # How many times to run iteration

param_sets <- data.frame(
  MR_mean = runif(n_t, min = 0, max = 1),
  MR_sd = runif(n_t, min = 0, max = 1),
  age_impact = runif(n_t, min = 0.1, max = 1),
  MR_death_impact = runif(n_t, min = 0, max = 1),
  MR_age_impact = runif(n_t, min = 5, max = 20),
  recruitment_const = runif(n_t, min = 0.002, max = 0.007),
  dist_imp = sample(c(TRUE, FALSE), n_t, replace = TRUE)
)

param_sets <- param_sets %>% mutate(i=row_number())

## Libraries
## Load in libraries
library(tidyverse)

## Load in parameters
source("Intervention/configurations_int")
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
  
  # Load in params
  source("Intervention/configurations_int")
  list2env(as.list((param_sets)[1,]), envir = .GlobalEnv)

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

run_status
run_res
run_res_LS

sink()
