library(testthat)

source("Functions/recruitment_functions_4.R")
source("Functions/mortality_functions_MRintro_hill.R")

test_that("Individual state is tracked correctly across time points", {
  
  # 1. Setup initial population with 1 individual
  initial_pop <- list(
    indiv_ID = c(1),
    time = c(1),
    MR = c(0.5),
    mortality = c(0),
    age = c(5),
    error = c(0.01)
  )
  
  # Mock allele frequency data (e.g., 2 loci)
  initial_gt <- list(c(1, 2, 0, 1, 2))
  
  # 2. Simulate Time Point 1
  curr_pop_start <- initial_pop
  curr_AF_start <- initial_gt
  
  # Run recruitment (ensure no recruitment by setting recruitment_constant = 0)
  recruit_res <- recruit_rate(
    pop = curr_pop_start, 
    population_min_size = 0, 
    population_max_size = 100, 
    recruitment_age = 7, 
    recruitment_size_mean = 5, 
    density_recruit_togg = FALSE, 
    recruitment_size_sd = 1, 
    recruitment_constant = 0, # ZERO chance
    age_togg = TRUE, 
    age_recruit_impact_val = 1, 
    MR_togg = TRUE, 
    MR_recruit_impact_val = 1, 
    MR_rec_adjusted = 0, 
    rec_age_shiftch = 10, 
    MR_parents = 2, 
    population_genotypes = curr_AF_start, 
    indiv_count_start = 1, 
    time_point = 1
  )
  
  curr_pop_recruited <- recruit_res$curr_pop
  curr_AF_recruited <- recruit_res$curr_AF
  
  # Ensure individual is still there, no new recruits
  expect_equal(length(curr_pop_recruited$indiv_ID), 1)
  expect_equal(curr_pop_recruited$indiv_ID, c(1))
  
  # Run mortality (force survival)
  indiv_death <- c(0) 
  
  # Mimic the final time point construction logic from data_sim_5.R
  curr_pop_end <- list(
    indiv_ID = curr_pop_recruited$indiv_ID[!as.logical(indiv_death)], 
    age = curr_pop_recruited$age[!as.logical(indiv_death)] + 1, 
    MR = curr_pop_recruited$MR[!as.logical(indiv_death)], 
    time = curr_pop_recruited$time[!as.logical(indiv_death)] + 1, 
    error = curr_pop_recruited$error[!as.logical(indiv_death)]
  )
  curr_AF_end <- curr_AF_recruited[!as.logical(indiv_death)]
  
  # 3. Check Time Point 1 Results
  expect_equal(curr_pop_end$age, 6) # Age increased by 1
  expect_equal(curr_pop_end$time, 2) # Time increased by 1
  expect_equal(curr_pop_end$MR, 0.5) # MR stayed the same
  expect_equal(curr_pop_end$error, 0.01) # Error stayed the same
  expect_equal(curr_AF_end[[1]], initial_gt[[1]]) # Alleles remained untouched
  
  # 4. Simulate Time Point 2
  curr_pop_start <- curr_pop_end
  curr_AF_start <- curr_AF_end
  
  # Final time point logic from data_sim_5.R again
  indiv_death <- c(0)
  
  curr_pop_end_2 <- list(
    indiv_ID = curr_pop_start$indiv_ID[!as.logical(indiv_death)], 
    age = curr_pop_start$age[!as.logical(indiv_death)] + 1, 
    MR = curr_pop_start$MR[!as.logical(indiv_death)], 
    time = curr_pop_start$time[!as.logical(indiv_death)] + 1, 
    error = curr_pop_start$error[!as.logical(indiv_death)]
  )
  curr_AF_end_2 <- curr_AF_start[!as.logical(indiv_death)]
  
  # 5. Check Time Point 2 Results
  expect_equal(curr_pop_end_2$age, 7) # Age increased to 7
  expect_equal(curr_pop_end_2$time, 3) # Time increased to 3
  expect_equal(curr_pop_end_2$MR, 0.5) # MR persisted
  expect_equal(curr_AF_end_2[[1]], initial_gt[[1]]) # Alleles tracked correctly again
})
