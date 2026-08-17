library(testthat)

# Note: assuming working directory is project root
source("Functions/recruitment_functions_4.R")

test_that("recruit_rate behaves across a wide parameter space", {
  
  # Basic mock population 
  create_mock_pop <- function(size) {
    if (size == 0) {
      return(list(indiv_ID=numeric(0), age=numeric(0), MR=numeric(0), time=numeric(0), mortality=numeric(0), error=numeric(0)))
    }
    list(
      indiv_ID = 1:size,
      age = sample(1:50, size, replace = TRUE),
      MR = runif(size, 0, 1),
      time = rep(1, size),
      mortality = rep(0, size),
      error = runif(size, -0.05, 0.05)
    )
  }
  
  # Genotypes mock
  create_mock_gt <- function(size) {
    if (size == 0) return(list())
    lapply(1:size, function(x) rbinom(10, 2, 0.5))
  }
  
  pop_sizes <- c(0, 5, 50, 150)
  min_sizes <- c(10, 100)
  max_sizes <- c(100, 500)
  MR_impacts <- c(0.1, 1, 10)
  age_impacts <- c(0.1, 1, 10)
  density_toggs <- c(TRUE, FALSE)
  
  for (s in pop_sizes) {
    for (min_s in min_sizes) {
      for (max_s in max_sizes) {
        for (m_i in MR_impacts) {
          for (a_i in age_impacts) {
            for (d_t in density_toggs) {
              
              pop <- create_mock_pop(s)
              gt <- create_mock_gt(s)
              
              res <- recruit_rate(
                pop = pop, 
                population_min_size = min_s, 
                population_max_size = max_s, 
                recruitment_age = 7, 
                recruitment_size_mean = 5, 
                density_recruit_togg = d_t, 
                recruitment_size_sd = 1, 
                recruitment_constant = 0.1, 
                age_togg = TRUE, 
                age_recruit_impact_val = a_i, 
                MR_togg = TRUE, 
                MR_recruit_impact_val = m_i, 
                MR_rec_adjusted = 0, 
                rec_age_shiftch = 10, 
                MR_parents = 2, 
                population_genotypes = gt, 
                indiv_count_start = s, 
                time_point = 1
              )
              
              # Should return a list with curr_pop and curr_AF
              expect_true(is.list(res))
              expect_true("curr_pop" %in% names(res))
              expect_true("curr_AF" %in% names(res))
              
              # If population was 0, it should remain 0
              if (s == 0) {
                 expect_equal(length(res$curr_pop$indiv_ID), 0)
              } else {
                 # New population shouldn't be smaller than initial (since recruit only adds)
                 expect_true(length(res$curr_pop$indiv_ID) >= s)
              }
            }
          }
        }
      }
    }
  }
})
