library(testthat)

# Note: assuming working directory is project root
source("Functions/mortality_functions_MRintro_hill.R")

test_that("Hill_eqn returns values in bounds over a wide range of parameters", {
  ages <- c(0, 10, 100, 150, 200, 500)
  steepnesses <- c(1.1, 2, 10, 15) # avoid 1 because steepness - 1 / steepness + 1 = 0 => 0^(1/1) = 0
  shifts <- c(50, 150, 300)
  flips <- c(TRUE, FALSE)
  
  for (a in ages) {
    for (s in steepnesses) {
      for (sh in shifts) {
        for (f in flips) {
          res <- Hill_eqn(age=a, steepness=s, mortality_age_shiftch=sh, flip=f)
          expect_true(is.numeric(res), info=paste("age:", a, "steepness:", s, "shift:", sh, "flip:", f))
          # Hill equation output should be finite
          expect_true(is.finite(res), info=paste("age:", a, "steepness:", s, "shift:", sh, "flip:", f))
        }
      }
    }
  }
})

test_that("young_mortality returns values in bounds (0 to 1) over a wide range", {
  ages <- c(0, 10, 50, 100, 150, 200)
  impacts <- c(0, 0.1, 0.5, 1)
  shifts <- c(50, 150, 300)
  
  for (a in ages) {
    for (i in impacts) {
      for (sh in shifts) {
        res <- young_mortality(age_x=a, age_impact_val=i, mortality_age_shiftch=sh)
        expect_true(is.numeric(res), info=paste("age:", a, "impact:", i, "shift:", sh))
        # Sometimes floating point operations result in slightly >1 values, but should be bounded
        expect_true(res >= 0 && res <= 1 + 1e-6, info=paste("age:", a, "impact:", i, "shift:", sh, "res:", res))
      }
    }
  }
})

test_that("mature_mortality returns values in bounds (0 to 1) over a wide range", {
  ages <- c(0, 10, 50, 100, 150, 200, 500)
  impacts <- c(0, 0.1, 0.5, 1)
  shifts <- c(50, 150, 300)
  
  for (a in ages) {
    for (i in impacts) {
      for (sh in shifts) {
        res <- mature_mortality(age_x=a, age_impact_val=i, mortality_age_shiftch=sh)
        expect_true(is.numeric(res), info=paste("age:", a, "impact:", i, "shift:", sh))
        expect_true(res >= 0 && res <= 1 + 1e-6, info=paste("age:", a, "impact:", i, "shift:", sh, "res:", res))
      }
    }
  }
})
