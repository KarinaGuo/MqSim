## Function - Mortality death rate

## Population age is impacted by age in a logistic function

## Inputs
# pop - Population
# age_impact_val - impact of age ond eath (multiplier)
# mortality_age_shift - at what age does increases in age increase chance of death
# comp_togg - Turn on and off the competition  affect
# comp_impact_val - impact of competion on death (multiplier)
# MR_togg - Turn on and off the Myrtle rust affect
# MR_death_impact_val - impact of Myrtle rust susceptibility on death (multiplier)
######################################################################################
young_mortality <- function(age_x, age_impact_val){
  # Mortality chance for individuals <10
  death_perc <- abs(rnorm(n = length(age_x), mean = age_impact_val)) * (1 / (1 + exp((age_x - 10)/2)))  # Decreasing death with age
}

mature_mortality <- function(age_x, age_impact_val){
  # Mortality chance for individuals >=10
  death_perc <- abs(rnorm(n = length(age_x), mean = age_impact_val)) * (1 / (1 + exp(-(age_x - 20)/5)))  # Rising death with age
}

######################################################################################
mortality_death_rate  <- function(pop, comp_togg, comp_impact_val, MR_togg, MR_death_impact_val, age_impact_val, mortality_age_shiftch){
  
  require(scales)
  # Age death
  ages <- pop$age
  age_mortality_chance <- numeric(length(ages))
  for (i in seq_along(ages)) {
    x <- ages[i]
    if (x < mortality_age_shiftch) {
      age_mortality_chance[i] <- young_mortality(x, age_impact_val)
    } else {
      age_mortality_chance[i] <- mature_mortality(x, age_impact_val)
    }
  }
  
  # If both toggle is off 
  if (!MR_togg & !comp_togg)  { # If MR toggle is on but not competition

    final_mortality_chance_norm <- rescale(age_mortality_chance, c(0,1))
  }
  
  # If MR toggle is on but not competition
  if (MR_togg & !comp_togg)  { # If MR toggle is on but not competition
    MR <- pop$MR
    MR_chance <- MR_death_impact_val * (MR)
    
    final_mortality_chance_norm <- rescale(MR_chance+age_mortality_chance, c(0,1))
  }
  
  # If compeititon toggle is on but not MR
  if (comp_togg & !MR_togg) {
    pop_size = length(pop$indiv_id)
    scaled_compimpact = log10(pop_size) * comp_impact_val # Scale competition impact by population size
    comp_chance <- rnorm(n = 1, mean = scaled_compimpact)
    
    final_mortality_chance_norm <- rescale(comp_chance+age_mortality_chance, c(0,1))
  }
  
  # If both MR and compeititon toggle is on
  if (comp_togg & MR_imp) {
    MR <- pop$MR
    MR_chance <- MR_death_impact_val * (MR)
    
    pop_size = length(pop$indiv_id)
    scaled_compimpact = log10(pop_size) * comp_impact_val # Scale competition impact by population size
    comp_chance <- rnorm(n = 1, mean = scaled_compimpact)
    
    final_mortality_chance_norm <- rescale(MR_chance+comp_chance+age_mortality_chance, c(0,1))
  }
  
  # Mortality
  mortality_base <- rbinom(n = length(final_mortality_chance_norm), size = 1, prob = final_mortality_chance_norm)
  return(mortality_base)
}