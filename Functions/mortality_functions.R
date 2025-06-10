## Function - Mortality death rate

## Inputs
# pop - Population
# population_capacity - Maximum population size until 'comp' is used as punishment 🔨

# age_impact_val - impact of age ond eath (multiplier)
# mortality_age_shiftch - at what age does increases in age increase chance of death

# comp_togg - Turn on and off the competition  affect
# comp_impact_val - impact of competion on death (multiplier)

# MR_togg - Turn on and off the Myrtle rust affect
# MR_death_impact_val - impact of Myrtle rust susceptibility on death (multiplier)
# MR_age_impact_val - scaled impact of age on MR inflicted death increase


######################################################################################
young_mortality <- function(age_x, age_impact_val){
  # Mortality chance for individuals <age_impact_val
  death_perc <- (1 / exp((age_x-1)/5)) * age_impact_val # Decreasing death with age
}

mature_mortality <- function(age_x, age_impact_val, mortality_age_shiftch){
  # Mortality chance for individuals >=age_impact_val
  death_perc <- exp((age_x - mortality_age_shiftch*2)/20) * age_impact_val # Rising death with age
  #death_perc <- abs(rnorm(n = length(age_x), mean = age_impact_val, sd = age_impact_val*2)) * (exp(((age_x - mortality_age_shiftch)-5/age_x - mortality_age_shiftch)))  # Rising death with age
}

######################################################################################
mortality_death_rate  <- function(pop, population_capacity, population_min_size, comp_togg, comp_impact_val, MR_togg, MR_death_impact_val, MR_age_impact_val, age_impact_val, mortality_age_shiftch){
  
  require(scales)
  # Age death
  ages <- pop$age
  age_mortality_chance <- numeric(length(ages))
  for (i in seq_along(ages)) {
    x <- ages[i]
    if (x <= mortality_age_shiftch) {
      age_mortality_chance[i] <- young_mortality(x, age_impact_val)
    } else {
      age_mortality_chance[i] <- mature_mortality(x, age_impact_val, mortality_age_shiftch)
    }
  }
  
  age_mortality_chance[age_mortality_chance > 1] <- 1 # remove anomaly high and low vals
  age_mortality_chance[age_mortality_chance < 0] <- 0 # remove anomaly high and low vals
  
  # MR chance by death
  MR <- pop$MR
  MR_chance <- (ages+MR_age_impact_val)/(ages) * MR * MR_death_impact_val; MR_chance <- rescale(MR_chance, to = c(0,1))

  
  # If both toggle is off 
  if (!MR_togg & !comp_togg)  { # If MR toggle is on but not competition
    final_mortality_chance_norm <- rescale(age_mortality_chance, to=c(0,1))
  }
  
  # If MR toggle is on but not competition
  if (MR_togg & !comp_togg)  { # If MR toggle is on but not competition
    final_mortality_chance_norm <- rescale(MR_chance+age_mortality_chance, c(0,1))
  }
  
  # If compeititon toggle is on but not MR
  if (comp_togg & !MR_togg) {
    pop_size = length(pop$indiv_ID)
    
    if(pop_size > population_capacity){
      comp_chance = (pop_size - population_capacity)/pop_size  * 1+comp_impact_val  # Scale competition impact by how much over carrying capacity of population size
      final_mortality_chance_norm <- rescale(age_mortality_chance, c(((pop_size - population_capacity)/pop_size),1))
    } else {
      comp_chance=0
      final_mortality_chance_norm <- rescale(age_mortality_chance, c(0,1)) 
    }
    
    
  }
  
  # If both MR and compeititon toggle is on
  if (comp_togg & MR_togg) {
    pop_size = length(pop$indiv_ID)
    
    if(pop_size > population_capacity){
      comp_chance = (pop_size - population_capacity)/pop_size *  1+comp_impact_val  # Scale competition impact by how much over carrying capacity of population size
      final_mortality_chance_norm <- rescale((age_mortality_chance + MR_chance), c(((pop_size - population_capacity)/pop_size),1)) # overrides
    } else {
      comp_chance=0
      final_mortality_chance_norm <- rescale(age_mortality_chance + MR_chance, c(0,1)) # overrides
    }
    
  }
  
  # Mortality
  mortality_base <- rbinom(n = length(final_mortality_chance_norm), size = 1, prob = final_mortality_chance_norm)
  return(mortality_base)
}
