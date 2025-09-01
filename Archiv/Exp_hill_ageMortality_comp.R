Hill_eqn <- function(age, steepness, mortality_age_shiftch, flip) {
  K <- mortality_age_shiftch / (((steepness - 1) / (steepness + 1))^(1 / steepness))
  y <- if(flip){0.7*K^steepness / (K^steepness + age^steepness)} else {age^steepness / (K^steepness + age^steepness)}
  return(y)
}
young_mortality <- function(age_x, age_impact_val, mortality_age_shiftch){
  # Mortality chance for individuals <age_impact_val
  death_perc <- Hill_eqn(age=age_x, mortality_age_shiftch=2, steepness = 2, flip=TRUE) * age_impact_val # Decreasing death with age
  return(death_perc)
}


mature_mortality <- function(age_x, age_impact_val, mortality_age_shiftch){
  # Mortality chance for individuals >=age_impact_val
  death_perc <- Hill_eqn(age=age_x, mortality_age_shiftch=mortality_age_shiftch, steepness = 15, flip=FALSE) * age_impact_val# Rising death with age
  return(death_perc)
}


young_mortality_old <- function(age_x, age_impact_val){
  #death_young_base <- ifelse(age_impact_val == 1, 0.6, 1 - 0.4 * age_impact_val)
  # Mortality chance for individuals <age_impact_val
  death_perc <- ((1-0.4) / exp((age_x-1)/6)) * age_impact_val # Decreasing death with age
}

mature_mortality_old <- function(age_x, age_impact_val, mortality_age_shiftch){
  # Mortality chance for individuals >=age_impact_val
  death_perc <- exp((age_x - mortality_age_shiftch*2)/20) * age_impact_val # Rising death with age
  #death_perc <- abs(rnorm(n = length(age_x), mean = age_impact_val, sd = age_impact_val*2)) * (exp(((age_x - mortality_age_shiftch)-5/age_x - mortality_age_shiftch)))  # Rising death with age
}

age_mortality_chance_new <- numeric(length(ages))

for (i in seq_along(ages)) {
  x <- ages[i]
  if (x <= mortality_age_shiftch) {
    age_mortality_chance_new[i] <- young_mortality(x, age_impact_val,mortality_age_shiftch)
  } else {
    age_mortality_chance_new[i] <- mature_mortality(x, age_impact_val, mortality_age_shiftch)
  }
}

age_mortality_chance_old <- numeric(length(ages))

for (i in seq_along(ages)) {
  x <- ages[i]
  if (x <= mortality_age_shiftch) {
    age_mortality_chance_old[i] <- young_mortality_old(x, age_impact_val)
  } else {
    age_mortality_chance_old[i] <- mature_mortality_old(x, age_impact_val, mortality_age_shiftch)
  }
}

age_mortality_chance_old[age_mortality_chance_old > 1] <- 1 # remove anomaly high and low vals
age_mortality_chance_old[age_mortality_chance_old < 0] <- 0 # remove anomaly high and low vals

ggplot() + geom_point(aes(x=ages, y=age_mortality_chance), colour='purple') + geom_point(aes(x=ages,y=age_mortality_chance_old), colour='red')


#################

age_mortality_chance_young <- numeric(length(ages))

for (i in seq_along(ages)) {
  x <- ages[i]
  age_mortality_chance_young[i] <- young_mortality(x, age_impact_val,mortality_age_shiftch)
  }

age_mortality_chance_mature <- numeric(length(ages))

for (i in seq_along(ages)) {
  x <- ages[i]
  age_mortality_chance_mature[i] <- mature_mortality(x, age_impact_val,mortality_age_shiftch)
}

ggplot() + geom_point(aes(x=ages, y=age_mortality_chance_young), colour='purple') + geom_point(aes(x=ages,y=age_mortality_chance_mature), colour='red')

ggplot() + geom_point(aes(x=ages, y=age_mortality_chance), colour='purple') + geom_point(aes(x=ages,y=age_mortality_chance_old), colour='red')                                                                                  