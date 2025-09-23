## Function - Mortality death rate _ Hill Function

######################################################################################

Hill_eqn <- function(age, steepness, mortality_age_shiftch, flip) {
  K <- mortality_age_shiftch / (((steepness - 1) / (steepness + 1))^(1 / steepness))
  y <- if(flip){0.75*K^steepness / (K^steepness + age^steepness)} else {age^steepness / (K^steepness + age^steepness)}
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

######################################################################################
library(tidyverse)  

# Age death
  ages <- seq(1:200)
  MR <- runif(n=200)
  mortality_age_shiftch=150
  MR_age_impact_val=12
  
  age_mortality_chance_all=NULL
  
  for(age_impact_value in c(0.2, 0.4, 0.6, 0.8, 1.0)) {
    
    age_mortality_chance <- numeric(length(ages))
    
    for (i in seq_along(ages)) {
      x <- ages[i]
      if (x <= mortality_age_shiftch/2) {
        age_mortality_chance[i] <- young_mortality(x, age_impact_value, mortality_age_shiftch)
      } else {
        age_mortality_chance[i] <- mature_mortality(x, age_impact_value, mortality_age_shiftch)
      }
    }
    
    age_mortality_chance[age_mortality_chance > 1] <- 1 # remove anomaly high and low vals
    age_mortality_chance[age_mortality_chance < 0] <- 0 # remove anomaly high and low vals
    
    age_mortality_chance_all <- data.frame(rbind(age_mortality_chance_all , cbind(age_mortality_chance=age_mortality_chance, age_impact_value=age_impact_value)))
    
  }
  age_mortality_chance_all <- data.frame(cbind(ages=rep(ages, 5), age_mortality_chance_all))
  
  ## Plot age
  age_plot <- ggplot() + 
    geom_line(data=age_mortality_chance_all, aes(x=ages, y=age_mortality_chance, colour=as.character(age_impact_value))) + #
    theme_bw() + 
    scale_color_manual(
      values = hcl.colors(
        palette = "Burg", 
        n = length(unique(as.character(age_mortality_chance_all$age_impact_value))))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y="Age imparted mortality chance", x="Age", colour="Age impact value")
  
  ggsave(age_plot, file="Plots/SI plots/Mortality_age_impact_value.jpeg", width = 1700, height=1600, units="px", limitsize=F)
  
  #########################
  
  # MR chance by death
  MR_mortality_chance_all=NULL
  for(MR_death_impact_val in c(0.2, 0.4, 0.6, 0.8, 1.0)) {
    ages=50
    MR_chance <- numeric(length(MR))
    
    if(!(MR_age_impact_val==0)){
      MR_chance <- (1 / (1 + (ages / MR_age_impact_val))) * MR * MR_death_impact_val
    } else {
      MR_chance <- MR * MR_death_impact_val
    }
    
    
    MR_chance[MR_chance > 1] <- 1; MR_chance[MR_chance < 0] <- 0 # remove anomaly high and low vals
    
    MR_mortality_chance_all <- data.frame(rbind(MR_mortality_chance_all , cbind(MR_chance=MR_chance, MR_impact_value=MR_death_impact_val)))
    
  }
  
  MR_mortality_chance_all <- data.frame(cbind(MR=rep(MR, 5), MR_mortality_chance_all))
  
  ## Plot MR
  MR_plot <- ggplot() + 
    geom_line(data=MR_mortality_chance_all, aes(x=MR, y=MR_chance, colour=as.character(MR_impact_value))) +
    #stat_smooth(data=MR_mortality_chance_all, aes(x=MR, y=MR_chance, colour=as.character(MR_impact_value)), se=F, method='lm', linetype='dashed') +
    theme_bw() + 
    scale_color_manual(
      values = hcl.colors(
        palette = "Burg", 
        n = length(unique(as.character(MR_mortality_chance_all$MR_impact_value))))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y="MR imparted mortality chance", x="Myrtle rust", colour="MR impact value")
  
  ggsave(MR_plot, file="Plots/SI plots/Mortality_MR_impact_value.jpeg", width = 1700, height=1600, units="px", limitsize=F)
    
  
  
 ########################################## Total impact
  
  results_all <- data.frame(
    MR_death_impact_val = numeric(),
    age = numeric(),
    age_impact_value = numeric(),
    MR = numeric(),
    age_mortality_chance = numeric(),
    MR_chance = numeric()
  )
  
  for (MR_death_impact_val in c(0.3, 0.9)) {
    for (age in seq(0, 200, 10)) {
      for (age_impact_value in c(0.3, 0.9)) {
        
        # Compute age mortality
        if (age <= mortality_age_shiftch/2) {
          age_chance <- young_mortality(age, age_impact_value, mortality_age_shiftch)
        } else {
          age_chance <- mature_mortality(age, age_impact_value, mortality_age_shiftch)
        }
        age_chance <- max(0, min(1, age_chance))  # clamp
        
        for (MR in seq(0, 1, 0.1)) {
          # Compute MR chance
          if (MR_age_impact_val != 0) {
            MR_chance <- (1 / (1 + (age / MR_age_impact_val))) * MR * MR_death_impact_val
          } else {
            MR_chance <- MR * MR_death_impact_val
          }
          MR_chance <- max(0, min(1, MR_chance))  # clamp
          
          # Append one row to master dataset
          results_all <- rbind(
            results_all,
            data.frame(
              MR_death_impact_val = MR_death_impact_val,
              age = age,
              age_impact_value = age_impact_value,
              MR = MR,
              age_mortality_chance = age_chance,
              MR_chance = MR_chance
            )
          )
        }
      }
    }
  }
  #beepr::beep(sound = 13)
  
  
  results_all <- results_all %>% 
    mutate(final_mortality_chance_norm=MR_chance + age_mortality_chance - (MR_chance*age_mortality_chance))
  
  results_all$MR <- factor(results_all$MR)   # make MR discrete
  
  both_plot <-  ggplot(results_all, aes(x=age, y=final_mortality_chance_norm, group=MR, colour=MR)) +
    geom_line() +
    theme_bw()  +
    scale_color_manual(
      values = hcl.colors(
        palette = "Burg", 
        n = length(unique(results_all$MR)))) +
    facet_wrap(~ age_impact_value + MR_death_impact_val, labeller = label_both) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y = "Total mortality chance", x = "Age", colour = "MR")
  

  ggsave(both_plot, file="Plots/SI plots/Mortality_MR_age_impact_value.jpeg", width = 2500, height=2700, units="px", limitsize=F)
  


