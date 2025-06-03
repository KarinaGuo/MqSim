set.seed(12345)
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")

## Load in libraries
library(tidyverse)

## Load in parameters
source("configurations")
source("Functions/mortality_functions.R")

## Initiate random population
init_MR <- rnorm(n=population_size, mean = MR_mean, sd = MR_sd); init_MR[init_MR<0]=0
init_age <- rep(1, population_size)
indiv_ID <- seq(from=1, to=population_size)

pop_df <- list(
  indiv_ID = indiv_ID,
  time = rep(0, length(indiv_ID)),
  MR = init_MR,
  mortality = rep(0, length(indiv_ID)),
  age = init_age
)


## Initialise objects
indiv_pop=NULL; MR_pop=NULL; curr_pop=NULL; new_recruit_pop=NULL; indiv_count=0; death_df=NULL

for (time_point in 1:time_max){
  timepoint_count=timepoint_count+1
  
  if(time_point==1){
    curr_pop <- pop_df
  } 
  
  str(curr_pop)
  
  if((length(curr_pop$indiv_ID)==0)) {
    stop("All dead at time ", time_point, "\n")
  } else{
    
    indiv_count=length(unique(curr_pop$indiv_ID)) + indiv_count
    indiv_alive_count=nrow(curr_pop$indiv_ID)
    
    ## Recruitment
    
    recruitment_indivs <- curr_pop$indiv_ID[curr_pop$age > recruitment_age]
    recruitment_chance <- ifelse(is.null(nrow(recruitment_indivs)), 0, (nrow(recruitment_indivs)))
    indiv_recruitment <- rbinom(n = recruitment_chance, size = 1, prob = recruitment_constant)
    
    if (sum(indiv_recruitment)>0){
      recruitment_indiv_MR <- recruitment_indivs[indiv_recruitment,]$MR
      
      new_recruit <- rnorm(n=sum(indiv_recruitment), mean = recruitment_size_mean, sd = recruitment_size_sd); new_recruit[new_recruit<0]=0; sum(ceiling(new_recruit))
      new_recruit_MR <- rnorm(n=ceiling(length(new_recruit)), mean = recruitment_indiv_MR, sd = MR_sd/2); new_recruit_MR[new_recruit_MR<0]=0
      
      new_recruit_pop <- data.frame(indiv_ID=seq(from=indiv_count+1, to=indiv_count+ceiling(length(new_recruit))), MR=new_recruit_MR, time=time_point, mortality=0, age=1)
      
      # curr_pop <- rbind(curr_pop, pop_df_simres_tp, by='indiv_ID')
      curr_pop <- rbind(do.call(append, curr_pop, new_recruit_pop))
      
    }
      
    indiv_death <- mortality_death_rate(pop=curr_pop, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact, age_impact_val=age_impact, mortality_age_shiftch=mortality_age_shift)
    
    death_df_curr <- data.frame(Dead_ID=curr_pop$indiv_ID[as.logical(indiv_death)], age=curr_pop$age[as.logical(indiv_death)], MR=curr_pop$MR[as.logical(indiv_death)], time=curr_pop$time[as.logical(indiv_death)])
    death_df <- rbind(death_df, death_df_curr)
    
    curr_pop <- list(indiv_ID=curr_pop$indiv_ID[!as.logical(indiv_death)], age=curr_pop$age[!as.logical(indiv_death)]+1, MR=curr_pop$MR[!as.logical(indiv_death)], time=curr_pop$time[!as.logical(indiv_death)]+1)
    
    # curr_pop <- rbind(curr_pop, pop_df_simres_tp, by='indiv_ID')
    # indiv_pop <- as.data.frame(rbind(indiv_pop, cbind(time=time_point, indiv_alive=indiv_alive_count)) )
    # MR_pop <- as.data.frame(rbind(MR_pop, cbind(time=time_point, MR_currmean=MR_currmean)) )
    #   
    }
  }

plot(MR_pop)
plot(indiv_pop)
