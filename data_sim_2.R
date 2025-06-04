set.seed(12345)
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")

## Load in libraries
library(tidyverse)

## Load in parameters
source("configurations")
source("Functions/mortality_functions.R")
source("Functions/recruitment_functions.R")

## Initiate random population
init_MR <- rnorm(n=population_size, mean = MR_mean, sd = MR_sd); init_MR[init_MR<0]=0
init_age <- as.integer(runif(n=population_size, min=1, max=100))
indiv_ID <- seq(from=1, to=population_size)

pop_df <- list(
  indiv_ID = indiv_ID,
  time = rep(1, length(indiv_ID)),
  MR = init_MR,
  mortality = rep(0, length(indiv_ID)),
  age = init_age
)


## Initialise objects
indiv_pop=NULL; MR_pop=NULL; curr_pop=NULL; new_recruit_pop=NULL; indiv_count=0; death_df=NULL; age_df=NULL; MR_df=NULL; live_size_df=NULL

for (time_point in 1:time_max){
  
  if(time_point==1){
    curr_pop <- pop_df
  } else {
    curr_pop <- lapply(curr_pop_end, function(x) x[curr_pop_end$time==time_point])
  }
  
  if((length(curr_pop$indiv_ID)==0)) {
    stop("All dead at time ", time_point, "\n")
  } else{
    
    indiv_count=length(unique(curr_pop$indiv_ID)) + indiv_count
    indiv_alive_count=nrow(curr_pop$indiv_ID)
    
    ## Recruitment
    
    curr_pop <- recruit_rate(pop=curr_pop, recruitment_age=recruitment_age, population_min_size=population_minimum_size, recruitment_size_mean=recruitment_mean, recruitment_size_sd=recruitment_sd, recruitment_constant=recruitment_const, MR_togg=MR_recruit_imp, MR_recruit_impact_val=MR_recruit_impact)
    indiv_death <- mortality_death_rate(pop=curr_pop, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact, MR_age_impact_val=MR_age_impact, age_impact_val=age_impact, mortality_age_shiftch=mortality_age_shift)
    
    death_df_curr <- data.frame(Dead_ID=curr_pop$indiv_ID[as.logical(indiv_death)], age=curr_pop$age[as.logical(indiv_death)], MR=curr_pop$MR[as.logical(indiv_death)], time=curr_pop$time[as.logical(indiv_death)])
    death_df <- rbind(death_df, death_df_curr)
    
    # Generate summary data if not dead
    if (length(curr_pop$indiv_ID)>sum(indiv_death)){ 
      MR_summ <- data.frame(time=time_point, MR_mean_summ=mean(curr_pop$MR[!as.logical(indiv_death)], na.rm=TRUE), MR_sd_summ=sd(curr_pop$MR[!as.logical(indiv_death)]))
      MR_df <- rbind(MR_df, MR_summ)
      
      age_summ <- data.frame(time=time_point, age_mean_summ=mean(curr_pop$age[!as.logical(indiv_death)]), age_sd_summ=sd(curr_pop$age[!as.logical(indiv_death)]),pop_size=length(curr_pop$indiv_ID))
      age_df <- rbind(age_df, age_summ) 
    }
    
    live_size <- data.frame(time=time_point, sum_size=length(curr_pop$indiv_ID[!as.logical(indiv_death)]))
    live_size_df <- rbind(live_size_df, live_size) 
    
    curr_pop_end <- list(indiv_ID=curr_pop$indiv_ID[!as.logical(indiv_death)], age=curr_pop$age[!as.logical(indiv_death)]+1, MR=curr_pop$MR[!as.logical(indiv_death)], time=curr_pop$time[!as.logical(indiv_death)]+1)
    
    # Verbose 🗣️
    if(time_point%%output_timept == 0){ 
      
      cat("Time at:", time_point,"\n",
          "Individuals alive:", length(curr_pop_end$indiv_ID), "\n",
          "Mean MR of live individuals:", mean(curr_pop$MR[!as.logical(indiv_death)]), "\n")
      
      # Plots
      print(ggplot() + geom_histogram(data=data.frame(curr_pop_end), aes(x=age, y = after_stat(density)), binwidth=10) + theme_bw() + labs(title=paste("Age density at", time_point)))
      print(ggplot() + geom_point(data=data.frame(curr_pop_end), aes(x=age, y=MR)) + theme_bw() + labs(title=paste("MR by age at", time_point)))
      
    }
    }
  }

# Post-run plots

mean_MR_time_death <- death_df %>% group_by(time) %>% summarise(mean_MR=mean(MR), sd_MR=sd(MR))

plot_livesize <- ggplot() + geom_point(data=live_size_df, aes(x=time, y=sum_size)) + labs(title="Live population size")
plot_liveage  <- ggplot() + 
  geom_point(data=age_df, aes(x=time, y=age_mean_summ)) +
  geom_errorbar(data=age_df, aes(x=time, ymax = age_mean_summ + age_sd_summ, ymin = age_mean_summ - age_sd_summ)) + labs(title="Live age")

plot_deadMR   <- ggplot(mean_MR_time_death, aes(x=time, y=mean_MR)) + geom_point() + labs(title="Death MR")
plot_liveMR   <- ggplot()  + geom_errorbar(data=MR_df, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ)) + labs(title="Live MR")

library(patchwork)
plot_livesize / plot_liveage
plot_deadMR / plot_liveMR
