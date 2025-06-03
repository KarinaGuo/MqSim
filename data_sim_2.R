set.seed(12345)
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")

## Load in libraries
library(tidyverse)

## Load in parameters
source("configurations")
source("Functions/mortality_functions.R")

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
indiv_pop=NULL; MR_pop=NULL; curr_pop=NULL; new_recruit_pop=NULL; indiv_count=0; death_df=NULL; age_df=NULL; MR_df=NULL

for (time_point in 1:time_max){
  
  if(time_point==1){
    curr_pop <- pop_df
  } else {
    curr_pop <- lapply(curr_pop_end, function(x) x[curr_pop_end$time==time_point])
  }
  
  if((length(curr_pop$indiv_ID)==0)) {
    stop("All dead at time ", time_point, "\n")
  } else{
    
    cat("Number starting", length(curr_pop$indiv_ID), "during:", time_point, "\n")
    
    indiv_count=length(unique(curr_pop$indiv_ID)) + indiv_count
    indiv_alive_count=nrow(curr_pop$indiv_ID)
    
    ## Recruitment
    
    recruitment_indivs <- lapply(curr_pop, function(x) x[curr_pop$age > recruitment_age])
    indiv_recruitment <- rbinom(n = length(recruitment_indivs$indiv_ID), size = 1, prob = recruitment_constant)
    
    if (sum(indiv_recruitment)>0){
      recruitment_indiv_MR <- recruitment_indivs$MR[as.logical(indiv_recruitment)]
      
      new_recruit <- as.integer(rnorm(n=sum(indiv_recruitment), mean = recruitment_size_mean, sd = recruitment_size_sd)); new_recruit[new_recruit<0]=0
      
      new_recruit_MR=NULL
      for (i in 1:length(recruitment_indiv_MR)) {
        new_recruit_MR_new <- rnorm(n=new_recruit[i], mean=recruitment_indiv_MR[i], sd = MR_sd) # For each new recruit, use parent phenotype to generate MR
        new_recruit_MR <- append(new_recruit_MR, new_recruit_MR_new)
      }
      
      new_recruit_MR[new_recruit_MR<0]=0
      
      new_recruit_pop <- list(indiv_ID=seq(from=indiv_count+1, to=indiv_count+sum(new_recruit)), 
                              time=rep(time_point, sum(new_recruit)), 
                              MR=new_recruit_MR, 
                              mortality=rep(0, sum(new_recruit)), 
                              age=rep(1, sum(new_recruit)))
      
      cat("Recruited:", sum(new_recruit), "\n")
      
      # curr_pop <- rbind(curr_pop, pop_df_simres_tp, by='indiv_ID')
      curr_pop <- list(indiv_ID=c(curr_pop$indiv_ID, new_recruit_pop$indiv_ID), age=c(curr_pop$age, new_recruit_pop$age), MR=c(curr_pop$MR, new_recruit_pop$MR), time=c(curr_pop$time, new_recruit_pop$time), mortality=c(curr_pop$mortality, new_recruit_pop$mortality))
    }
      
    indiv_death <- mortality_death_rate(pop=curr_pop, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact, age_impact_val=age_impact, mortality_age_shiftch=mortality_age_shift)
    
    cat("Number died", sum(indiv_death), "during:", time_point, "\n")
    
    death_df_curr <- data.frame(Dead_ID=curr_pop$indiv_ID[as.logical(indiv_death)], age=curr_pop$age[as.logical(indiv_death)], MR=curr_pop$MR[as.logical(indiv_death)], time=curr_pop$time[as.logical(indiv_death)])
    death_df <- rbind(death_df, death_df_curr)
    
    # Generate summary data if not dead
    if (length(curr_pop$indiv_ID)>sum(indiv_death)){ 
      MR_summ <- data.frame(time=time_point, MR_mean_summ=mean(curr_pop$MR[!as.logical(indiv_death)], na.rm=TRUE))
      MR_df <- rbind(MR_df, MR_summ)
      
      age_summ <- data.frame(time=time_point, age=curr_pop$age[!as.logical(indiv_death)], pop_size=length(curr_pop$indiv_ID))
      age_df <- rbind(age_df, age_summ) 
    }
    
    curr_pop_end <- list(indiv_ID=curr_pop$indiv_ID[!as.logical(indiv_death)], age=curr_pop$age[!as.logical(indiv_death)]+1, MR=curr_pop$MR[!as.logical(indiv_death)], time=curr_pop$time[!as.logical(indiv_death)]+1)
    cat("Number remaining", length(curr_pop_end$indiv_ID), "during:", time_point, "\n")
    }
  }

mean_MR_time_death <- death_df %>% group_by(time) %>% summarise(mean_MR=mean(MR), sd_MR=sd(MR))
mean_age_time_live <- age_df %>% group_by(time) %>% summarise(mean_age=mean(age), sd_MR=sd(age))

plot_livesize <- ggplot(age_df, aes(x=time, y=pop_size, group=time)) + geom_point() + labs(title="Live population size")
plot_deadMR <- ggplot(mean_MR_time_death, aes(x=time, y=mean_MR)) + geom_point() + labs(title="Death MR")
plot_liveage <- ggplot(age_df, aes(x=time, y=age, group=time)) + geom_boxplot() + labs(title="Live age")
plot_liveMR <- ggplot(MR_df, aes(x=time, y=MR_mean_summ)) + geom_point() + labs(title="Live MR")


library(patchwork)
plot_livesize / plot_liveage
plot_deadMR / plot_liveMR
