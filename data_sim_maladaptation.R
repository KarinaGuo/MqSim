# running data_sim_2 with maladaptation
  # - run two populations in parallel
  # - rewrite into pop 1 and pop 2
    # = at each time point if pop 1 individual recruits, give it a pop2 / pop1+pop2 chance of recruiting a score of mean (parent 1 + mean(pop2 MR))

set.seed(12345)
rm(list = ls())
setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Simulation/")

## Load in libraries
library(tidyverse)

## Load in parameters
source("configurations_maladaptation")
source("Functions/mortality_functions_MRintro.R")
source("Functions/mortality_functions.R")
source("Functions/recruitment_functions_maladaptation.R")
source("Functions/disturbance_functions.R")


## Initiate random population
### Pop 1
init_MR_pop1 <- rnorm(n=population_size, mean = MR_mean_pop1, sd = MR_sd_pop1); init_MR_pop1[init_MR_pop1<0]=0
init_age_pop1 <- as.integer(runif(n=population_size, min=1, max=100))
indiv_ID_pop1 <- seq(from=1, to=population_size)

pop_df_pop1 <- list(
  indiv_ID_pop1 = indiv_ID_pop1,
  time = rep(1, length(indiv_ID_pop1)),
  MR = init_MR_pop1,
  mortality = rep(0, length(indiv_ID_pop1)),
  age = init_age_pop1
)

indiv_pop_pop1=NULL 
MR_pop_pop1=NULL 
curr_pop_pop1=NULL 
new_recruit_pop1=NULL 
indiv_count_pop1=0 
death_df_pop1=NULL 
age_df_pop1=NULL
MR_df_pop1=NULL 
live_size_df_pop1=NULL
pop_timepoints_pop1=NULL
dist_event_pop1=FALSE

### Pop2
init_MR_pop2 <- rnorm(n=population_size, mean = MR_mean_pop2, sd = MR_sd_pop2); init_MR_pop2[init_MR_pop2<0]=0
init_age_pop2 <- as.integer(runif(n=population_size, min=1, max=100))
indiv_ID_pop2 <- seq(from=1, to=population_size)

pop_df_pop2 <- list(
  indiv_ID_pop2 = indiv_ID_pop2,
  time = rep(1, length(indiv_ID_pop2)),
  MR = init_MR_pop2,
  mortality = rep(0, length(indiv_ID_pop2)),
  age = init_age_pop2
)

indiv_pop_pop2=NULL 
MR_pop_pop2=NULL 
curr_pop_pop2=NULL 
new_recruit_pop2=NULL 
indiv_count_pop2=0 
death_df_pop2=NULL 
age_df_pop2=NULL
MR_df_pop2=NULL 
live_size_df_pop2=NULL
pop_timepoints_pop2=NULL
dist_event_pop2=FALSE



for (time_point in 1:time_max){
  
  if(time_point==1){
    curr_pop_pop1 <- pop_df_pop1
    curr_pop_pop2 <- pop_df_pop2
  } else {
    curr_pop_pop1 <- lapply(curr_pop_end_pop1, function(x) x[curr_pop_end_pop1$time==time_point])
    curr_pop_pop2 <- lapply(curr_pop_end_pop2, function(x) x[curr_pop_end_pop2$time==time_point])
  }
  
  if((length(curr_pop_pop1$indiv_ID)==0) | (length(curr_pop_pop2$indiv_ID)==0)) {
    stop("Population dead at time ", time_point, "\n")
  } else{
    
    ## Disturbance
    disturbance_event_pop1 <- disturbance_event_chance (dist_togg = dist_imp, disturbance_age_struct = disturbance_age_struct_type, dist_impact_val = dist_impact, dist_age_impact_val = dist_age_impact, pop=curr_pop_pop1, age_imp=age_impact_pop1, MR_death_imp=MR_death_impact_pop1)
    disturbance_event_res_pop1 <- disturbance_event_pop1[1]; age_impact_pop1 = disturbance_event_pop1[2]; MR_death_impact_pop1 = disturbance_event_pop1[3]; recruitment_const_pop1 = disturbance_event_pop1[4]
    
    disturbance_event_pop2 <- disturbance_event_chance (dist_togg = dist_imp, disturbance_age_struct = disturbance_age_struct_type, dist_impact_val = dist_impact, dist_age_impact_val = dist_age_impact, pop=curr_pop_pop2, age_imp=age_impact_pop2, MR_death_imp=MR_death_impact_pop2)
    disturbance_event_res_pop2 <- disturbance_event_pop2[1]; age_impact_pop2 = disturbance_event_pop2[2]; MR_death_impact_pop2 = disturbance_event_pop2[3]; recruitment_const_pop2 = disturbance_event_pop2[4]
    
      
    indiv_count_pop1=length(unique(curr_pop_pop1$indiv_ID)) + indiv_count_pop1
    indiv_alive_count_pop1=nrow(curr_pop_pop1$indiv_ID)
    
    indiv_count_pop2=length(unique(curr_pop_pop2$indiv_ID)) + indiv_count_pop2
    indiv_alive_count_pop2=nrow(curr_pop_pop2$indiv_ID)
    
    ## Mortality
    
    if (time_point>=MR_timepoint & MR_lateintro){
      indiv_death_pop1 <- mortality_death_rate_MRlate(pop=curr_pop_pop1, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_death_impact_val=MR_death_impact_pop1, MR_age_impact_val=MR_age_impact_pop1, age_impact_val=age_impact_pop1, mortality_age_shiftch=mortality_age_shift, MR_intro=MR_lateintro, MR_intro_timepoint=MR_timepoint)
      if(time_point==MR_timepoint){cat("Using mortality_death_rate_MRlate \n")}
    } else if (time_point<MR_timepoint & MR_lateintro){
      MR_death_impact_beforeintro=0
      indiv_death_pop1 <- mortality_death_rate(pop=curr_pop_pop1, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact_beforeintro, MR_age_impact_val=MR_age_impact_pop1, age_impact_val=age_impact_pop1, mortality_age_shiftch=mortality_age_shift)
      if(time_point==1){cat("Using mortality_death_rate & MR before imp \n")}
    } else if (!MR_lateintro) {
      indiv_death_pop1 <- mortality_death_rate(pop=curr_pop_pop1, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact_pop1, MR_age_impact_val=MR_age_impact_pop1, age_impact_val=age_impact_pop1, mortality_age_shiftch=mortality_age_shift)
      if(time_point==1){cat("Using mortality_death_rate \n")}
    }
    
    if (time_point>=MR_timepoint & MR_lateintro){
      indiv_death_pop2 <- mortality_death_rate_MRlate(pop=curr_pop_pop2, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_death_impact_val=MR_death_impact_pop2, MR_age_impact_val=MR_age_impact_pop2, age_impact_val=age_impact_pop2, mortality_age_shiftch=mortality_age_shift, MR_intro=MR_lateintro, MR_intro_timepoint=MR_timepoint)
      if(time_point==MR_timepoint){cat("Using mortality_death_rate_MRlate \n")}
    } else if (time_point<MR_timepoint & MR_lateintro){
      MR_death_impact_beforeintro=0
      indiv_death_pop2 <- mortality_death_rate(pop=curr_pop_pop2, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact_beforeintro, MR_age_impact_val=MR_age_impact_pop2, age_impact_val=age_impact_pop2, mortality_age_shiftch=mortality_age_shift)
      if(time_point==1){cat("Using mortality_death_rate & MR before imp \n")}
    } else if (!MR_lateintro) {
      indiv_death_pop2 <- mortality_death_rate(pop=curr_pop_pop2, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact_pop2, MR_age_impact_val=MR_age_impact_pop2, age_impact_val=age_impact_pop2, mortality_age_shiftch=mortality_age_shift)
      if(time_point==1){cat("Using mortality_death_rate \n")}
    }
    
    
    maladaptation_chance_pop1 <- rbinom(n=1, size=1, indiv_count_pop1/(indiv_count_pop1 + indiv_count_pop2))
    maladaptation_chance_pop2 <- rbinom(n=1, size=1, indiv_count_pop2/(indiv_count_pop1 + indiv_count_pop2))
    
    curr_pop_pop1 <- recruit_rate(pop=curr_pop_pop1, indiv_count=indiv_count_pop1, MR_maladapt_pop=curr_pop_pop2$MR, recruitment_age=recruitment_age, population_min_size=population_minimum_size, recruitment_size_mean=recruitment_mean, recruitment_size_sd=recruitment_sd, recruitment_constant=recruitment_const, MR_togg=MR_recruit_imp, MR_recruit_impact_val=MR_recruit_impact, age_imp_rec_togg=age_imp_rec, maladaptation_chance=maladaptation_chance_pop1)
    curr_pop_pop2 <- recruit_rate(pop=curr_pop_pop2, indiv_count=indiv_count_pop2, MR_maladapt_pop=curr_pop_pop1$MR, recruitment_age=recruitment_age, population_min_size=population_minimum_size, recruitment_size_mean=recruitment_mean, recruitment_size_sd=recruitment_sd, recruitment_constant=recruitment_const, MR_togg=MR_recruit_imp, MR_recruit_impact_val=MR_recruit_impact, age_imp_rec_togg=age_imp_rec, maladaptation_chance=maladaptation_chance_pop2)
    
    death_df_curr_pop1 <- data.frame(Dead_ID=curr_pop_pop1$indiv_ID[as.logical(indiv_death_pop1)], age=curr_pop_pop1$age[as.logical(indiv_death_pop1)], MR=curr_pop_pop1$MR[as.logical(indiv_death_pop1)], time=curr_pop_pop1$time[as.logical(indiv_death_pop1)])
    death_df_pop1 <- rbind(death_df_pop1, death_df_curr_pop1)
    
    death_df_curr_pop2 <- data.frame(Dead_ID=curr_pop_pop2$indiv_ID[as.logical(indiv_death_pop2)], age=curr_pop_pop2$age[as.logical(indiv_death_pop2)], MR=curr_pop_pop2$MR[as.logical(indiv_death_pop2)], time=curr_pop_pop2$time[as.logical(indiv_death_pop2)])
    death_df_pop2 <- rbind(death_df_pop2, death_df_curr_pop2)
    
    # Generate summary data if not dead
    if (length(curr_pop_pop1$indiv_ID)>sum(indiv_death_pop1)){ 
      MR_summ_pop1 <- data.frame(time=time_point, MR_mean_summ=mean(curr_pop_pop1$MR[!as.logical(indiv_death_pop1)], na.rm=TRUE), MR_sd_summ=sd(curr_pop_pop1$MR[!as.logical(indiv_death_pop1)]), maladaptation_time=maladaptation_chance_pop1)
      MR_df_pop1 <- rbind(MR_df_pop1, MR_summ_pop1)
      
      age_summ_pop1 <- data.frame(time=time_point, age_mean_summ=mean(curr_pop_pop1$age[!as.logical(indiv_death_pop1)]), age_sd_summ=sd(curr_pop_pop1$age[!as.logical(indiv_death_pop1)]), pop_size=length(curr_pop_pop1$indiv_ID))
      age_df_pop1 <- rbind(age_df_pop1, age_summ_pop1) 
    }
    
    if (length(curr_pop_pop2$indiv_ID)>sum(indiv_death_pop2)){ 
      MR_summ_pop2 <- data.frame(time=time_point, MR_mean_summ=mean(curr_pop_pop2$MR[!as.logical(indiv_death_pop2)], na.rm=TRUE), MR_sd_summ=sd(curr_pop_pop2$MR[!as.logical(indiv_death_pop2)]), maladaptation_time=maladaptation_chance_pop2)
      MR_df_pop2 <- rbind(MR_df_pop2, MR_summ_pop2)
      
      age_summ_pop2 <- data.frame(time=time_point, age_mean_summ=mean(curr_pop_pop2$age[!as.logical(indiv_death_pop2)]), age_sd_summ=sd(curr_pop_pop2$age[!as.logical(indiv_death_pop2)]), pop_size=length(curr_pop_pop2$indiv_ID))
      age_df_pop2 <- rbind(age_df_pop2, age_summ_pop2) 
    }
    
    live_size_pop1 <- data.frame(time=time_point, sum_size=length(curr_pop_pop1$indiv_ID[!as.logical(indiv_death_pop1)]))
    live_size_df_pop1 <- rbind(live_size_df_pop1, live_size_pop1) 
    
    live_size_pop2 <- data.frame(time=time_point, sum_size=length(curr_pop_pop2$indiv_ID[!as.logical(indiv_death_pop2)]))
    live_size_df_pop2 <- rbind(live_size_df_pop2, live_size_pop2) 
    
    curr_pop_end_pop1 <- list(indiv_ID=curr_pop_pop1$indiv_ID[!as.logical(indiv_death_pop1)], age=curr_pop_pop1$age[!as.logical(indiv_death_pop1)]+1, MR=curr_pop_pop1$MR[!as.logical(indiv_death_pop1)], time=curr_pop_pop1$time[!as.logical(indiv_death_pop1)]+1)
    curr_pop_end_pop2 <- list(indiv_ID=curr_pop_pop2$indiv_ID[!as.logical(indiv_death_pop2)], age=curr_pop_pop2$age[!as.logical(indiv_death_pop2)]+1, MR=curr_pop_pop2$MR[!as.logical(indiv_death_pop2)], time=curr_pop_pop2$time[!as.logical(indiv_death_pop2)]+1)
    
    # Save populations at user designated timepoints
    if (!is.null(timepoint_pop_grab) && (time_point %in% timepoint_pop_grab)){
      i <- which(time_point == timepoint_pop_grab)
      pop_timepoints_pop1[[i]] <- curr_pop_end_pop1
      pop_timepoints_pop2[[i]] <- curr_pop_end_pop2
    }
    
    # Return to base
    if (as.logical(disturbance_event_pop1[1])){
      age_impact_pop1 = disturbance_event_pop1[5]
      MR_death_impact_pop1 = disturbance_event_pop1[6]
      recruitment_const = disturbance_event_pop1[7]
    }
    
    if (as.logical(disturbance_event_pop2[1])){
      age_impact_pop2 = disturbance_event_pop2[5]
      MR_death_impact_pop2 = disturbance_event_pop2[6]
      recruitment_const = disturbance_event_pop2[7]
    }
  }
}

# Post-run plots

mean_MR_time_death_pop1 <- death_df_pop1 %>% group_by(time) %>% summarise(mean_MR=mean(MR), sd_MR=sd(MR))
mean_MR_time_death_pop2 <- death_df_pop2 %>% group_by(time) %>% summarise(mean_MR=mean(MR), sd_MR=sd(MR))

plot_livesize_pop1 <- ggplot() + geom_point(data=live_size_df_pop1, aes(x=time, y=sum_size)) + labs(title="Live population size - Population 1")
plot_livesize_pop2 <- ggplot() + geom_point(data=live_size_df_pop2, aes(x=time, y=sum_size)) + labs(title="Live population size - Population 2")

plot_liveage_pop1  <- ggplot() + 
  geom_point(data=age_df_pop1, aes(x=time, y=age_mean_summ)) +
  geom_errorbar(data=age_df_pop1, aes(x=time, ymax = age_mean_summ + age_sd_summ, ymin = age_mean_summ - age_sd_summ)) + labs(title="Live age - Population 1")
plot_liveage_pop2  <- ggplot() + 
  geom_point(data=age_df_pop2, aes(x=time, y=age_mean_summ)) +
  geom_errorbar(data=age_df_pop2, aes(x=time, ymax = age_mean_summ + age_sd_summ, ymin = age_mean_summ - age_sd_summ)) + labs(title="Live age - Population 2")

plot_deadMR_pop1   <- ggplot(mean_MR_time_death_pop1, aes(x=time, y=mean_MR)) + geom_point() + labs(title="Death MR - Population 1")
plot_liveMR_pop1   <- ggplot() +
  geom_point(data=MR_df_pop1, aes(x=time, y = MR_mean_summ, colour=as.character(maladaptation_time))) +
  geom_errorbar(data=MR_df_pop1, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ)) + 
  stat_smooth(data=MR_df_pop1, aes(x=time, y = MR_mean_summ), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  labs(title="Live MR - Population 1") +
  ylim(c(-0.1,1.5)) +
  theme(legend.position = "none")

plot_deadMR_pop2   <- ggplot(mean_MR_time_death_pop2, aes(x=time, y=mean_MR)) + geom_point() + labs(title="Death MR - Population 2")
plot_liveMR_pop2   <- ggplot() +
  geom_point(data=MR_df_pop2, aes(x=time, y = MR_mean_summ, colour=as.character(maladaptation_time))) +
  geom_errorbar(data=MR_df_pop2, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ)) + 
  stat_smooth(data=MR_df_pop2, aes(x=time, y = MR_mean_summ), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  ylim(c(-0.1,1.5)) +
  labs(title="Live MR - Population 2")

library(patchwork)
(plot_livesize_pop1 / plot_liveage_pop1) | (plot_livesize_pop2 / plot_liveage_pop2)  + plot_layout(guides = "collect")
(plot_deadMR_pop1 / plot_liveMR_pop1) | (plot_deadMR_pop2 / plot_liveMR_pop2) + plot_layout(guides = 'collect')

#write.csv(MR_df_pop2, file="MaladaptationRunPop2.csv")
#write.csv(MR_df_pop1, file="MaladaptationRunPop1.csv")

# Plot both runs on top
MaladaptationRunPop1 <- read.csv("MaladaptationRunPop1.csv"); MaladaptationRunPop1$Run <- "MaladaptationRunPop1"
MaladaptationRunPop2 <- read.csv("MaladaptationRunPop2.csv"); MaladaptationRunPop2$Run <- "MaladaptationRunPop2"
SoloRunPop1 <- read.csv("DataSim2_SoloPop1.csv"); SoloRunPop1$Run <- "SoloRunPop1"; SoloRunPop1$maladaptation_time <- 0
SoloRunPop2 <- read.csv("DataSim2_SoloPop2.csv"); SoloRunPop2$Run <- "SoloRunPop2"; SoloRunPop2$maladaptation_time <- 0

Pop1_res <- data.frame(rbind(MaladaptationRunPop1, SoloRunPop1))
Pop2_res <- data.frame(rbind(MaladaptationRunPop2, SoloRunPop2))

Pop1_plot <- ggplot() + 
  geom_point(data=Pop1_res, aes(x=time,  y=MR_mean_summ, colour=Run, shape=as.character(maladaptation_time))) +
  geom_errorbar(data=Pop1_res, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ, colour=Run), alpha=0.5) + 
  stat_smooth(data=Pop1_res, aes(x=time, y = MR_mean_summ, colour=Run), linewidth = 0.75, linetype="dashed", span=10) +
  ylim(c(-0.1,1.5)) +
  labs(title="Live MR - Population 1") +
  theme_bw()

Pop2_plot <- ggplot() + 
  geom_point(data=Pop2_res, aes(x=time,  y=MR_mean_summ, colour=Run, shape=as.character(maladaptation_time))) +
  geom_errorbar(data=Pop2_res, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ, colour=Run), alpha=0.5) + 
  stat_smooth(data=Pop2_res, aes(x=time, y = MR_mean_summ, colour=Run), linewidth = 0.75, linetype="dashed", span=10) +
  ylim(c(-0.1,1.5)) +
  labs(title="Live MR - Population 2") +
  theme_bw()  

Pop1_plot | Pop2_plot
