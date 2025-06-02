set.seed(12345)



Simulations
• Look into matrix model
• We want this simulation to match our selection pressure observed
• Ideas:
  ○ Encoding data as a lifestage table per year with mortality risks (run this by itself at neutral selection over a period of time to determine whether it separates out into an appropriate age distribution)
○ Live individual table
○ Dead at time point table
○ Generate MR by the S-function (hill function of rnorm - has asymptotes of limits)
  ○ Change recruitment to 1000 but at 1% chance
• Future:
  ○ Add a self-thinning density dependency especially on subadult and seedlings
§ Accelerated mortality with higher density
○ Incorporate disturbance events -> higher recruitment chance + high mortality for older individuals
○ Incorporate different site disease pressures
Explore the impact of different selective pressures




indiv_pop=NULL; MR_pop=NULL; curr_pop=NULL; new_recruit_pop=NULL; indiv_count=0

for (time_point in 1:time_max){
  timepoint_count=timepoint_count+1
  
  
  if(time_point==1){
    curr_pop <- pop_df
  } else {
    curr_pop <- curr_pop[(curr_pop$mortality == 0 & curr_pop$time == time_point-1), ]
    curr_pop <- curr_pop %>% filter(!is.na(time))
    curr_pop$indiv_ID <- as.numeric(curr_pop$indiv_ID); curr_pop$MR <- as.numeric(curr_pop$MR); curr_pop$age <- as.numeric(curr_pop$age)
  }
  
  if(nrow(curr_pop)==0) {
    stop("All dead at time ", time_point, "\n")
  } else{
    
    indiv_count=length(unique(curr_pop$indiv_ID)) + indiv_count
    indiv_alive_count=nrow(curr_pop)
    
    MR_currmean = mean(curr_pop$MR)
    MR_norm=(curr_pop$MR - min(curr_pop$MR)) / (max(curr_pop$MR) - min(curr_pop$MR))
    
    mortality_perc <- death_constant + (MR_death_impact * (MR_norm)) + (age_impact * (1/curr_pop$age))
    mortality_perc <- pmin(pmax(mortality_perc, 0), 1)
    
    indiv_death <- rbinom(n = length(mortality_perc), size = 1, prob = mortality_perc)
    
    pop_df_simres_tp <- as.data.frame(cbind(indiv_ID=curr_pop$indiv_ID, MR=as.numeric(curr_pop$MR), time=time_point, mortality=indiv_death, age=curr_pop$age+1))
    
    ## Recruitment
    
    recruitment_indivs <- curr_pop %>% filter(age > recruitment_age)
    recruitment_chance <- nrow(recruitment_indivs)
    indiv_recruitment <- rbinom(n = recruitment_chance, size = 1, prob = recruitment_constant)
    
    if (sum(indiv_recruitment)>0){
      recruitment_indiv_MR <- recruitment_indivs[indiv_recruitment,]$MR
      
      new_recruit <- rnorm(n=sum(indiv_recruitment), mean = recruitment_size_mean, sd = recruitment_size_sd); new_recruit[new_recruit<0]=0; sum(ceiling(new_recruit))
      new_recruit_MR <- rnorm(n=ceiling(length(new_recruit)), mean = recruitment_indiv_MR, sd = MR_sd/2); new_recruit_MR[new_recruit_MR<0]=0
      
      new_recruit_pop <- data.frame(indiv_ID=seq(from=indiv_count+1, to=indiv_count+ceiling(length(new_recruit))), MR=new_recruit_MR, time=time_point, mortality=0, age=1)
      
      curr_pop <- rbind(curr_pop, pop_df_simres_tp, by='indiv_ID')
      
      curr_pop <- rbind(curr_pop, new_recruit_pop)
      
    }
      
    curr_pop <- rbind(curr_pop, pop_df_simres_tp, by='indiv_ID')
    indiv_pop <- as.data.frame(rbind(indiv_pop, cbind(time=time_point, indiv_alive=indiv_alive_count)) )
    MR_pop <- as.data.frame(rbind(MR_pop, cbind(time=time_point, MR_currmean=MR_currmean)) )
      
    }
}

plot(MR_pop)
plot(indiv_pop)
