#### data_sim_5.R param testing

set.seed(12345)

# 1. Save param sweep values
sweep_MR_mean <- MR_mean
sweep_MR_sd <- MR_sd
sweep_age_impact <- age_impact
sweep_MR_death_impact <- MR_death_impact
sweep_MR_age_impact <- MR_age_impact
sweep_age_recruit_impact_value <- age_recruit_impact_value
sweep_MR_recruit_impact <- MR_recruit_impact
sweep_recruitment_const <- recruitment_const
sweep_dist_imp <- dist_imp

# 2. Load the save
load("Phase_1_end_save.Rdata")

# 3. Restore param sweep values
MR_mean <- sweep_MR_mean
MR_sd <- sweep_MR_sd
age_impact <- sweep_age_impact
MR_death_impact <- sweep_MR_death_impact
MR_age_impact <- sweep_MR_age_impact
age_recruit_impact_value <- sweep_age_recruit_impact_value
MR_recruit_impact <- sweep_MR_recruit_impact
recruitment_const <- sweep_recruitment_const
dist_imp <- sweep_dist_imp

# 5. Clear unnecessary lists to save memory since we only care about param test outputs (keep pop_timepoints!)
AF_timepoints <- list()
death_df <- NULL
age_df <- NULL
MR_df <- NULL
live_size_df <- NULL

# Workflow
for (time_point in Phase_1_end:time_max){
  
  ##### Apply disturbance results
  if (dist_imp){
    disturbance_event <- disturbance_event_chance (dist_togg = dist_imp, disturbance_age_struct = disturbance_age_struct_type, dist_impact_val = dist_impact, dist_age_impact_val = dist_age_impact)
    disturbance_event_res <- disturbance_event[1]; age_impact = disturbance_event[2]; MR_death_impact = disturbance_event[3]; recruitment_const = disturbance_event[4]
  } else {
    disturbance_event_res=0
  }
  
  #### Initiating population
  if(time_point==1){
    curr_pop_start <- pop_df
    curr_AF_start <- AF_list
  } else {
    curr_pop_start <- lapply(curr_pop_end, function(x) x[curr_pop_end$time==time_point])
    curr_AF_start <- curr_AF_end[curr_pop_end$time==time_point]
  }
  
  if (time_point == Phase_1_end){
    cat("Rewriting AF start with SNP_AF_Histset \n")
    
    curr_AF_start <- lapply(1:length(curr_pop_start$indiv_ID), function(i) {
      rbinom(n = length(SNP_AF_Histset$frequency), size = 2, prob = SNP_AF_Histset$frequency)
      
    })
    
    invisible(capture.output(
      curr_pop_start$MR <- Phenotype_from_genotype_GAPIT(individuals_GT = curr_AF_start, SNPs_tested = SNPs_tested)
    ))
  } 
  
  # curr_pop_start$MR <- Phenotype_from_Genotype(snp_effects = effect_size$V2, dominance_effect = effect_size$V3, individuals_GT = curr_AF_start, error=curr_pop_start$error, phenotype_baseline = baseline_pheno)
  
  if((length(curr_pop_start$indiv_ID)==0)) {
    stop("All dead at time ", time_point, "\n")
  } else{
    
    #### Count number of individuals
    indiv_count_start=length(curr_pop_start$indiv_ID) + indiv_count_end
    indiv_alive_count=length(curr_pop_start$indiv_ID)
    
    # Intercept inputs
    if (intercept_togg & time_point > intercept_timepoint & intercept_reducMort){intercept_pop_indiv_ID=intercept_pop$indiv_ID; int_togg=TRUE} else {int_togg=FALSE; intercept_pop_indiv_ID=NULL}
    
    if (time_point == intercept_timepoint+1 & intercept_togg){cat ("intercept_pop_indiv_ID = ", length(intercept_pop_indiv_ID), " ; int_togg = ",int_togg,"\n" )}
    
    #### Recruitment on intial start pop
    # If MR has not activated then regardless MR impact on rec is 0 ðŸ˜º
    if (time_point>=MR_timepoint & MR_lateintro & MR_imp){
      MR_recruit_impact_tp = MR_recruit_impact
    } else if (time_point<MR_timepoint & MR_lateintro & MR_imp) {
      MR_recruit_impact_tp = 0
    } else if (!MR_lateintro | !MR_imp) {
      MR_recruit_impact_tp = 0
    }
    
    recruit_res <- recruit_rate(pop=curr_pop_start, recruitment_age=recruitment_age, population_min_size=population_minimum_size, population_max_size=population_carrying_capacity, density_recruit_togg=density_recruit_toggle, recruitment_size_mean=recruitment_mean, recruitment_size_sd=recruitment_sd, recruitment_constant=recruitment_const, MR_togg=MR_rec_toggle, MR_recruit_impact_val=MR_recruit_impact_tp, MR_rec_adjusted=MR_rec_adj, age_togg=age_rec_toggle, age_recruit_impact_val=age_recruit_impact_value, rec_age_shiftch=rec_age_shift, MR_parents=MR_inherit_par_num, population_genotypes=curr_AF_start, indiv_count_start=indiv_count_start, time_point=time_point)
    curr_pop_recruited <- recruit_res$curr_pop
    curr_AF_recruited <- recruit_res$curr_AF
    
    recruited_indivs = length(curr_pop_recruited$indiv_ID) - indiv_alive_count
    indiv_count_end = recruited_indivs + indiv_count_end
    
    #### Restoration action
    if (time_point == intercept_timepoint & intercept_togg){
      intercept_indiv = intercept_indiv_original
      if (intercept_indiv <= 0) {stop("too few intercept indivs")}
      
      cat("Time at:", time_point,"\n",
          "Individuals alive:", length(curr_pop_recruited$indiv_ID), "\n",
          "Mean MR of live individuals:", mean(curr_pop_recruited$MR), "\n")
      
      int_MR <- rnorm(n=intercept_indiv, mean = intercept_MR_mean, sd = intercept_MR_sd); int_MR[int_MR<0]=0; int_MR[int_MR>1]=1
      
      intercept_pop <- list(
        indiv_ID = seq(from = indiv_count_end + 1,
                       to   = indiv_count_end + intercept_indiv),
        time = rep(time_point, intercept_indiv),
        MR=as.numeric(int_MR),
        #mortality = rep(0, intercept_indiv),
        age = rep(2, intercept_indiv))
      
      curr_pop_og <- curr_pop_recruited
      curr_pop_int <- list( # Merging onto previous population recruited
        indiv_ID=c(curr_pop_recruited$indiv_ID, intercept_pop$indiv_ID), 
        age=c(curr_pop_recruited$age, intercept_pop$age), 
        MR=c(curr_pop_recruited$MR, intercept_pop$MR), 
        time=c(curr_pop_recruited$time, intercept_pop$time))
      
      curr_pop_recruited <- curr_pop_int 
      
      indiv_count_end=length(curr_pop_recruited$indiv_ID) + indiv_count_end # Adding onto indiv_ID count
      
      cat(  "## Intervention ##\n",
            "Individuals alive:", length(curr_pop_recruited$indiv_ID), "\n",
            "Mean MR of live individuals after:", mean(curr_pop_recruited$MR), "\n",
            "#################\n")
      
      # Plots
      cat(str(curr_pop_recruited))
      #print(ggplot() + geom_point(aes(x=curr_pop_recruited$age, y=curr_pop_recruited$MR)) + theme_bw() + labs(title=paste("MR by age at", time_point)))
    } else {intercept_indiv=0}
    
    #### Population mortality on initial population
    
    # Different ifelse for each scenario
    
    if (time_point>=MR_timepoint & MR_lateintro & MR_imp){
      indiv_death <- mortality_death_rate_MRlate(pop=curr_pop_start, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_death_impact_val=MR_death_impact, MR_age_impact_val=MR_age_impact, age_impact_val=age_impact, mortality_age_shiftch=mortality_age_shift, MR_intro=MR_lateintro, MR_intro_timepoint=MR_timepoint, int_togg=int_togg, intercept_pop_indiv_ID=intercept_pop_indiv_ID)
      if(time_point==MR_timepoint){cat("Using mortality_death_rate_MRlate \n")}
      
    } else if (time_point<MR_timepoint & MR_lateintro & MR_imp){
      MR_death_impact_beforeintro = 0
      indiv_death <- mortality_death_rate(pop=curr_pop_start, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact_beforeintro, MR_age_impact_val=MR_age_impact, age_impact_val=age_impact, mortality_age_shiftch=mortality_age_shift, int_togg=int_togg, intercept_pop_indiv_ID=intercept_pop_indiv_ID)
      if(time_point==1){cat("Using mortality_death_rate & MR before imp \n")}
      
    } else if (!MR_lateintro | !MR_imp) {
      indiv_death <- mortality_death_rate(pop=curr_pop_start, population_capacity=population_carrying_capacity, comp_togg=comp_imp, comp_impact_val=comp_impact, MR_togg=MR_imp, MR_death_impact_val=MR_death_impact, MR_age_impact_val=MR_age_impact, age_impact_val=age_impact, mortality_age_shiftch=mortality_age_shift, int_togg=int_togg, intercept_pop_indiv_ID=intercept_pop_indiv_ID)
      if(time_point==1){cat("Using mortality_death_rate \n")}
      
    }
    
    if (any(sapply(curr_AF_recruited, is.null))){cat("Error ", time_point)}
    
    #### Tracking death counts
    death_df_curr <- data.frame(Dead_ID=curr_pop_start$indiv_ID[as.logical(indiv_death)], age=curr_pop_start$age[as.logical(indiv_death)], MR=curr_pop_start$MR[as.logical(indiv_death)], time=curr_pop_start$time[as.logical(indiv_death)])
    death_df <- rbind(death_df, death_df_curr)
    
    indiv_death = c(indiv_death, rep(0, recruited_indivs + intercept_indiv)) 
    
    #### Final time point pop
    curr_pop_end <- list(indiv_ID=curr_pop_recruited$indiv_ID[!as.logical(indiv_death)], age=curr_pop_recruited$age[!as.logical(indiv_death)]+1, MR=curr_pop_recruited$MR[!as.logical(indiv_death)], time=curr_pop_recruited$time[!as.logical(indiv_death)]+1, error = curr_pop_recruited$error[!as.logical(indiv_death)])
    curr_AF_end <- curr_AF_recruited[!as.logical(indiv_death)] # check lengths
    
    if (any(sapply(curr_AF_end, is.null))){cat("Error ", time_point)}
    
    # Generate summary data if not dead
    if (length(curr_pop_recruited$indiv_ID)>sum(indiv_death)){ 
      MR_summ <- data.frame(time=time_point, MR_mean_summ=mean(curr_pop_end$MR, na.rm=TRUE), MR_sd_summ=sd(curr_pop_end$MR))
      MR_df <- rbind(MR_df, MR_summ)
      
      age_summ <- data.frame(time=time_point, age_mean_summ=mean(curr_pop_end$age), age_sd_summ=sd(curr_pop_end$age),pop_size=length(curr_pop_end$indiv_ID))
      age_df <- rbind(age_df, age_summ) 
    }
    
    live_size <- data.frame(time=time_point, sum_size=length(curr_pop_end$indiv_ID))
    live_size_df <- rbind(live_size_df, live_size) 
    
    # Save populations at user designated timepoints
    if (!is.null(timepoint_pop_grab) && (time_point %in% timepoint_pop_grab)){
      i <- which(time_point == timepoint_pop_grab)
      pop_timepoints[[i]] <- curr_pop_end
      AF_timepoints[[i]] <- curr_AF_end
    }
    
    # Return to base
    if (as.logical(disturbance_event_res)){
      age_impact = disturbance_event[5]
      MR_death_impact = disturbance_event[6]
      recruitment_const = disturbance_event[7]
    }
  }
}
