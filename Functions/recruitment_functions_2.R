## Function - Recruitment

## Inputs
# pop - Population
# recruitment_age - age to maturity
# recruitment_constant - chance of recruitment (1 = recruits, 0 = none)
# population_min_size - Increased fecundity due to disturbance

# MR_togg - Turn on and off the Myrtle rust affect
# MR_recruit_impact_val - impact of Myrtle rust susceptibility on recruitment (multiplier)

# Note: MR is recruited based on parent pheno using beta distribution. To visualise parent MR pheno of 1-4: for (i in 0:4){hist(rbeta(n=4000, shape1=i+1, shape2 = 3)) }

##############################################
recruit_rate <- function(pop, population_min_size, recruitment_age, recruitment_size_mean, recruitment_size_sd, recruitment_constant, MR_togg, MR_recruit_impact_val, age_imp_rec_togg){
  
  # Current pop_size
  if (length(pop$indiv_ID) < population_min_size){
    recruitment_constant <- recruitment_constant*10
  }
  
  # How many individuals are fecund and recruit?
  
  fecund_indivs <- pop$age >= recruitment_age
  recruitment_indivs <- lapply(pop, function(x) x[fecund_indivs])
  age_trans <- 1 - 2/recruitment_indivs$age
  recruitment_indivs_MR <- recruitment_indivs$MR
  
  if (MR_togg){
    indiv_recruitment <- rbinom(n = length(recruitment_indivs$indiv_ID), size = 1, prob = recruitment_constant * 1/(1+MR_recruit_impact_val*recruitment_indivs_MR))
  } else {
    indiv_recruitment <- rbinom(n = length(recruitment_indivs$indiv_ID), size = 1, prob = recruitment_constant)
  }
  
  
  # For recruited individuals, what are their MR statuses
  
  if (sum(indiv_recruitment)>0){
    recruitment_indiv_MR <- recruitment_indivs$MR[as.logical(indiv_recruitment)]
    
    if (age_imp_rec_togg){
      
      recruitment_size_mean_age <- age_trans * recruitment_size_mean
      
      new_recruit = NULL
      for (i in 1:length(recruitment_indiv_MR)){
        recruitment_size_mean <- recruitment_size_mean_age[i]
        
        new_recruit_list <- as.integer(rnorm(n=1, mean = recruitment_size_mean, sd = recruitment_size_sd)); new_recruit_list[new_recruit_list<1] =  1
        new_recruit <- append(new_recruit, new_recruit_list)
        
      }
    } else {
      
      new_recruit <- as.integer(rnorm(n=sum(indiv_recruitment), mean = recruitment_size_mean, sd = recruitment_size_sd)); new_recruit[new_recruit<1]=1
    
      }
      
    new_recruit_MR=NULL
    for (i in 1:length(recruitment_indiv_MR)) { # For each new recruit, use parent phenotype to generate MR, dependent on MR
      
      MR_rec_PDF <- rbeta(n=new_recruit[i], shape1=recruitment_indiv_MR[i]+1, shape2=2-recruitment_indiv_MR[i]) # distribution of recruited individual's MR between 0 to 1
      
      #new_recruit_MR_new <- MR_rec_PDF+(recruitment_indiv_MR[i]-mean(MR_rec_PDF)) # recalibrate to make the mean the MR of parent pheno
      new_recruit_MR_new <- rescale(MR_rec_PDF, from = c(0,1), to=c(0,2)) # recalibrate to make the mean the MR of parent pheno
      
      new_recruit_MR <- append(new_recruit_MR, new_recruit_MR_new)
    }
    
    new_recruit_MR[new_recruit_MR<0]=0; new_recruit_MR[new_recruit_MR>=2]=1.9
    
    new_recruit_pop <- list(indiv_ID=seq(from=indiv_count+1, to=indiv_count+sum(new_recruit)), 
                            time=rep(time_point, sum(new_recruit)), 
                            MR=new_recruit_MR, 
                            mortality=rep(0, sum(new_recruit)), 
                            age=rep(1, sum(new_recruit)))
    
    # curr_pop <- rbind(curr_pop, pop_df_simres_tp, by='indiv_ID')
    curr_pop <- list(
      indiv_ID=c(pop$indiv_ID, new_recruit_pop$indiv_ID), 
      age=c(pop$age, new_recruit_pop$age), 
      MR=c(pop$MR, new_recruit_pop$MR), 
      time=c(pop$time, new_recruit_pop$time), 
      mortality=c(pop$mortality, new_recruit_pop$mortality))
    
    return(curr_pop)
    
  } else {
    curr_pop <- list(
    indiv_ID=c(pop$indiv_ID, NULL), 
    age=c(pop$age, NULL), 
    MR=c(pop$MR, NULL), 
    time=c(pop$time, NULL), 
    mortality=c(pop$mortality, NULL))
  }
    
}
