## Function - Recruitment _ v3

## Inputs
# Version includes fixed population size

##############################################
recruit_rate <- function(pop, recruit_size, pop_size, recruitment_age, recruitment_size_mean, recruitment_size_sd, recruitment_constant, age_togg, age_recruit_impact_val, MR_togg, MR_recruit_impact_val, MR_rec_adjusted, rec_age_shiftch){
  
  
  # How many individuals are fecund and recruit?
  ages <- pop$age
  fecund_indivs <- ages >= recruitment_age
  recruitment_indivs <- lapply(pop, function(x) x[fecund_indivs])
  
  #recruitment_indivs_MR <- rescale(recruitment_indivs$MR, to = c(0,1))
  recruitment_indivs_ID <- recruitment_indivs$indiv_ID
  recruitment_indivs_MR <- recruitment_indivs$MR
  recruitment_indivs_ages <- recruitment_indivs$age
  
  ### Reducing recruitment chances
  # Default impacts
  MR_impact  <- 1
  age_impact <- 1
  
  MR_impact <- 1 - recruitment_indivs_MR ^ MR_recruit_impact_val
  age_scaled <- recruitment_indivs_ages / rec_age_shiftch; age_scaled[age_scaled>1]=1
  age_impact <- age_scaled ^ age_recruit_impact_val
  
  # Both MR and age impacts
  if (MR_togg & age_togg) {
    total_impact <- MR_impact + age_impact - (MR_impact * age_impact)
  } else if (MR_togg & !age_togg) {
    total_impact <- MR_impact
  } else if (age_togg & !MR_togg) {
    total_impact <- age_impact
  } else {
    total_impact <- 1
  }
  
  #tmp <- data.frame(recruitment_indivs_ages=recruitment_indivs_ages, recruitment_indivs_MR=recruitment_indivs_MR, MR_impact=MR_impact, age_impact=age_impact, total_impact=total_impact)
  #ggplot(data=tmp, aes(x=recruitment_indivs_MR, y=total_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  #ggplot(data=tmp, aes(x=recruitment_indivs_ages, y=total_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  #ggplot(data=tmp, aes(x=recruitment_indivs_MR, y=MR_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  #ggplot(data=tmp, aes(x=recruitment_indivs_ages, y=age_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  
  # Recruitment draw
  if (length(recruitment_indivs_ID) == 1){
    
    indiv_recruitment <- recruitment_indivs_ID
    recruitment_indiv_MR <- rep(recruitment_indivs_MR, recruit_size)
  
  } else {
    
    indiv_recruitment <- sample(x=recruitment_indivs_ID, size = recruit_size, replace=T, prob = recruitment_constant * total_impact)
    
    # For recruited individuals, what are their MR statuses
    recruitment_indiv_MR <- recruitment_indivs_MR[match(indiv_recruitment, recruitment_indivs_ID)]
    
  }
  
  new_recruit_MR=NULL
  for (i in 1:recruit_size) { # For each new recruit, use parent phenotype to generate MR, dependent on MR
      
    MR_rec_PDF <- rbeta(n=1, shape1=(recruitment_indiv_MR[i]+1)^2, shape2=(2-recruitment_indiv_MR[i])^2) # distribution of recruited individual's MR between 0 to 1
    #new_recruit_MR_new <- MR_rec_PDF+(recruitment_indiv_MR[i]+MR_rec_adjusted-mean(MR_rec_PDF)) # recalibrate to make the mean the MR of parent pheno
    #new_recruit_MR_new <- rescale(MR_rec_PDF, from = c(0,1), to=c(0,2)) # recalibrate to make the mean the MR of parent pheno
      
    new_recruit_MR <- append(new_recruit_MR, MR_rec_PDF)
    
    new_recruit_MR[new_recruit_MR<0]=0; new_recruit_MR[new_recruit_MR>=1]=1
  }
    
    new_recruit_pop <- list(indiv_ID=seq(from=indiv_count_end+1, to=indiv_count_end+recruit_size), 
                            time=rep(time_point, recruit_size), 
                            MR=new_recruit_MR, 
                            mortality=rep(0, recruit_size), 
                            age=rep(1, recruit_size))
    
    curr_pop <- list(
      indiv_ID=c(pop$indiv_ID, new_recruit_pop$indiv_ID), 
      age=c(pop$age, new_recruit_pop$age), 
      MR=c(pop$MR, new_recruit_pop$MR), 
      time=c(pop$time, new_recruit_pop$time), 
      mortality=c(pop$mortality, new_recruit_pop$mortality))
    return(curr_pop)}
