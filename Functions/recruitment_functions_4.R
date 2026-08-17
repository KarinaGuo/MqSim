## Function - Recruitment - density dependent recruitment

## Inputs
# pop - Population
# recruitment_age - age to maturity
# recruitment_constant - chance of recruitment (1 = recruits, 0 = none)
# population_min_size - Increased fecundity due to disturbance

# MR_togg - Turn on and off the Myrtle rust affect
# MR_recruit_impact_val - impact of Myrtle rust susceptibility on recruitment (multiplier)

# Note: MR is recruited based on parent pheno using beta distribution. To visualise parent MR pheno of 1-4: for (i in 0:4){hist(rbeta(n=4000, shape1=i+1, shape2 = 3)) }

##############################################
recruit_rate <- function(pop, population_min_size, population_max_size, recruitment_age, recruitment_size_mean, density_recruit_togg, recruitment_size_sd, recruitment_constant, age_togg, age_recruit_impact_val, MR_togg, MR_recruit_impact_val, MR_rec_adjusted, rec_age_shiftch, MR_parents, population_genotypes, indiv_count_start, time_point, Phase_1_end, effect_size, baseline_pheno, SNPs_tested){
  
  # Current pop_size
  if (length(pop$indiv_ID) < population_min_size){
    recruitment_constant <- recruitment_constant*10
    recruitment_adjust=1
  } else if (length(pop$indiv_ID) >= population_min_size & length(pop$indiv_ID) < population_max_size & density_recruit_togg) {
    recruitment_adjust = 1-length(pop$indiv_ID)/population_max_size
  } else {recruitment_adjust=1}
  
  # How many individuals are fecund and recruit?
  ages <- pop$age
  fecund_indivs <- ages >= recruitment_age
  recruitment_indivs <- lapply(pop, function(x) x[fecund_indivs])
  
  #recruitment_indivs_MR <- rescale(recruitment_indivs$MR, to = c(0,1))
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
  
  # Total impact on individuals recruitment
  total_impact <- total_impact*recruitment_adjust
  
  #tmp <- data.frame(recruitment_indivs_ages=recruitment_indivs_ages, recruitment_indivs_MR=recruitment_indivs_MR, MR_impact=MR_impact, age_impact=age_impact, total_impact=total_impact)
  #ggplot(data=tmp, aes(x=recruitment_indivs_MR, y=total_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  #ggplot(data=tmp, aes(x=recruitment_indivs_ages, y=total_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  #ggplot(data=tmp, aes(x=recruitment_indivs_MR, y=MR_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  #ggplot(data=tmp, aes(x=recruitment_indivs_ages, y=age_impact, colour=recruitment_indivs_ages)) + geom_point() +theme_bw()
  
  # Recruitment draw
  indiv_recruitment <- rbinom(n = length(recruitment_indivs$indiv_ID), size = 1, prob = recruitment_constant * total_impact)
  
  # For recruited individuals, what are their MR statuses
  
  if (sum(indiv_recruitment)>0){
    recruitment_indiv_MR <- recruitment_indivs_MR[as.logical(indiv_recruitment)]
    recruitment_indiv_gt <- population_genotypes[as.logical(indiv_recruitment)]
    
    new_recruit <- as.integer(rnorm(n=sum(indiv_recruitment), mean = recruitment_size_mean, sd = recruitment_size_sd)); new_recruit[new_recruit<1]=1
    new_recruit_genotypes <- list()
    for (i in 1:length(recruitment_indiv_MR)) { # For each new recruit, use parent phenotype to generate MR, dependent on MR
      
      parent_1_idx <- i
    
      num_offspring <- new_recruit[i]
      # New allele genotypes
      
      parent_1_gt <- recruitment_indiv_gt[[parent_1_idx]]
      if(MR_parents == 2 & sum(indiv_recruitment) > 1) {
        parent_2_idx <- sample(1:length(recruitment_indiv_MR), size = 1)
        parent_2_gt <- recruitment_indiv_gt[[parent_2_idx]]
      } else {
        parent_2_gt = parent_1_gt
      }
      
      n_loci <- length(parent_1_gt)
      
      alleles_p1 <- matrix(rbinom(n = num_offspring * n_loci, size = 1, prob = parent_1_gt / 2), 
                           nrow = num_offspring, byrow = TRUE)
      
      alleles_p2 <- matrix(rbinom(n = num_offspring * n_loci, size = 1, prob = parent_2_gt / 2), 
                           nrow = num_offspring, byrow = TRUE)
      
      offspring_gts_matrix <- alleles_p1 + alleles_p2
      
      offspring_list <- split(offspring_gts_matrix, seq_len(nrow(offspring_gts_matrix)))
      new_recruit_genotypes <- c(new_recruit_genotypes, unname(offspring_list))
    }
    
    new_recruit_MR <- rep(0, sum(new_recruit)) # will be overwritten later using AF calc
    new_recruit_error <- runif(n=sum(new_recruit), min = -0.3, max=0.3)
    
    if (time_point < Phase_1_end){
      new_recruit_MR <- Phenotype_from_Genotype(snp_effects = effect_size$V2, dominance_effect = effect_size$V3, individuals_GT = new_recruit_genotypes, error=new_recruit_error, phenotype_baseline = baseline_pheno)
    } else {
      if (time_point == Phase_1_end) {cat("Running GAPIT as pheno prediction \n")}
      invisible(capture.output(
        new_recruit_MR <- Phenotype_from_genotype_GAPIT(individuals_GT = new_recruit_genotypes, SNPs_tested = SNPs_tested)
      ))
    }
    
    
    new_recruit_pop <- list(indiv_ID=seq(from=indiv_count_start+1, to=indiv_count_start+sum(new_recruit)), 
                            time=rep(time_point, sum(new_recruit)), 
                            MR=new_recruit_MR, 
                            mortality=rep(0, sum(new_recruit)), 
                            age=rep(0, sum(new_recruit)),
                            error = new_recruit_error)
    
    curr_AF <- append(population_genotypes, new_recruit_genotypes)
    
    curr_pop <- list(
      indiv_ID=c(pop$indiv_ID, new_recruit_pop$indiv_ID), 
      age=c(pop$age, new_recruit_pop$age), 
      MR=c(pop$MR, new_recruit_pop$MR), 
      time=c(pop$time, new_recruit_pop$time), 
      mortality=c(pop$mortality, new_recruit_pop$mortality),
      error = c(pop$error, new_recruit_pop$error))
    
  } else {
    curr_pop <- list(
      indiv_ID=c(pop$indiv_ID, NULL), 
      age=c(pop$age, NULL), 
      MR=c(pop$MR, NULL), 
      time=c(pop$time, NULL), 
      mortality=c(pop$mortality, NULL),
      error = c(pop$error, NULL))
    
    curr_AF <- population_genotypes
  }
  
     return(list(curr_pop = curr_pop, curr_AF = curr_AF))
  
}
