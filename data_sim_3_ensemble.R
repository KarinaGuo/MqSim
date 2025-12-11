iterations = 2

setwd("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Natural selection/Simulation/")

source("MainPlot_MR_tests/Rerun_Mult_iter/MR_ensrun_configurations.txt")

for (MR_death_impact in c(0.2, 0.4, 0.9)){
  MR_death_impact = MR_death_impact
  for (iteration in iterations){
    source(file = "data_sim_3.R")
    
    #MR runs
    filesavename = paste0("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_", MR_death_impact,"_Intro1000_",iteration,".csv")
    write.csv(live_size_df, file=filesavename)
    
  }
}

MR_low_all = NULL
MR_mid_all = NULL
MR_high_all = NULL

for (MR_death_impact in c(0.2, 0.4, 0.9)){
  for  (i in iterations){
    filename = paste0("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_", MR_death_impact,"_Intro1000_",iteration,".csv")
    
    file = read.csv(filename) %>% 
      mutate(Run=paste0("MR_", MR_death_impact))
    
    MR_low_all <- as.data.frame(rbind(MR_low_all, file))
    
    if (i == iterations){
      MR_low_all_summ <- MR_low_all %>% 
        group_by(Run, time) %>% 
        summarise(mean_sumsize = mean(sum_size))
    }
  }
  
}