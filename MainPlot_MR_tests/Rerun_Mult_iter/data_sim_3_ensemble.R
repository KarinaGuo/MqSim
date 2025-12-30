iterations = 10
setwd("/home/karina/Simulation/MqSim/")

source("MainPlot_MR_tests/Rerun_Mult_iter/MR_ensrun_configurations.txt")

for (MR_death_impact in c(0.2, 0.4, 0.9)){
  MR_death_impact = MR_death_impact
  
  for (iteration in 1:iterations){
    source(file = "data_sim_3.R")
    
    #MR runs
    filesavename = paste0("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_", MR_death_impact,"_Intro1000_",iteration,".csv")
    write.csv(live_size_df, file=filesavename)
    
    filesavename_2 = paste0("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_", MR_death_impact,"_Intro1000_",iteration,".csv")
    write.csv(MR_df, file=filesavename_2)
    
  }
}


MR_all = NULL
SIZE_all = NULL

for (MR_death_impact in c(0.2, 0.4, 0.9)){
  for  (iteration in 1:iterations){
    filename = paste0("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_", MR_death_impact,"_Intro1000_",iteration,".csv")
    filename_2 = paste0("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_", MR_death_impact,"_Intro1000_",iteration,".csv")
    
    
    file = read.csv(filename) %>% 
      mutate(Run=paste0("MR_", MR_death_impact))
    file_2 = read.csv(filename_2) %>% 
      mutate(Run=paste0("MR_", MR_death_impact))
    
    SIZE_all <- as.data.frame(rbind(SIZE_all, file))
    MR_all <- as.data.frame(rbind(MR_all, file_2))
    
    if (iteration == 1:iterations){
      SIZE_all_summ <- SIZE_all %>% 
        group_by(Run, time) %>% 
        summarise(mean_sumsize = mean(sum_size))
      
      MR_all_summ <- MR_all %>% 
        group_by(Run, time) %>% 
        summarise(mean_MR_mean_summ = mean(MR_mean_summ), 
                  mean_MR_sd_summ = mean (MR_sd_summ))
      
      write.csv(MR_all_summ, file = paste0("MainPlot_MR_tests/Rerun_Mult_iter/MR_base_", MR_death_impact,"_Intro1000_summ.csv"))
      write.csv(SIZE_all_summ, file = paste0("MainPlot_MR_tests/Rerun_Mult_iter/SIZE_base_", MR_death_impact,"_Intro1000_summ.csv"))
      
    }
  }
  
}
