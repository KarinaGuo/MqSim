iterations = 10
setwd("/home/karina/Simulation/MqSim/")

source("MainPlot_Intervention/Rerun_mult_iter/configurations_int_esrun")

for (intercept_indiv_original in c(1000, 2500, 5000)) {
  
  for (intercept_MR_mean in c(0.1, 0.5, 0.9)) {
    
    for (iteration in 1:iterations) {
      
      cat("Running int indiv", intercept_indiv_original, "with MR", intercept_MR_mean, "\n")    
      intercept_indiv = as.numeric(intercept_indiv_original)
  
      source("data_sim_3.R")
      
      filesavename  <- paste0("MainPlot_Intervention/Rerun_mult_iter/SIZE_int_MR_0.4_Intro1000_S", intercept_indiv_original, "_intMR_", intercept_MR_mean, "_", iteration, ".csv")
      filesavename_2 <- paste0("MainPlot_Intervention/Rerun_mult_iter/MR_int_MR_0.4_Intro1000_S", intercept_indiv_original, "_intMR_", intercept_MR_mean, "_", iteration, ".csv")
      
      write.csv(live_size_df, filesavename, row.names=F)
      write.csv(MR_df, filesavename_2, row.names=F)
    }
  }
}


SIZE_file_all = NULL
MR_file_all = NULL

for (intercept_indiv_original in c(1000, 2500, 5000)) {
  
  for (intercept_MR_mean in c(0.1, 0.5, 0.9)) {
    
    for (iteration in 1:iterations) {
    filename  <- paste0("MainPlot_Intervention/Rerun_mult_iter/SIZE_int_MR_0.4_Intro1000_S", intercept_indiv_original, "_intMR_", intercept_MR_mean, "_", iteration, ".csv")
    filename_2 <- paste0("MainPlot_Intervention/Rerun_mult_iter/MR_int_MR_0.4_Intro1000_S", intercept_indiv_original, "_intMR_", intercept_MR_mean, "_", iteration, ".csv")
      
    file = read.csv(filename) %>% 
      mutate(intercept_indiv_original=intercept_indiv_original,
      intercept_MR_mean=intercept_MR_mean) %>% 
      dplyr::select(time, sum_size, intercept_indiv_original, intercept_MR_mean)
      
    file_2 = read.csv(filename_2) %>% 
      mutate(intercept_indiv_original=intercept_indiv_original,
      intercept_MR_mean=intercept_MR_mean) %>% 
      dplyr::select(time, MR_mean_summ, MR_sd_summ, intercept_indiv_original, intercept_MR_mean)
        
    SIZE_file_all <- as.data.frame(rbind(SIZE_file_all, file))
    MR_file_all <- as.data.frame(rbind(MR_file_all, file_2))
  }
  
}
}

SIZE_file_all_summ <- SIZE_file_all %>% 
        group_by(intercept_indiv_original, intercept_MR_mean, time) %>% 
        summarise(mean_sumsize = mean(sum_size))
        
MR_file_all_summ <- MR_file_all %>% 
        group_by(intercept_indiv_original, intercept_MR_mean, time) %>% 
        summarise(mean_MR_mean_summ = mean(MR_mean_summ),
                  mean_MR_sd_summ = mean(MR_sd_summ))

write.csv(SIZE_file_all_summ, file="MainPlot_Intervention/Rerun_mult_iter/SIZE_int_MR_0.4_Intro1000_summ.csv")
write.csv(MR_file_all_summ, file="MainPlot_Intervention/Rerun_mult_iter/MR_int_MR_0.4_Intro1000_summ.csv")


