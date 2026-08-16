## Identify critical zone: +100 since selection introduction
## Count number of reproductive individuals
## Calculate percentage against carrying capacity

file_list <- list.files("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Run_results/")



for (file in file_list){
  rm(list = ls())
  load(paste0("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Run_results/", file))

  pops <- bind_rows(pop_timepoints)
    
  sub_time <- pops |> 
    filter(time >= MR_timepoint + 90, time <= MR_timepoint + 110)
  
  # Calculate number of reproductive individuals
  
  sub_time_repro <- sub_time |> 
    filter(age >= recruitment_age)
  numb_repro <- nrow(sub_time_repro)
  
  
}
