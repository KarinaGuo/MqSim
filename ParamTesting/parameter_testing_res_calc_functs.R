### Measure summary statistic

# age_df - columns= age, count

## Functions
exponential_curve <- function(df){
  age_df <- df %>% 
    group_by(age) %>% 
    summarise (n=n())
  
  exponential <- nls(
    n ~ a * exp(b * age),
    data = age_df,
    start = list(a = max(age_df$n), b = -0.1)
  )
  
  params <- coef(exponential)
  a <- params["a"]
  b <- params["b"]
  
  x1 <- min(age_df$age)
  x2 <- max(age_df$age)
  
  AUC <- (a / b) * (exp(b * x2) - exp(b * x1))
  return(AUC)
}

Myrtlerust_calc <- function(df){
  mean_MR = mean(df$MR)
  sd_MR = sd(df$MR)
  
  return(data.frame(cbind(mean_MR=mean_MR, sd_MR=sd_MR)))
}

calculate_lifestage <- function(timepoint_df){
  timepoint_df <- timepoint_df %>% mutate(Lifestage = case_when(
    age <= 2 ~ "Seedling",
    age > 2 & age <= 7 ~ "Subadult",
    age > 7 ~ "Adult"
  ))
  
  timepoint_df_sub <- timepoint_df %>% group_by(time, Lifestage) %>% sample_n(5) %>% ungroup()
  
  MRpop_LS <- timepoint_df_sub %>% group_by(Lifestage) %>%  group_modify(~ Myrtlerust_calc(.x))
  
  return(MRpop_LS)
}

calculate_timepoint_vals <- function(pop_timepoints){
   
  # Timepoint before
  timepoints_before <- which(timepoint_pop_grab < MR_timepoint)
  
  pop_before <- pop_timepoints[timepoints_before]
  pop_before_df <- bind_rows(lapply(pop_before, as.data.frame))
  
  pop_struct <- exponential_curve(pop_before_df)
  pop_size <- nrow(pop_before_df)
  MR_res <- Myrtlerust_calc((pop_before_df))
  before_res <- data.frame(cbind(timeperiod="Before", pop_struct=pop_struct, pop_size=pop_size, MR_res))
  
  # Timepoint soon MR intro
  
  timepoints_soon <- which(timepoint_pop_grab > MR_timepoint & timepoint_pop_grab < MR_timepoint*1.5)
  
  pop_soon <- pop_timepoints[timepoints_soon]
  pop_soon_df <- bind_rows(lapply(pop_soon, as.data.frame))
  
  pop_struct <- exponential_curve(pop_soon_df)
  pop_size <- nrow(pop_soon_df)
  MR_res <- Myrtlerust_calc((pop_soon_df))
  soon_res <- data.frame(cbind(timeperiod="Soon", pop_struct=pop_struct, pop_size=pop_size, MR_res))
  
  # Timepoint after MR intro
  timepoints_after <- which(timepoint_pop_grab > MR_timepoint*1.5)
  
  pop_after <- pop_timepoints[timepoints_after]
  pop_after_df <- bind_rows(lapply(pop_after, as.data.frame))
  
  pop_struct <- exponential_curve(pop_after_df)
  pop_size <- nrow(pop_after_df)
  MR_res <- Myrtlerust_calc(pop_after_df)
  after_res <- data.frame(cbind(timeperiod="After", pop_struct=pop_struct, pop_size=pop_size, MR_res))
  
  ## Merge res
  iter_res <- rbind(before_res, soon_res, after_res)
   
  return(iter_res)
}

calculate_timepoint_LSvals <- function(pop_timepoints){
  
  # Timepoint soon MR intro
  
  timepoints_soon <- which(timepoint_pop_grab > MR_timepoint & timepoint_pop_grab < MR_timepoint*1.5)
  
  pop_soon <- pop_timepoints[timepoints_soon]
  pop_soon_df <- bind_rows(lapply(pop_soon, as.data.frame))

  MR_soon_LS_res <- calculate_lifestage(pop_soon_df)
  soon_res <- data.frame(cbind(timeperiod="Soon", MR_soon_LS_res))
  
  # Timepoint after MR intro
  timepoints_after <- which(timepoint_pop_grab > MR_timepoint*1.5)
  
  pop_after <- pop_timepoints[timepoints_after]
  pop_after_df <- bind_rows(lapply(pop_after, as.data.frame))
  
  MR_after_LS_res <- calculate_lifestage(pop_after_df)
  after_res <- data.frame(cbind(timeperiod="After", MR_after_LS_res))
  
  ## Merge res
  iter_res <- as.data.frame(rbind(soon_res, after_res))
  
  return(iter_res)
}
