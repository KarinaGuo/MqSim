### Exported time point analyses

df_list <- lapply(seq_along(pop_timepoints), function(i) {
  pop <- pop_timepoints[[i]]
  data.frame(
    time = pop$time - 1,
    age = pop$age,
    MR = pop$MR  
  )
})

final_df <- do.call(rbind, df_list)

final_df <- final_df %>% 
  mutate(Lifestage = case_when(
    age <= 2 ~ "Seedling",
    age > 2 & age <= 7 ~ "Subadult",
    age > 7 ~ "Adult"
  ))

final_df$Lifestage <- factor(final_df$Lifestage, levels = c("Seedling", "Subadult", "Adult")) # Reorder Sev factor levels

LS_MR <- ggplot() + 
  geom_boxplot(data = final_df %>% dplyr::filter(time > 10), aes(x=Lifestage, y=(log1p(MR)))) +  # Time selected at plateau of 10
  theme_bw() 
LS_MR
LS_MR + facet_wrap(~time)


