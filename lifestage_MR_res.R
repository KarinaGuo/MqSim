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

final_df_summ <- final_df %>% 
  group_by(time) %>% 
  summarise(MR_mean_summ=mean(MR, na.rm=TRUE), MR_sd_summ=sd(MR, na.rm=TRUE), pop_size=n())


plot_liveMR <- ggplot() +
  geom_point(data=final_df_summ, aes(x=time, y = MR_mean_summ)) +
  geom_errorbar(data=final_df_summ, aes(x=time, ymax = MR_mean_summ + MR_sd_summ, ymin = MR_mean_summ - MR_sd_summ)) + 
  stat_smooth(data=final_df_summ, aes(x=time, y = MR_mean_summ), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  labs(title="Live MR")
plot_liveMR

plot_liveage <- ggplot() +
  geom_point(data=final_df_summ, aes(x=time, y = pop_size)) +
  stat_smooth(data=final_df_summ, aes(x=time, y = pop_size), linewidth = 0.75, linetype="dashed", colour="grey40", span=10) +
  geom_vline(xintercept=MR_timepoint, linewidth = 0.75, linetype="dashed", colour="chocolate") +
  labs(title="Live pop size")
plot_liveage


LS_MR <- ggplot() + 
  #geom_boxplot(data = final_df %>% dplyr::filter(time > 2200 & time < 2400), aes(x=Lifestage, y=(log1p(MR)))) +  
  geom_boxplot(data = final_df %>% dplyr::filter(time > 1010 & time < 1020), aes(x=Lifestage, y=MR)) +  
  #geom_point(data = final_df %>% dplyr::filter(time > 2010 & time < 2020), aes(x=age, y=MR), size = 0.05) + 
  theme_bw() +
  labs(title="Impact 0.2; time +10-20")
LS_MR


LS_MR_2 <- ggplot() + 
  #geom_boxplot(data = final_df %>% dplyr::filter(time > 2200 & time < 2400), aes(x=Lifestage, y=(log1p(MR)))) +  
  geom_boxplot(data = final_df %>% dplyr::filter(time > 1030 & time < 1050), aes(x=Lifestage, y=MR)) +  
  #geom_point(data = final_df %>% dplyr::filter(time > 2010 & time < 2020), aes(x=age, y=MR), size = 0.05) + 
  theme_bw() +
  labs(title="Impact 0.2; time +30-50")
LS_MR_2


