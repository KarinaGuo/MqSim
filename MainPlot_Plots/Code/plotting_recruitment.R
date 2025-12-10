## Function - Mortality death rate _ Hill Function

recruitment_indivs_ages <- seq(8:200)
recruitment_indivs_MR <- runif(n=193)
rec_age_shiftch = 100
MR_recruit_impact_val = 0.3

MR_impact  <- 1
age_impact <- 1


##### MR impact value
MR_impact_all = NULL
for (MR_recruit_impact_val in seq(from=0.1, to=1, by=0.1)){
  MR_impact <- 1 - recruitment_indivs_MR ^ MR_recruit_impact_val
  
  MR_impact_all <- data.frame(rbind(MR_impact_all, cbind(MR=recruitment_indivs_MR, MR_impact=MR_impact, MR_recruit_impact_val=MR_recruit_impact_val)))
}

MR_plot <- ggplot(data=MR_impact_all, aes(x=MR, y=MR_impact, colour=as.character(MR_recruit_impact_val))) + 
  geom_line() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(
    values = hcl.colors(
      palette = "Burg", 
      n = length(unique(MR_impact_all$MR_recruit_impact_val)))) +
  labs(x="Myrtle rust", y="Impact multiplier on base recruitment", colour = "MR recruit \nimpact value")

ggsave(MR_plot, file="Plots/SI plots/Recruitment_MR_impact_value.jpeg", width = 1700, height=1600, units="px")

##### Age impact value
age_impact_all = NULL
for (age_recruit_impact_val in seq(from=0.1, to=1, by=0.1)){
  age_scaled <- recruitment_indivs_ages / rec_age_shiftch; age_scaled[age_scaled>1]=1
  age_impact <- age_scaled ^ age_recruit_impact_val
  
  age_impact_all <- data.frame(rbind(age_impact_all, cbind(Age=recruitment_indivs_ages, age_impact=age_impact, age_recruit_impact_val=age_recruit_impact_val)))
}

age_plot <- ggplot(age_impact_all, aes(x = Age, y = age_impact, colour = as.character(age_recruit_impact_val))) + 
  geom_vline(xintercept = 100, linetype = 'dashed', size = 0.2, color = "chocolate4") +
  geom_line() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(
    values = hcl.colors(
      palette = "Burg", 
      n = length(unique(age_impact_all$age_recruit_impact_val)))) +
  labs(x = "Age", y = "Impact multiplier on base recruitment", colour = "Age recruit\nimpact value")

ggsave(age_plot, file="Plots/SI plots/Recruitment_age_impact_value.jpeg", width = 1700, height=1600, units="px")


########################################## Total impact


results_all <- data.frame(
  age_recruit_impact_val = numeric(),
  age = numeric(),
  age_impact = numeric(),
  MR_recruit_impact_val = numeric(),
  MR = numeric(),
  MR_impact = numeric()
)

for (MR_recruit_impact_val in c(0.3, 0.9)) {
  for (recruitment_indivs_ages in seq(0, 200, 10)) {
    for (age_recruit_impact_val in c(0.3, 0.9)) {
      
        age_scaled <- recruitment_indivs_ages / rec_age_shiftch; age_scaled[age_scaled>1]=1
        age_impact <- age_scaled ^ age_recruit_impact_val
      
      for (recruitment_indivs_MR in seq(0, 1, 0.1)) {
        # Compute MR chance
        MR_impact <- 1 - recruitment_indivs_MR ^ MR_recruit_impact_val
        
        # Append one row to master dataset
        results_all <- rbind(results_all,
          data.frame(
            age_recruit_impact_val = age_recruit_impact_val,
            age = recruitment_indivs_ages,
            age_impact = age_impact,
            MR_recruit_impact_val = MR_recruit_impact_val,
            MR = recruitment_indivs_MR,
            MR_impact = MR_impact
          )
        )
      }
    }
  }
}

results_all <- results_all %>% mutate(total_impact = MR_impact + age_impact - (MR_impact * age_impact) )

results_all$MR <- factor(results_all$MR)   # make MR discrete

both_plot <- ggplot(results_all, aes(x=age, y=total_impact, group=MR, colour=MR)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ age_recruit_impact_val + MR_recruit_impact_val, labeller = label_both) +
  scale_color_manual(
    values = hcl.colors(
      palette = "Burg", 
      n = length(unique(results_all$MR)))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Total recruitment impact modifier", x = "Age", colour = "MR")
both_plot

ggsave(both_plot, file="Plots/SI plots/Recruitment_MR_age_impact_value.jpeg", width = 2500, height=2700, units="px", limitsize=F)
