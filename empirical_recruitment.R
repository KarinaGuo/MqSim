
library(ggridges)
library(ggplot2)

parent_meta <- read.csv(file="Mquin_samples_pheno.csv")

parent_COI_dist <- ggplot(parent_meta, aes(x=COI^0.25, y=FID, fill=FID), alpha=0.8) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
#ggsave(parent_COI_dist, file = "~/Delete/tmp.jpg", units = "px", limitsize = F, height=8000, width = 4000)

random_group <- parent_meta %>%
  group_by(FID) %>%
  summarise(n=n()) 
random_group_big <- random_group %>%
  filter(n>5) %>% 
  sample_n(20)
par_sub <- parent_meta %>%
  semi_join(random_group_big, by = "FID")

fid_means <- par_sub %>%
  group_by(FID) %>%
  summarise(mean_COI = mean(COI, na.rm = TRUE))

ggplot(data = par_sub, aes(x = COI, fill = FID)) +
  geom_density(alpha = 0.8) +
  geom_vline(data = fid_means, aes(xintercept = mean_COI), 
             color = "grey70", linetype = "dashed", linewidth = 0.75) +
  theme_ridges() +
  theme(legend.position = "none") +
  xlim(c(0,80)) +
  facet_wrap(~FID, scales = "free_y")