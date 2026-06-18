#################################################################################################
########### Calculating effect size of each snp #################################################
#################################################################################################

library(tidyverse)

final_prediction_gt <- read.csv("~/Uni/Doctorate/Ch Natural selection/Simulated_SNPDirection/prediction_gt.csv")

final_prediction_gt_sim <- final_prediction_gt[grepl("MqA", final_prediction_gt$Taxa),] 

final_prediction_gt_WGS <- final_prediction_gt[!grepl("MqA", final_prediction_gt$Taxa),]  
Global_intercept <- mean(final_prediction_gt_WGS$Prediction, na.rm = TRUE)

final_prediction_gt_sim_mut <- final_prediction_gt_sim %>% 
  dplyr::select(Taxa, Prediction) %>% 
  mutate(locID = str_remove(Taxa, "_SNP.*$")) %>% 
  extract(
    col = Taxa, 
    into = c("Chromosome", "Pos", "SNPType", "SNP_identity"), 
    regex = "^(.*?):(.*?)_(.*?)_SNP(.*)$"
  ) %>%
  mutate(SNP_identity = gsub("_", "/", SNP_identity),
         Position = as.numeric(sub("-.*", "", Pos)) + 150) %>%
  
  ## Effects
  
  mutate(GenotypeClass = case_when(
    SNP_identity == "0/0" ~ "P_00",
    SNP_identity == "1/1" ~ "P_11",
    SNP_identity %in% c("0/1", "1/0") ~ "P_het"
  )) %>%
  group_by(locID, Chromosome, Pos, SNPType, GenotypeClass) %>%
  summarise(Mean_Pred = mean(Prediction, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = GenotypeClass, values_from = Mean_Pred) %>%
  mutate(
    Dominance_Effect = case_when(
      (P_11 - P_00) == 0 ~ NA_real_, 
      TRUE ~ (P_het - P_00) / (P_11 - P_00)
    ), 
    Total_Effect = P_11 - P_00,
  )

  

hist(final_prediction_gt_sim_mut$Total_Effect)

#eff_size_normalised <- final_pred_Pos_effsize$Pred_Diff / sum(final_pred_Pos_effsize$Pred_Diff, na.rm = TRUE)

# Degree of Dominance
  # If Phet​=P1/1​, the result is 1 (Allele 1 is completely dominant).
  # If Phet​ is exactly halfway between P0/0​ and P1/1​, the result is 0.5 (Codominant/Additive).
  # If Phet​=P0/0​, the result is 0 (Allele 1 is recessive).
# Dominance Deviation (d): The deviation of the heterozygous genotype from the midpoint of the two homozygotes.
# The total genotypic value (G) for an individual at a single locus is mathematically represented as:G = a ⋅ x +  d⋅ z
# Where:x is the additive regressor (e.g., 1 for A₁A₁, 0 for A₁A₂, and -1 for A₂A₂).
# z is the dominance regressor (e.g., 0 for homozygotes and 1 for the heterozygote).

#dominance_effect <- scales::rescale(dominance_df$Dominance_Effect, to = c(0,1))

## eff_size_normalised, dominance_effect

write.table(data.frame(final_prediction_gt_sim_mut$locID, final_prediction_gt_sim_mut$Total_Effect, final_prediction_gt_sim_mut$Dominance_Effect, baseline_phenotype = Global_intercept), row.names=F, col.names = F, file = "~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/SNP_eff_dom_size.csv", sep =",")



############
## Base AF
dartgl_1row <- gl.read.dart(filename="~/Uni/Doctorate/Samples/Genotyping/Report-DMela26-11752/DMela26-11752/raw/Report_DMela26-11752_SNP.csv", nas = "-", lastmetric = "RatioAvgCountRefAvgCountSnp", ind.metafile="~/Uni/Doctorate/Ch HistSeeds/Extra_data/ind_meta.csv")
historical_AF <- gl.keep.ind(dartgl_1row, dartgl_1row@ind.names[grepl("Historical", dartgl_1row@pop)])
SNP_AF <- gl.allele.freq(dartgl_1row, by="loc", verbose = 5)

write.csv(SNP_AF, file = "~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/SNP_AF.csv", row.names=F)
