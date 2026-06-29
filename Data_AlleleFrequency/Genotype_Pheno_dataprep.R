#################################################################################################
########### Calculating effect size of each snp #################################################
#################################################################################################

library(tidyverse)
library(dartR)

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




####################################

### Manual effect sizes?
# Top 20 SNPs
n_snps=500

effect_size <- read.csv("~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/SNP_eff_dom_size.csv", header = F) 
effect_size <- effect_size[!grepl("climate", effect_size$V1), ]%>% 
  arrange(V2, desc = T) %>% 
  slice_head(n=n_snps)


# Run on WGS individuals

Valsamps <- read.csv("~/Uni/Doctorate/Samples/Genotyping/Report-DMela25-10229/Report_DMela25-10229_ValidationSamples/raw/Report_DMela25-10229_SNP.csv", na.strings = "-")
Valsamps_gt <- Valsamps[-c(1:6), -c(1:16)]
colnames(Valsamps_gt) <- Valsamps[6,-c(1:16)]

Valsamps_gt_downsamp <- Valsamps_gt[match(effect_size$V1, Valsamps[,1]),]
Valsamps_gt_downsamp_rmtechrep <- Valsamps_gt_downsamp[,!grepl("_", colnames(Valsamps_gt_downsamp))]

  # Check eff size order
effect_size$V1 == Valsamps[match(effect_size$V1, Valsamps[,1]),1]

  # Actual phenotypes
seedling_mquin_rust <- read.csv("~/RBGSyd_Technical Officer/MQuin/Processing Meta/mq_phenotypes.csv") # Rust data of seedling individuals for training
colnames(seedling_mquin_rust) <- c('LIBRARY', 'COI')

seedling_mquin_rust$COI <- (seedling_mquin_rust$COI)^0.25

seedling_mquin_rust_Library <- seedling_mquin_rust %>%
  filter(str_starts(LIBRARY, "S_")) %>% 
  mutate(ID=(str_extract(LIBRARY, "(?<=S_)\\d+"))) %>% 
  mutate(replicate="GT") %>% 
  filter(ID %in% colnames(Valsamps_gt_downsamp_rmtechrep))

  # Calculate baseline values
Valsamps_gt_downsamp_rmtechrep_rmnoCOI <- Valsamps_gt_downsamp_rmtechrep[,match(seedling_mquin_rust_Library$ID, colnames(Valsamps_gt_downsamp_rmtechrep))]

source("Functions/genotype_phenotype_v1.R")
P_to_G <- Phenotype_from_Genotype(snp_effects = effect_size$V2, dominance_effect = effect_size$V3, individuals_GT = as.list(Valsamps_gt_downsamp_rmtechrep_rmnoCOI), error=rep(0,ncol(Valsamps_gt_downsamp_rmtechrep_rmnoCOI)), phenotype_baseline = 0, set_floor=F)
P_to_G_ID <-  data.frame(ID = colnames(Valsamps_gt_downsamp_rmtechrep_rmnoCOI), PtoG = P_to_G)

P_to_G_GT <- left_join(seedling_mquin_rust_Library[,append(2,3)], P_to_G_ID, by = "ID")

ggplot(P_to_G_GT, aes(x=COI, y=PtoG)) +
  geom_point() +
  stat_smooth(method = "lm")

  # Minimise error of weights
snp_effects = effect_size$V2; dominance_effect = effect_size$V3; individuals_GT = as.list(Valsamps_gt_downsamp_rmtechrep_rmnoCOI); error=0; phenotype_baseline = 0

initial_baseline <- mean(P_to_G_GT$COI, na.rm = TRUE)
initial_params <- c(initial_baseline, rep(0.01, n_snps))

#########
# MSE
# Notice we no longer pass 'baseline' as a separate argument.
objective_function_dynamic <- function(params, snp_fx, dominance, err, gt_list, truth) {
  
  # Extract the baseline (1st element) and weights (remaining elements)
  proposed_baseline <- params[1]
  proposed_weights <- params[2:length(params)]
  
  # Generate predictions using your rewritten phenotype function
  predictions <- Phenotype_from_Genotype_weighted(
    phenotype_baseline = proposed_baseline,
    snp_effects = snp_fx,              # Raw effects
    snp_weights = proposed_weights,    # Optimized weights
    dominance_effect = dominance,
    error = err,
    individuals_GT = gt_list,
    set_floor = FALSE,                 # Keep floor OFF during training
    if_NA = 0
  )
  
  # Calculate Mean Squared Error
  mse <- mean((truth - predictions)^2, na.rm = TRUE)
  
  return(mse)
}

# 3. Run the Optimizer using Nelder-Mead
optimization_result <- optim(
  par = initial_params,
  fn = objective_function_dynamic,
  method = "Nelder-Mead",     
  snp_fx = effect_size$V2,
  dominance = effect_size$V3,  
  err = rep(0, length(P_to_G_GT$COI)), 
  gt_list = as.list(Valsamps_gt_downsamp_rmtechrep_rmnoCOI),
  truth = P_to_G_GT$COI,
  control = list(maxit = 5000, trace = 1)     # trace=1 prints progress to the console
)

# 4. Extract the results
optimal_baseline <- optimization_result$par[1]
optimal_weights <- optimization_result$par[2:length(optimization_result$par)]

# View the new baseline the model decided was best
print(optimal_baseline)

# Rerun with new weights
source("Functions/genotype_phenotype_v1.R")
P_to_G <- Phenotype_from_Genotype_weighted(snp_effects = effect_size$V2, snp_weights = optimal_weights , dominance_effect = effect_size$V3, individuals_GT = as.list(Valsamps_gt_downsamp_rmtechrep_rmnoCOI), error=rep(0,ncol(Valsamps_gt_downsamp_rmtechrep_rmnoCOI)), phenotype_baseline = 0.5 , set_floor=F)
P_to_G_ID <-  data.frame(ID = colnames(Valsamps_gt_downsamp_rmtechrep_rmnoCOI), PtoG = P_to_G)

P_to_G_GT <- left_join(seedling_mquin_rust_Library[,append(2,3)], P_to_G_ID, by = "ID")

ggplot(P_to_G_GT, aes(x=COI, y=PtoG)) +
  geom_point() +
  stat_smooth(method = "lm")

effect_size$V5 = pattern_weights

write.csv(effect_size, file = "Data_AlleleFrequency/Weighted_PtoG.csv", row.names = F)


###### Correlation
set.seed(42) # For reproducibility
initial_weights <- runif(n_snps, -1, 1)

# 2. Define the new Correlation-based objective function
objective_function_cor <- function(proposed_weights, snp_fx, dominance, err, gt_list, truth) {
  
  # Baseline is irrelevant for correlation, so we lock it at 0
  predictions <- Phenotype_from_Genotype_weighted(
    phenotype_baseline = 0,
    snp_effects = snp_fx,
    snp_weights = proposed_weights,
    dominance_effect = dominance,
    error = err,
    individuals_GT = gt_list,
    set_floor = FALSE,
    if_NA = 0
  )
  
  # Calculate Pearson correlation
  # suppressWarnings prevents console spam if the optimizer tests a flat line
  r <- suppressWarnings(cor(truth, predictions, use = "complete.obs"))
  
  # If the optimizer tries a combination that results in NA (zero variance), penalize it
  if (is.na(r)) { return(1) } 
  
  # Return negative correlation (optim minimizes, so minimizing -r maximizes r)
  return(-r)
}

# 3. Run the optimizer
optimization_result_cor <- optim(
  par = initial_weights,
  fn = objective_function_cor,
  method = "Nelder-Mead",
  snp_fx = effect_size$V2,
  dominance = effect_size$V3,  
  err = rep(0, length(P_to_G_GT$COI)), 
  gt_list = as.list(Valsamps_gt_downsamp_rmtechrep_rmnoCOI),
  truth = P_to_G_GT$COI,
  control = list(maxit = 5000)
)

# Extract the pattern-optimized weights
pattern_weights <- optimization_result_cor$par

source("Functions/genotype_phenotype_v1.R")
P_to_G <- Phenotype_from_Genotype_weighted(snp_effects = effect_size$V2, snp_weights = pattern_weights , dominance_effect = effect_size$V3, individuals_GT = as.list(Valsamps_gt_downsamp_rmtechrep_rmnoCOI), error=rep(0,ncol(Valsamps_gt_downsamp_rmtechrep_rmnoCOI)), phenotype_baseline = 0.5 , set_floor=F)
P_to_G_ID <-  data.frame(ID = colnames(Valsamps_gt_downsamp_rmtechrep_rmnoCOI), PtoG = P_to_G)

P_to_G_GT <- left_join(seedling_mquin_rust_Library[,append(2,3)], P_to_G_ID, by = "ID")

ggplot(P_to_G_GT, aes(x=COI, y=PtoG)) +
  geom_point() +
  stat_smooth(method = "lm")
