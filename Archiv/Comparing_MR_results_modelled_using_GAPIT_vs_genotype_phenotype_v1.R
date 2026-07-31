############################################################################################################
################### Comparing MR results modelled using GAPIT vs genotype_phenotype_v1.R ###################
############################################################################################################

################# PRE MR introduction


################# POST MR introduction

####### Check GAPIT vs function

# Simulated
simulated_TP_MR <- Phenotype_from_Genotype(snp_effects = effect_size$V2, dominance_effect = effect_size$V3, individuals_GT = TP_after_AF, error=TP_after_curr$error, phenotype_baseline = baseline_pheno)

# GAPIT modelled? Convert the numeric data to hapmap data. Need to encode with known ref/alt identities

## reading in snp_meta data
base_dir_gt = "~/Uni/Doctorate/Samples/Genotyping"; base_dir=paste(base_dir_gt, "Merged_datasets/", sep="/")
log_file="~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/pred_GAPIT_log.txt"
out_dir="~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency"
gt_datafile = paste(base_dir_gt, "Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping", sep="/", "Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt")
input_phenodatafile="~/RBGSyd_Technical Officer/MQuin/Processing Meta/mq_phenotypes.csv"

df_base <- read.csv(paste(base_dir_gt, "Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping", sep="/", "Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt"), sep = "\t", header = T) %>% 
  filter(rs. %in% SNPs_tested)

snp_refalt_identity <- df_base %>%
  separate(
    col = alleles, 
    into = c("REF", "ALT"), 
    sep = "/", 
    fill = "right", 
    remove = FALSE # Set to TRUE if you want to drop the original 'alleles' column
  ) %>% 
  dplyr::select(rs., REF, ALT) 

## check order
SNPs_tested_ord <- match(SNPs_tested, snp_refalt_identity$rs.) # match ref alt into order in GT
snp_refalt_identity$rs.[SNPs_tested_ord] == SNPs_tested
snp_refalt_identity_reord <- snp_refalt_identity[SNPs_tested_ord,]

## Convert simulated data to hapmap
gt_matrix <- do.call(cbind, TP_after_AF)
colnames(gt_matrix) <- paste0("Sample_", 1:ncol(gt_matrix))

ref_alleles <- snp_refalt_identity_reord$REF
alt_alleles <- snp_refalt_identity_reord$ALT

## IUPAC encoding
iupac_dict <- c(
  "AG" = "R", "GA" = "R",
  "CT" = "Y", "TC" = "Y",
  "GC" = "S", "CG" = "S",
  "AT" = "W", "TA" = "W",
  "GT" = "K", "TG" = "K",
  "AC" = "M", "CA" = "M"
)
het_codes <- iupac_dict[paste0(ref_alleles, alt_alleles)]

iupac_matrix <- matrix("N", nrow = nrow(gt_matrix), ncol = ncol(gt_matrix))
colnames(iupac_matrix) <- colnames(gt_matrix)

iupac_matrix[gt_matrix == 0] <- ref_alleles[row(gt_matrix)[gt_matrix == 0]]
iupac_matrix[gt_matrix == 1] <- het_codes[row(gt_matrix)[gt_matrix == 1]]
iupac_matrix[gt_matrix == 2] <- alt_alleles[row(gt_matrix)[gt_matrix == 2]]

iupac_matrix[is.na(iupac_matrix)] <- "N"

iupac_df <- data.frame(
  rs. = SNPs_tested,
  iupac_matrix,
  stringsAsFactors = FALSE
)

## Add in hapmap headers/meta 

hapmap_meta <- df_base[,1:11]

simulated_hapmap <- left_join(iupac_df, hapmap_meta)

### Append training individuals to simulated hapmap
training_hapmapfull <- read.csv(paste(base_dir_gt, "Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping", sep="/", "Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt"), sep = "\t")
training_hapmap <- training_hapmapfull[, append(1, grep("S_", colnames(training_hapmapfull)))]

simualted_hapmap_trainingconcat <- left_join(simulated_hapmap, training_hapmap)

write.table(simualted_hapmap_trainingconcat, file = "~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/tmp_simulated.hapmap.hmp.txt", row.names = F, sep = "\t")
gt_datafile = "~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/tmp_simulated.hapmap.hmp.txt"


### Run GAPIT
filename = "/prediction_gt_simulated_run.csv"

source("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Samples/Genotyping/Code/GP_functs.R")
run_genomic_prediction(gt_datafile = gt_datafile, input_phenodatafile = input_phenodatafile, trans_COI=TRUE, rm_clim_snp=TRUE, homo_site_filt=1, miss_site_filt=1, homo_samp_filt=1, miss_samp_filt=1, log_file = log_file, out_dir = out_dir, filename=filename)


GAPIT_pred_sim_AF <- read.csv("prediction_gt_simulated_run.csv")
GAPIT_pred_sim_AF_onlySample <- GAPIT_pred_sim_AF[grepl("Sample_", GAPIT_pred_sim_AF$Taxa),]
GAPIT_pred_sim_AF_onlySample_rescale <- scales::rescale(GAPIT_pred_sim_AF_onlySample$Prediction, from = c(0,3), to = c(0,1))

## Find which failed prediction
miss_samples_idx <- !(colnames(iupac_df)[-c(1)] %in% GAPIT_pred_sim_AF_onlySample$Taxa)
original_sample_id <- colnames(iupac_df)[-c(1)]
miss_samples <- original_sample_id[miss_samples_idx]

simulated_TP_MR_samplenames <- colnames(iupac_df)[!(colnames(iupac_df)[-c(1)] %in% miss_samples)]
simulated_TP_MR_filt <- simulated_TP_MR[!(colnames(iupac_df)[-c(1)] %in% miss_samples)]

## plot results
comp_MR <- data.frame(cbind(GAPIT_simulated = GAPIT_pred_sim_AF_onlySample_rescale, funct_simulated = simulated_TP_MR_filt))

ggplot(comp_MR, aes(x = as.numeric(GAPIT_simulated), y=as.numeric(funct_simulated))) +
  geom_point(alpha=0.1) +
  stat_smooth(method = "lm") +
  geom_abline(intercept = 0, slope =1, linetype="dashed") +
  labs(x = "GAPIT predicted from AF SNP of timepoint x", y="Genotype-to-phenotype function in timepoint x")


#### Compare against contemporary HS
source("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch HistSeeds/Code/HistSeedAF_Analysis.R")

HS_scores_contemporary <- Scores_meta_LJ %>% 
  filter(Collection_group == "Contemporary")
HS_scores_contemporary <- scales::rescale(Scores_meta_LJ$COI, from = c(0,100), to = c(0,1))

ggplot() +
  geom_boxplot(aes(x="Function simulation", y=comp_MR$funct_simulated)) +
  geom_boxplot(aes(x="GAPIT AF simulation", y=comp_MR$GAPIT_simulated)) +
  geom_boxplot(aes(x="HS empirical contemporary", y=HS_scores_contemporary)) 