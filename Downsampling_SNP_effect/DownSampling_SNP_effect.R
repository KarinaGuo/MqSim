#############################################################################################################################
### Simulate SNP datasets for downsampling SNPs impact on GEBV  #############################################################
#	• To find the effect of downsampling, we resample the base set 20 times. at 50%, 25%, 10%, 5%, 1% #########################
# ○ Other SNPs are held stable
#############################################################################################################################

require(tidyverse)
require(patchwork)
library(dartR)

#############################
##Training dataset
hapmap_to_numeric <- function(hapmap_df, metadata_cols = 11) {
  # 1. Separate the genotype data from the metadata
  geno_data <- hapmap_df[, (metadata_cols + 1):ncol(hapmap_df)]
  
  # 2. Extract reference and alternate alleles (Assuming format "Ref/Alt" in column 2)
  # Adjust the index if your 'alleles' column is located elsewhere
  alleles_split <- strsplit(as.character(hapmap_df[, 2]), "/")
  ref_allele <- sapply(alleles_split, `[`, 1)
  alt_allele <- sapply(alleles_split, `[`, 2)
  
  # 3. Initialize an empty numeric matrix
  num_matrix <- matrix(NA, nrow = nrow(geno_data), ncol = ncol(geno_data))
  colnames(num_matrix) <- colnames(geno_data)
  rownames(num_matrix) <- hapmap_df[, 1] # Assuming column 1 is the SNP ID (rs#)
  
  # 4. Loop through each SNP (row) to assign dosages
  for (i in 1:nrow(geno_data)) {
    ref <- ref_allele[i]
    alt <- alt_allele[i]
    
    # Define possible genotype strings
    hom_ref <- paste0(ref, ref)
    het1    <- paste0(ref, alt)
    het2    <- paste0(alt, ref)
    hom_alt <- paste0(alt, alt)
    
    row_genotypes <- as.character(geno_data[i, ])
    
    # Assign dosages
    num_matrix[i, row_genotypes == hom_ref] <- 0
    num_matrix[i, row_genotypes %in% c(het1, het2)] <- 1
    num_matrix[i, row_genotypes == hom_alt] <- 2
    # Anything else (like "NN") remains NA
  }
  
  return(num_matrix)
}

my_hapmap <- read.table("data.hmp.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")
numeric_geno <- hapmap_to_numeric(my_hapmap)

#############################
##Base dataset

dart_data <- read.csv("~/Uni/Doctorate/Samples/Genotyping/Report-DMela25-10229/Report_DMela25-10229_ValidationSamples/raw/Report_DMela25-10229_SNP.csv", stringsAsFactors = FALSE, check.names = FALSE)
techreps <- grepl("_", dart_data[6,])

dart_data_rmtechs <- dart_data [, !techreps]
geno_start_col <- 17
metadata_cols <- dart_data_rmtechs[, 1:(geno_start_col - 1)]

geno_cols <- dart_data_rmtechs[-c(1:6), geno_start_col:ncol(dart_data_rmtechs)]
inds <- dart_data_rmtechs[6, geno_start_col:ncol(dart_data_rmtechs)]

locID <- dart_data_rmtechs[-c(1:6), 1]
locs_gsub <- gsub(":", "_",locID) 
chr_full <- str_extract(locID, "^[^:]+"); chr <- as.numeric(gsub("MqA_CHR", "", chr_full))
pos <- as.numeric(str_extract(locID, "(?<=:)\\d+")) + 150
locs <- data.frame(SNP=locs_gsub, Chromosome=chr, Position=pos)


write.table(locs , "~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/locs_numeric.txt", row.names = F)

geno_matrix <- as.matrix(geno_cols)
converted_matrix <- matrix(NA_integer_, 
                           nrow = nrow(geno_matrix), 
                           ncol = ncol(geno_matrix),
                           dimnames = dimnames(geno_matrix))

converted_matrix[geno_matrix == "0"] <- 0
converted_matrix[geno_matrix == "1"] <- 2
converted_matrix[geno_matrix == "2"] <- 1

converted_matrix <- cbind(as.vector(t(inds)), t(converted_matrix))

colnames(converted_matrix) <- append("taxa", locs_gsub)
converted_df <- as.data.frame(converted_matrix)
converted_df[,1] <- as.character(converted_df[,1])
converted_df[,-1] <- lapply(converted_df[,-1], function(x) as.numeric(as.character(x)))

write.table(converted_matrix , "~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/DMela25-10229_numeric_rmtechreps.txt", row.names = F)

#########
# Run base GAPIT - initial ground truth data
source("~/Uni/Doctorate/gapit_functions_080425.txt")
#Import files
input_phenodatafile="~/RBGSyd_Technical Officer/MQuin/Processing Meta/mq_phenotypes.csv"
seedling_mquin_rust <- read.csv(input_phenodatafile, sep = ",") # Rust data of seedling individuals
sample_lib_UID <- sub("^S_(\\d+).*", "\\1", seedling_mquin_rust$library) # UID of library
seedling_mquin_rust[,1] <- sample_lib_UID

# filter UID to present in base validation dataset
seedling_mquin_rust_pres <- seedling_mquin_rust[(sample_lib_UID %in% as.vector(t(inds))),]


colnames(seedling_mquin_rust_pres) <- c('LIBRARY', 'COI')

Y <- seedling_mquin_rust_pres; Y$COI <- (Y$COI)^0.25
chromosome_mapping <- c("MQA_CHR01" = 1, "MQA_CHR02" = 2,"MQA_CHR03" = 3, "MQA_CHR04" = 4, "MQA_CHR05" = 5, "MQA_CHR06" = 6, "MQA_CHR07" = 7, "MQA_CHR08" = 8, "MQA_CHR09" = 9, "MQA_CHR10" = 10, "MQA_CHR11" = 11)

initialbase_GAPIT <- GAPIT(Y=Y, GD=converted_df, GM=locs, PCA.total=3, model=c("gBLUP"), file.output = F, SNP.impute = "Major")





####################################
source("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Samples/Genotyping/Code/GP_functs.R")

### Initial set

base_dir=paste(base_dir_gt, "Merged_datasets/", sep="/")
log_file="~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/log.txt"
out_dir="~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/Pred_res/"
gt_datafile = paste(base_dir_gt, "Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping", sep="/", "Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt")
input_phenodatafile="~/RBGSyd_Technical Officer/MQuin/Processing Meta/mq_phenotypes.csv"

run_genomic_prediction(gt_datafile = gt_datafile, input_phenodatafile = input_phenodatafile, trans_COI=TRUE, rm_clim_snp=TRUE, homo_site_filt=1, miss_site_filt=1, homo_samp_filt=1, miss_samp_filt=1, log_file = log_file, out_dir = out_dir, filename="base_prediction_gt.csv")


### Downsample hapmap
df_base <- read.csv(paste(base_dir_gt, "Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping", sep="/", "Report_DMela25-10229_GenotypingSamples_trainingconcat_sort.hapmap.hmp.txt"), sep = "\t");
df_base_rmclim <- df_base[!grepl("climate", df_base$rs.), ]
total_snps <- nrow(df_base_rmclim)

remove_snp_df=NULL
for (set_num in c(.5,.25,.1,.05,.01)){
  set  = ifelse(set_num == .5, "Set1", ifelse(set_num == .25, "Set2", ifelse(set_num == .1, "Set3", ifelse(set_num == .05, "Set4", "Set5"))))
                                                                           
  keep_count = floor(total_snps * set_num) # number of snps to keep
  
  for (iter in 1:20){
    cat("Running iteration", iter, "for set", set, "\n")
    
    snp_keeps <- sample(seq_len(total_snps), size = keep_count, replace = FALSE)
    reduced_hapmap <- df_base_rmclim[snp_keeps, ]
    
    # Removed snp identity
    snp_rmed <- !(df_base_rmclim$rs. %in% reduced_hapmap$rs.); rm_snps <- df_base_rmclim$rs.[snp_rmed]
    rm_snp_inf <- data.frame(snp = rm_snps) %>% 
      tidyr::extract(
        col = snp, 
        into = c("chr", "start_pos", "end_pos", "snp_type"),
        regex = "^(.+):(\\d+)-(\\d+)_(.+)$",
        remove = FALSE # Set to TRUE if you want to drop the original locID column
      )
    
    # Track removed snps
    new_row <- data.frame(Set = set, Iteration = iter, as.list(table(rm_snp_inf$snp_type)))
    remove_snp_df <- bind_rows(remove_snp_df, new_row)
    
    # write temporary hapmap
    write.table(reduced_hapmap, file = "~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/tmp_reduced.hapmap.hmp.txt", row.names = F, sep = "\t")
    gt_datafile = "~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/tmp_reduced.hapmap.hmp.txt"
    
    filename = paste0("prediction_gt_", set, "_",iter, ".csv")
    
    run_genomic_prediction(gt_datafile = gt_datafile, input_phenodatafile = input_phenodatafile, trans_COI=TRUE, rm_clim_snp=TRUE, homo_site_filt=1, miss_site_filt=1, homo_samp_filt=1, miss_samp_filt=1, log_file = log_file, out_dir = out_dir, filename=filename)
    
  }
}

final_res <- remove_snp_df
final_res$SNP_percentage_removed <-  as.numeric(ifelse(final_res$Set == "Set1", .5, ifelse(final_res$Set == "Set2", .25, ifelse(final_res$Set == "Set3", .1, ifelse(final_res$Set == "Set4", .05, .01)))))

write.csv(final_res, file = "~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/remove_snp_df.csv", row.names=F)

##########
setwd("~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/Pred_res")
file_list <- list.files(path = ".", pattern = "^prediction_gt_.*\\.csv$", full.names = TRUE)
file_list <- file_list[basename(file_list) != "base_prediction_gt.csv"] # remove base prediction file

data_list <- lapply(file_list, function(filepath) {
  fname <- basename(filepath)
  set_val <- sub("prediction_gt_(.*)_.*\\.csv", "\\1", fname)
  set_num <- gsub("Set", "", set_val)
  iter_val <- as.integer(sub("prediction_gt_.*_(.*)\\.csv", "\\1", fname))

  df <- read.csv(filepath)
  df %>%
    dplyr::select(Taxa, Prediction, PEV) %>%
    mutate(Set = set_val, Set_num = set_num, Iteration = iter_val)
})

final_combined_df <- bind_rows(data_list)

#########

base_prediction_gt <- read.csv("~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/Pred_res/base_prediction_gt.csv") %>% 
  dplyr::select(Taxa, Prediction, PEV)
SNP_rm <- read.csv("~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/remove_snp_df.csv")

colnames(base_prediction_gt)[2:3] <- paste0("Base_", colnames(base_prediction_gt)[2:3])

final_combined_df_base <- left_join(final_combined_df, SNP_rm)

final_combined_df_base <- left_join(final_combined_df_base, base_prediction_gt)
final_combined_df_base$Prediction_difference = final_combined_df_base$Base_Prediction - final_combined_df_base$Prediction
final_combined_df_base$PEV_difference = final_combined_df_base$Base_Prediction - final_combined_df_base$Prediction

ggplot(final_combined_df_base, aes(x=Set_num, y=(Prediction_difference), fill = as.factor(Iteration))) +
  geom_boxplot()

ggplot(final_combined_df_base, aes(x=Set_num, y=(Prediction_difference))) +
  geom_boxplot()

ggplot(final_combined_df_base, aes(x=Set_num, y=PEV_difference, group=Set_num)) +
  geom_boxplot()

ggplot(final_combined_df_base, aes(x = as.factor(MR), y = Prediction_difference)) +
  geom_boxplot(aes(fill = as.factor(Iteration), alpha = 0.2)) + 
  stat_smooth(aes(group = 1), se=F, method = "lm", colour = "red", alpha = 0.5, linewidth = 1, linetype = "dashed") +
  facet_wrap(~Set, scales = "free")

ggplot(final_combined_df_base, aes(x=set_num, y=Prediction_difference)) +
  geom_boxplot()


load("~/Uni/Doctorate/Ch Natural selection/Simulation/Downsampling_SNP_effect/Pred_res/150626.RData")
