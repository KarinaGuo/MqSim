library(dartR); library(tidyverse); library(qqman)
load("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/Run_results/07072026_GAPIT_failed.RData")
theme_set(theme_bw())

SNPs_tested <- effect_size$V1
SNPs_effsize <- effect_size$V2

TP_after_AF <- AF_timepoints[[19]]
TP_after_curr <- pop_timepoints[[19]] 
young_indvs_aft <- TP_after_curr$age < 5

#

TP_after_AF_young <- TP_after_AF[young_indvs_aft]

TP_before_AF <- AF_timepoints[[2]] 
TP_before_curr <- pop_timepoints[[2]] 
young_indvs_bef <- TP_before_curr$age < 5

TP_before_AF_young <- TP_before_AF[young_indvs_bef]


### Empirical
dartgl_1row <- gl.read.dart(filename="~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/DMela2510229_merge_DMela2611752_1row.csv", nas = "-", lastmetric = "RatioAvgCountRefAvgCountSnp", ind.metafile="~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/ind_meta.csv")

empirical_postMR_AF <- gl.keep.ind(dartgl_1row, dartgl_1row@ind.names[grepl("Contemporary", dartgl_1row@pop)])
empirical_postMR_AF <- gl.keep.loc(empirical_postMR_AF, loc.list = SNPs_tested)
SNP_AF_empirical <- gl.allele.freq(empirical_postMR_AF, by="loc", verbose = 5)

  ### Pull out outlier SNPs
filter_sig <- function(data){
  data_sig <- data %>% 
    filter(is.na(filt), holm_Lifestage < 0.05, !(snp_type=="climate")) 
  return(data_sig)
}

pop_glm_HS_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_pop_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "Pop_HS")
HS_glm_FB_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_FernBay_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "FernBay_HS")
HS_glm_HN_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_HawksNest_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "HawksNest_HS")
HS_glm_WC_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_WarrellCreek_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "WarrellCreek_HS")
HS_glm_LJ_sig <- filter_sig(read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch HistSeeds/Extra_data/GLM_res_LongJetty_HS_halfsib.csv")) %>% mutate(Test = "HS", Set = "LongJetty_HS")

all_results <- bind_rows(pop_glm_HS_sig, HS_glm_FB_sig, HS_glm_HN_sig, HS_glm_WC_sig, HS_glm_LJ_sig)
union_results <- all_results %>% group_by(Set) %>% distinct(locID, .keep_all = TRUE) %>% ungroup()

###
##################### QB outlier test
run_quasibinom_glm <- function(snp_index, allele_count, total_count, popID) {
  ac <- allele_count[snp_index,]
  tc <- total_count[snp_index,]
  
  # initiate for NA
  pval_Population  <- NA
  
  # Check for zero variance
  if(var(ac) == 0) return(c(pval_TP=pval_TP))
  
  # Fit the model
  model <- glm(cbind(ac, tc - ac) ~ popID, family = "quasibinomial")
  
  if(!is.null(model)) {
    model_anova <- tryCatch({
      car::Anova(model, type = 3, test.statistic = "F")
    }, error = function(e) {
      return(NULL)
    })
    
    if(!is.null(model_anova)) {
      # Extract the respective p-values
      pval_TP   <- model_anova["popID", "Pr(>F)"]
    }
  }
  return(c(pval_TP=pval_TP))
}

##################### SNP line plot
final_prediction_gt <- read.csv("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural Selection/Simulated_SNPDirection/prediction_gt.csv")

## Where every individual is named by their artificially perturbated SNP
final_prediction_gt_sim <- final_prediction_gt[grepl("MqA", final_prediction_gt$Taxa),] 

# Pull out SNP name and direction
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
  group_by(Chromosome, Pos) %>%
  mutate(Pred_Diff = max(Prediction, na.rm = TRUE) - min(Prediction, na.rm = TRUE)) %>%
  ungroup() 


search_pattern <- paste(union_results$start_pos, collapse = "|")
final_prediction_gt_sim_mut_sigSNP <- final_prediction_gt_sim_mut[grepl(search_pattern, final_prediction_gt_sim_mut$Pos),]

final_prediction_gt_sim_mut_sigSNP <- final_prediction_gt_sim_mut_sigSNP %>% 
  group_by(Position) %>% 
  mutate(Direction = ifelse(
    Prediction[match("1/1", SNP_identity)] > Prediction[match("0/0", SNP_identity)], FALSE, # FALSE => alt homo = 2  
    TRUE
  )) %>% 
  ungroup()


SNPs_tested_ord <- match(SNP_AF_empirical$locus, SNPs_tested)
SNPs_effsize_reord <- SNPs_effsize[SNPs_tested_ord]

final_prediction_gt_sim_mut_sigSNP$locID <- gsub("[_:-]", ".", final_prediction_gt_sim_mut_sigSNP$locID)

################################################################################################
#### Bootstrap

iterations = 50
final_results_df = NULL
AF_comparison = NULL
Counts_summary = NULL

for (iter in 1:iterations) {
  #############
  # Compare AF
  
    ## Before
  TP_before_AF_downsampled <- TP_before_AF_young[sample(length(TP_before_AF_young), 124)] # Number of seedlings sampled in HS experiment for Historical dataset
  
    ## After
  TP_after_AF_downsampled <- TP_after_AF_young[sample(length(TP_after_AF_young), 130)] # Number of seedlings sampled in HS experiment for Contemporary dataset
  
  total_alt_alleles <- Reduce(`+`, TP_after_AF_downsampled) # Sum the alternate alleles across all individuals for every SNP
  total_alleles <- 2 * length(TP_after_AF_downsampled) # Calculate the total number of alleles in the population (2 alleles per individual)
  allele_frequencies_sim <- total_alt_alleles / total_alleles # Calculate the frequencies
  
  ## check order
  AF_comparison_set <- data.frame(locID = SNP_AF_empirical$locus, empirical_AF = SNP_AF_empirical$frequency, sim_AF = allele_frequencies_sim, eff_size = SNPs_effsize_reord) 
  AF_comparison_set <- AF_comparison_set %>% 
    mutate(AF_diff = empirical_AF - sim_AF,
           SNP_Type = str_extract(AF_comparison_set$locID, "[^_]+$"),
           outlier_SNP = case_when(
             locID %in% union_results$locID ~ TRUE, 
             TRUE ~ FALSE
           ),
           iteration = iter)
  
  AF_comparison <- data.frame(rbind(AF_comparison, AF_comparison_set))

  #############
  # Run FsT outlier test on the two allele matrices
  
  TP_before_AF_matrix <- as.data.frame(lapply(TP_before_AF_downsampled, I)) # Convert AF list of lists to df
  TP_after_AF_matrix <- as.data.frame(lapply(TP_after_AF_downsampled, I))
  
  TP_AF_matrix <- as.matrix(cbind(TP_after_AF_matrix, TP_before_AF_matrix))
  popID <- c(rep("post", ncol(TP_after_AF_matrix)), rep("pre", ncol(TP_before_AF_matrix)))
  
  allele_counts_matrix <- (2 * (TP_AF_matrix == 0)) + (1 * (TP_AF_matrix == 1))
  allele_counts_matrix[is.na(allele_counts_matrix)] <- 0
  
  total_counts_matrix <- matrix(2, nrow = nrow(TP_AF_matrix), ncol = ncol(TP_AF_matrix))
  total_counts_matrix[is.na(TP_AF_matrix)] <- 0
  
  ### Run QB GLM
  pval_TP = NA
  results_list <- lapply(1:nrow(allele_counts_matrix), function(i) {
    if (i%%100==0){cat("Running, ",i,"\n")}
    run_quasibinom_glm(snp_index = i, allele_count = allele_counts_matrix, total_count = total_counts_matrix, popID = popID )
  })
  
  final_results_df_set <- as.data.frame(do.call(rbind, results_list))
  colnames(final_results_df_set) <- c("Timepoint")
  final_results_df_set$holm_Timepoint <- p.adjust(final_results_df_set$Timepoint, method = "holm")
  
  final_results_df_set <- cbind(locID = AF_comparison_set$locID, final_results_df_set) 
  
  final_results_df <- rbind(final_results_df, final_results_df_set |> mutate(iteration = iter))

  #############
  # SNP Line plot
  locID <- AF_comparison_set$locID
  unique_sigSNP_locID <- unique(union_results$locID)
  
  gt_rel_loc <- data.frame(t(TP_AF_matrix)[,(locID %in% unique_sigSNP_locID)])
  
  colnames(gt_rel_loc) <- locID[locID %in% unique_sigSNP_locID]
  gt_rel_loc$group <- popID
  
  gt_rel_loc_long <- data.frame(gt_rel_loc) %>%
    pivot_longer(
      cols = -group, 
      names_to = "locID", 
      values_to = "genotype"
    )
  
  gt_rel_loc_long$locID <- gsub("_", ".", gt_rel_loc_long$locID)
  
  gt_rel_loc_long_dir <- left_join(gt_rel_loc_long, unique(final_prediction_gt_sim_mut_sigSNP %>% dplyr::select(locID, Direction, Pred_Diff)))
  
  total_counts_summary <- gt_rel_loc_long_dir %>%
    filter(!is.na(genotype)) %>% 
    group_by(locID) %>% 
    summarise(n_indv_gt = n())
  
  allele_counts_summary <- gt_rel_loc_long_dir %>%
    filter(!is.na(genotype)) %>% 
    mutate(
      # Determine how many alleles each genotype represents based on Direction
      allele_count = case_when(
        Direction == TRUE  & genotype == "0" ~ 2,
        Direction == TRUE  & genotype == "1" ~ 1,
        Direction == TRUE  & genotype == "2" ~ 0,
        Direction == FALSE & genotype == "2" ~ 2,
        Direction == FALSE & genotype == "1" ~ 1,
        Direction == FALSE & genotype == "0" ~ 0
      )
    ) %>%
    group_by(locID, group) %>%
    summarise(
      Sum_Alleles = sum(allele_count, na.rm = TRUE))  %>% 
    ungroup()
  
  Counts_summary_set <- left_join(allele_counts_summary, total_counts_summary) %>% 
    mutate(Prop_Sum_Alleles = Sum_Alleles/(3*n_indv_gt))
  Counts_summary_set <- left_join(Counts_summary_set, unique(final_prediction_gt_sim_mut_sigSNP %>%
                                                       dplyr::select(locID, Pred_Diff))) %>%
    mutate(iteration = iter)
  
  Counts_summary <- data.frame(rbind(Counts_summary, Counts_summary_set))
}

final_results_df <- final_results_df %>% 
  tidyr::extract(
    col = locID, 
    into = c("chr", "start_pos", "end_pos", "snp_type"),
    regex = "^(.+):(\\d+)-(\\d+)_(.+)$",
    remove = FALSE
  )

Counts_summary <- Counts_summary %>%
  separate(
    col = locID,
    into = c("Prefix", "Chromosome", "Start", "End", "SNP_Type"),
    sep = "\\.",     
    remove = FALSE,  
    convert = TRUE   
  ) %>%
  mutate(
    Position = Start + 150,
    loc_label = str_glue("{Chromosome} : {Position} - {SNP_Type}")
  )

##########
## Final datasets
AF_comparison # AF comps
final_results_df # QB binomial GLM
Counts_summary # AF shifts

final_results_df_sim <- final_results_df %>% 
  filter(holm_Timepoint < 0.05, !(snp_type=="climate")) 

sim_outliers <- final_results_df_sim %>%
  select(locID, iteration) %>%
  distinct() %>%
  mutate(is_sim_outlier = TRUE)

AF_comparison_join <- AF_comparison %>%
  select(-any_of("outlier_SNP_sim")) %>% 
  left_join(sim_outliers, by = c("locID", "iteration")) %>%
  mutate(
    outlier_SNP_sim = if_else(is.na(is_sim_outlier), FALSE, TRUE)
  ) 

ggplot(AF_comparison_join, aes(x=empirical_AF, y=sim_AF, colour = eff_size)) +
  geom_point(alpha = 0.5) +
  geom_point(data=(AF_comparison_join |> dplyr::filter(outlier_SNP_sim == TRUE)), colour="blue", shape=1, fill = "white" ) +
  geom_point(data=(AF_comparison_join |> dplyr::filter(outlier_SNP == TRUE)), colour="red", shape=1, fill = "white") +
  geom_point(data=(AF_comparison_join |> dplyr::filter(outlier_SNP == TRUE & outlier_SNP_sim == TRUE)), colour="green") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ggtitle(paste("AF comparisons at time", unique(TP_after_curr$time))) +
  labs(caption = "Red = HS outlier SNPs\nBlue = simulated outlier SNPs") +
  facet_wrap(~iteration)  


################
# Investigating sensitivity
AF_comparison # AF comps
final_results_df_sim # QB binomial GLM
Counts_summary # AF shifts

  #### Does the iteration significantly alter the correlation?

summary(lm(data = AF_comparison, formula = sim_AF ~ empirical_AF + iteration))

  #### Does the same significant SNPs show up across iterations? 

SigSNP_count <- final_results_df_sim |> 
  group_by(locID) |> 
  summarise(n = n())
hist(SigSNP_count$n)

  #### Which ones are repeats?

high_rep_SigSNPs <- SigSNP_count |> filter(n>45)
high_rep_SigSNPs_meta <- left_join(high_rep_SigSNPs, final_results_df_sim |> dplyr::select(locID, chr, start_pos, end_pos, snp_type))
print(high_rep_SigSNPs, n=100)

  #### Plot them on the Manhattan plot?

file <- "/data/iter_4-gwas_2-COI.txt"
qqdata <- read.table(paste0("~/Uni/Doctorate/Ch Hist_Nat/Ch Natural Selection", file), sep="\t",header=TRUE)

dart_tag_SNPs <- data.frame(locnames = dartgl_1row@loc.names)  %>% 
  mutate(chr = str_extract(locnames, "^[^:]+"), first_num = as.numeric(str_extract(locnames, "(?<=:)\\d+")), ps = first_num + 150)

gwasResults <- data.frame(SNP=paste0("snp",rownames(qqdata)), CHR=as.integer(substr(qqdata$chr, 8,9)), BP=qqdata$ps, P=qqdata$p_wald)
gwasResults_sort <- gwasResults %>% arrange(CHR, BP) %>% mutate(group = ifelse(CHR %% 2 == 0, "even", "odd"))
gwasResults_sort$index <- seq(1, nrow(gwasResults_sort), 1)

high_rep_SigSNPs_meta$ps <- as.numeric(high_rep_SigSNPs_meta$start_pos)+150
outlier_snps_plotdetail <- high_rep_SigSNPs_meta %>% dplyr::select(chr, ps) %>% mutate(Outlier = TRUE) 
outlier_snps_plotdetail$chr <- substr(outlier_snps_plotdetail$chr, 8,9)
outlier_snps_plotdetail$chr_ps <- paste(as.numeric(outlier_snps_plotdetail$chr), outlier_snps_plotdetail$ps,sep= "_")

dart_tag_SNPs_plotdetail <- dart_tag_SNPs %>% dplyr::select(chr, ps) %>% mutate(Outlier = FALSE)
dart_tag_SNPs_plotdetail$chr <- substr(dart_tag_SNPs_plotdetail$chr, 8,9)
dart_tag_SNPs_plotdetail$chr_ps <- paste(as.numeric(dart_tag_SNPs_plotdetail$chr), dart_tag_SNPs_plotdetail$ps,sep= "_")
dart_tag_SNPs_plotdetail <- dart_tag_SNPs_plotdetail %>%   filter(!(chr_ps %in% outlier_snps_plotdetail$chr_ps))

gwasResults_keep <- gwasResults_sort %>% 
  mutate(chr_ps = paste(as.numeric(CHR), BP,sep= "_")) %>% 
  filter(chr_ps %in% outlier_snps_plotdetail$chr_ps | chr_ps %in% dart_tag_SNPs_plotdetail$chr_ps)
gwasResults_thin <- gwasResults_sort[sample(nrow(gwasResults_sort), 30000, replace = FALSE), ]  %>% 
  mutate(Outlier = NA)

colnames(outlier_snps_plotdetail)[1:2] <- c("CHR", "BP")
outlier_snps_plotdetail$CHR <- as.numeric(outlier_snps_plotdetail$CHR)

colnames(dart_tag_SNPs_plotdetail)[1:2] <- c("CHR", "BP")
dart_tag_SNPs_plotdetail$CHR <- as.numeric(dart_tag_SNPs_plotdetail$CHR)
combined_snps <- bind_rows(outlier_snps_plotdetail, dart_tag_SNPs_plotdetail)

gwasResults_keep <- left_join(gwasResults_keep, combined_snps) %>%
  dplyr::select(-c(chr_ps))
gwasResults_plot <- rbind(gwasResults_keep, gwasResults_thin) %>% unique()

xlabel = "Chromosome"
labs_POS <- gwasResults_plot %>% group_by (CHR) %>% summarise(index = median(index))
ticks = as.character(unname(unlist(labs_POS[,1])))
lab_POS = as.numeric(unname(unlist(floor(labs_POS[,2]))))

Manhattan <- ggplot() + 
  # geom_vline(xintercept = 1790398, linewidth = 10, colour = "firebrick", alpha = 0.1) +
  geom_point(data = gwasResults_plot, aes(x = as.integer(index), y = -log10(P), colour = "All SNPs"), size = 0.5) + 
  geom_point(data = gwasResults_plot %>% filter(Outlier == FALSE, !is.na(Outlier)), aes(x = as.integer(index), y = -log10(P), colour = "SNP Panel"), size = 0.6) + 
  geom_point(data = gwasResults_plot %>% filter(Outlier == TRUE), aes(x = as.integer(index), y = -log10(P)), colour = "white", size = 2) + 
  geom_point(data = gwasResults_plot %>% filter(Outlier == TRUE), aes(x = as.integer(index), y = -log10(P), colour = "High repeat sigSNP"), size = 1, ) + 
  scale_x_continuous(breaks = lab_POS, labels = as.integer(ticks)) +
  theme(axis.text.x = element_text(size = 10), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")) +
  scale_color_manual(name = "SNP Set", values = c("All SNPs"  = "grey95", "SNP Panel" = "grey70",  "High repeat sigSNP" = "#DF536B")) +
  labs(x = 'Chromosome') 
Manhattan

  #### Plotting the SNP line plot by mean + variance
Counts_summary$group <- factor(Counts_summary$group, levels = c("pre", "post"))
Counts_summary_itersum <- Counts_summary |> 
  group_by(locID, group) |> 
  summarise(mean_Prop_sum = mean(Prop_Sum_Alleles),
            sd = sd(Prop_Sum_Alleles))

ggplot(Counts_summary_itersum, aes(x = group, y = mean_Prop_sum, group = locID, color = locID)) +
  geom_errorbar(aes(ymin = mean_Prop_sum - sd, ymax = mean_Prop_sum + sd),  width = 0.05, linewidth = 0.6,  alpha = 0.7) +
  geom_point(size = 1.5) +
  geom_line(alpha = 0.5)
