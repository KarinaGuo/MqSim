################################################################################
##################### SNP line plot
# source information from C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Ch Hist_Nat/Ch Natural selection/Simulation/post_run_analyses_plots.R

  # Generate simulated dataset of SNP directions (final_prediction_gt) 
# /Code/SNP_direction_gblup_simulation.R


### Read in dataset
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


## Plot general SNP direction of outlier SNPs

search_pattern <- paste(union_results$start_pos, collapse = "|")
final_prediction_gt_sim_mut_sigSNP <- final_prediction_gt_sim_mut[grepl(search_pattern, final_prediction_gt_sim_mut$Pos),]

final_prediction_gt_sim_mut_sigSNP <- final_prediction_gt_sim_mut_sigSNP %>% 
  group_by(Position) %>% 
  mutate(Direction = ifelse(
    Prediction[match("1/1", SNP_identity)] > Prediction[match("0/0", SNP_identity)], FALSE, # FALSE => alt homo = 2  
    TRUE
  )) %>% 
  ungroup()


##### Run across simulated individuals - time point 990, 1020
TP_before_AF_matrix <- as.data.frame(lapply(TP_before_AF_downsampled, I)) # Convert AF list of lists to df
TP_after_AF_matrix <- as.data.frame(lapply(TP_after_AF_downsampled, I))

TP_AF_matrix <- t(as.matrix(cbind(TP_after_AF_matrix, TP_before_AF_matrix)))
popID <- c(rep("post", ncol(TP_after_AF_matrix)), rep("pre", ncol(TP_before_AF_matrix)))

locID <- AF_comparison$locID

unique_sigSNP_locID <- unique(union_results$locID)

gt_rel_loc <- data.frame(TP_AF_matrix[,(locID %in% unique_sigSNP_locID)])

colnames(gt_rel_loc) <- locID[locID %in% unique_sigSNP_locID]
gt_rel_loc$group <- popID

gt_rel_loc_long <- data.frame(gt_rel_loc) %>%
  pivot_longer(
    cols = -group, 
    names_to = "locID", 
    values_to = "genotype"
  )

final_pred_sim$locID <- gsub("_", ".", final_pred_sim$locID)
final_pred_sim$locID <- gsub(":", ".", final_pred_sim$locID)
final_pred_sim$locID <- gsub("-", ".", final_pred_sim$locID)
gt_rel_loc_long$locID <- gsub("_", ".", gt_rel_loc_long$locID)

gt_rel_loc_long_dir <- left_join(gt_rel_loc_long, unique(final_pred_sim %>% dplyr::select(locID, Direction, Pred_Diff)))
  
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

Counts_summary <- left_join(allele_counts_summary, total_counts_summary) %>% 
  mutate(Prop_Sum_Alleles = Sum_Alleles/(3*n_indv_gt))
Counts_summary <- left_join(Counts_summary, unique(final_pred_sim %>%
                                                     dplyr::select(locID, Pred_Diff))) %>%
  # Split the string at every period (.)
  separate(
    col = locID,
    into = c("Prefix", "Chromosome", "Start", "End", "SNP_Type"),
    sep = "\\.",     
    remove = FALSE,  
    convert = TRUE   
  ) %>%
  # Calculate the position and stitch the final string together
  mutate(
    Position = Start + 150,
    loc_label = str_glue("{Chromosome} : {Position} - {SNP_Type}")
  )

left_labels <- Counts_summary %>%
  group_by(loc_label) %>%
  arrange(group) %>% 
  slice(1) |> 
  mutate(group = "pre")

Counts_summary$group <- factor(Counts_summary$group, levels = c("pre", "post"))

snp_lineplot <- ggplot(Counts_summary, aes(x=group, y=Prop_Sum_Alleles, colour=SNP_Type, group=loc_label)) +
  geom_point() +
  geom_path() +
  ggrepel::geom_text_repel(data = left_labels, aes(label = loc_label), hjust = 1,  nudge_x = -0.05,  direction = "y", min.segment.length = 0, segment.size = 0.2 ,show.legend = FALSE, size = 3) +
  labs(x="Collection group", y="Resistance allele count by proportion", colour="Locus") +
  coord_cartesian(clip = "off")+
  theme(legend.position = "none")

snp_lineplot

snp_lineplot_facet <- ggplot(Counts_summary, aes(x=group, y=Prop_Sum_Alleles, colour=SNP_Type, group=loc_label)) +
  geom_point() +
  geom_path() +
  labs(x="Collection group", y="Resistance allele count by proportion", colour="Locus") +
  facet_wrap(~SNP_Type) +
  theme(legend.position = "none")
  
snp_lineplot_plot <- snp_lineplot + snp_lineplot_facet + plot_layout(axes = "collect", guides = "collect")
print(snp_lineplot_plot) 
