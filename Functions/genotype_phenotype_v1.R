Phenotype_from_Genotype <- function(phenotype_baseline, snp_effects, dominance_effect, error, individuals_GT, set_floor = TRUE){
  
  n_individuals <- length(individuals_GT)
  
  # Pre-allocate the vector to its final size to prevent memory reallocation
  p <- numeric(n_individuals) 
  
  for (i in seq_len(n_individuals)) {
    p_i <- 0.0
    gt_vec <- individuals_GT[[i]] 
    
    for (j in seq_along(gt_vec)) {
      gt_ij <- gt_vec[j]
      
      # Skip calculations if homozygous reference (0)
      if (gt_ij == 0) next 
      
      loc <- snp_effects[j]
      
      if (gt_ij == 2) {
        p_i <- p_i + loc
      } else if (gt_ij == 1) {
        p_i <- p_i + (loc * dominance_effect[j])
      }
    }
    
    # Store the final score for individual 'i'
    p[i] <- phenotype_baseline + p_i + error[i] 
  }
  
  # Apply the zero-floor ONCE outside the loop
  if(set_floor){p[p < 0] <- 0}
  if(set_floor){p[p >= 1] <- 1}
  
  return(p)
}

Phenotype_from_genotype_GAPIT <- function(individuals_GT, SNPs_tested){
  gt_matrix <- do.call(cbind, individuals_GT)
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
  simulated_hapmap <- left_join(iupac_df, hapmap_meta)
  
  ### Append training individuals to simulated hapmap
  simualted_hapmap_trainingconcat <- left_join(simulated_hapmap, training_hapmap)
  
  write.table(simualted_hapmap_trainingconcat, file = "~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/tmp_simulated.hapmap.hmp.txt", row.names = F, sep = "\t")
  gt_datafile = "~/Uni/Doctorate/Ch Natural selection/Simulation/Data_AlleleFrequency/tmp_simulated.hapmap.hmp.txt"
  
  ### Run GAPIT
  filename = "/prediction_gt_simulated_run.csv"
  
  run_genomic_prediction_simulation(gt_datafile = gt_datafile, input_phenodatafile = input_phenodatafile, trans_COI=TRUE, rm_clim_snp=TRUE, homo_site_filt=1, miss_site_filt=1, homo_samp_filt=1, miss_samp_filt=1, log_file = log_file, out_dir = out_dir, filename=filename)
  
  pred <- read.csv(paste0(out_dir,"/prediction_gt_simulated_run.csv")); pred <- pred[grepl("X", pred$Taxa), "Prediction"]
  MR <- scales::rescale(pred, from=c(0,3), to=c(0,1))
  
  return(MR)
}