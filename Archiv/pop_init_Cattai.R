# Read in samples
sample_meta <- read_xlsx("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Samples/All_samples.xlsx") %>% filter(!is.na(NSWID))
sample_meta <- sample_meta[1:586,] # clean
sample_meta$Active_spore_pres <- grepl("(?<!no )active MR spores", sample_meta$`MR Notes`, perl = TRUE, ignore.case = TRUE)
sample_meta$No_new_growth_pres <- grepl("no new growth", sample_meta$`MR Notes`, perl = TRUE, ignore.case = TRUE)

# GP predicted information 
prediction_gt <- read.csv("~/Uni/Doctorate/Samples/Genotyping/Report-DMela25-10229/Report_DMela25-10229_RegularGenotyping/GP_Out/prediction_gt.csv")

# Checks 
ggplot(prediction_gt, aes(x=COI,y=Prediction)) + geom_point() + geom_abline(intercept = 0, slope=1) + theme_bw() + labs(title="Training predicted COI v groundtruth COI")
ggplot((prediction_gt %>% filter(RefInf==2)), aes(x=Prediction)) + geom_histogram() + theme_bw() + labs(title="Distribution of Predicted COI")
ggplot((prediction_gt %>% filter(RefInf==2)), aes(x=PEV)) + geom_histogram() + theme_bw() + labs(title="Distribution of PEV")

colnames(prediction_gt)[2] <- "NSWID"; prediction_gt$NSWID <- as.character(prediction_gt$NSWID)

prediction_gt_meta <- inner_join(prediction_gt[prediction_gt$RefInf==2,c("NSWID", "Prediction", "PEV")], sample_meta, by="NSWID") # Prediction_gt filtered for relevant columns + rows

## Cattai individuals as pop seed
prediction_gt_meta_Cattai <- prediction_gt_meta %>% dplyr::filter(Site == "Cattai")

prediction_gt_meta_Cattai_age <- prediction_gt_meta_Cattai %>%   
  mutate(age = case_when(
    Stage_v2 == 'Seedling' ~ as.integer(runif(n(), min=1, max=2)),
    Stage_v2 == 'Subadult' ~ as.integer(runif(n(), min=3, max=7)),
    Stage_v2 == 'Adult' ~ as.integer(runif(n(), min=8, max=100))
  ))

indiv_ID <- seq(from=1, to=nrow(prediction_gt_meta_Cattai_age))

pop_df <- list(
  indiv_ID = indiv_ID,
  time = rep(1, length(indiv_ID)),
  MR = prediction_gt_meta_Cattai_age$Prediction,
  mortality = rep(0, length(indiv_ID)),
  age = prediction_gt_meta_Cattai_age$age
)
