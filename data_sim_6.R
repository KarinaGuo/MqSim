####################################################################################################
##################       Updating data_sim_5 to incorporate seasonality        #####################
####################################################################################################

## Each time point is split into 4, one for each season. 
## Each season is set by a mean seasonal temperature with an standard deviation

## Current day climate data is from BOM.
## Future day climate data is from CMIP.
## Climate variables: mean seasonal max temp, mean seasonal min temp, mean rainfall

## Climate impacts:
### Set thresholds of each, parabolic relationship with an optimum

## Assumptions:
### No interactive factors between the variables
### No lag effects
### No changes in MR climate adaptations

## To do:
### Iterate across all CMIP models and take the ensemble
### Allow different years and SSPs 

####################################################################################################

# 1)
## Load in climate data for a site. Split into seasons and calculate variables' mean and standard deviations
## Determine trajectory for x time points in the future - augment current day to this, pre-populate the climate dataframe


# 2) 
## Build effect curve for each variable - use the thresholds determined in the PDP

# time point match climate row. climate row -> scaler -> generates a mr impact score using pdp (climate_imp), climate_imp affects mr_imp in mortality 


####################################################################################################

## Example visualisation
MR_orig <- seq(from = 0.001, to = 1, length.out = 20)
scaler <- seq(from = -1, to = 1, length.out = 20)

MR_aug <- NULL
for (scale in scaler){
  if (scale >= 0){
    MR_aug_new <- sapply(MR_orig, function(x) x + scale * (1 - x))
  } else if (scale < 0) {
    MR_aug_new <- sapply(MR_orig, function(x) x + scale * x)
  }
  
  MR_aug <- append(MR_aug, MR_aug_new)
}

MR_aug_clim <- data.frame(
  MR_aug  = MR_aug, 
  MR_orig = rep(MR_orig, times = length(scaler)), 
  scaler  = rep(scaler, each = length(MR_orig))    
)

library(tidyverse)
ggplot(data = MR_aug_clim, aes(y=MR_aug, x =MR_orig, colour = scaler)) +
  geom_point() +
  geom_path() +
  facet_wrap(~scaler)

##

####################################################################################################
## Climate parameters

Site <- c("Cattai") # Site name
Station.Number <- c("060141") # BOM climate station number downloaded locally
Clim_FP <- "~/Uni/Doctorate/Ch Planting/data/climate_BOM_20260625/" # Filepath to downloaded BOM climate data
latitude = "-31.831259151525416"
longitude = "152.6356114"


###################################################################################################

# Building current climate range - using example station ID

Site_StationID_prec <- data.frame(cbind(Station.Number, Site))
Site_StationID_prec$Station.Number <- as.numeric(Site_StationID_prec$Station.Number)

Station_data = NULL
for (ID in Station.Number){
  fn = paste0(Clim_FP, "IDCJAC0001_", print(ID), "_Data12.csv")
  data <- read.csv(fn) 
  data <- data %>% filter (Year >= 2024 & Year < 2025)
  data[4:15] <- apply(data[4:15], 2, function(x) as.character(x)) 
  data <- tidyr::pivot_longer(data, 
                              cols = Jan:Dec,
                              names_to = "Month",
                              values_to = "Rainfall")
  data <- data %>% dplyr::select(Year, Month, Rainfall, Station.Number)
  Station_data <- rbind(Station_data, data)
}
Station_data$Rainfall <- as.numeric(Station_data$Rainfall)
Station_data$Station.Number <- as.numeric(Station_data$Station.Number)

Station_data <- left_join(Station_data, Site_StationID_prec) %>% 
  dplyr::select(-c(Station.Number))

## MeanMaxTemp

Site_StationID_meanmaxtemp <- data.frame(cbind(Station.Number, Site))
Site_StationID_meanmaxtemp$Station.Number <- as.numeric(Site_StationID_meanmaxtemp$Station.Number)

Station_data_temp=NULL
for (ID in Station.Number){
  fn = paste0(Clim_FP, "IDCJAC0002_", print(ID), "_Data12.csv")
  data <- read.csv(fn) 
  data <- data %>% filter (Year >= 2024 & Year < 2025)
  data[4:15] <- apply(data[4:15], 2, function(x) as.character(x)) 
  data <- tidyr::pivot_longer(data, 
                              cols = Jan:Dec,
                              names_to = "Month",
                              values_to = "MeanMaxTemp")
  data <- data %>% dplyr::select(Year, Month, MeanMaxTemp, Station.Number)
  Station_data_temp <- rbind(Station_data_temp, data)
}

Station_data_temp$Station.Number <- as.numeric(Station_data_temp$Station.Number)
Station_data_temp <- left_join(Station_data_temp, Site_StationID_meanmaxtemp) %>% 
  dplyr::select(-c(Station.Number))

Station_data <- left_join(Station_data, Station_data_temp)
Station_data$MeanMaxTemp <- as.numeric(Station_data$MeanMaxTemp)

## MeanMinTemp

Station.Number <- c("060141"); Site <- c("Cattai");
Site_StationID_meanmintemp <- data.frame(cbind(Station.Number, Site))
Site_StationID_meanmintemp$Station.Number <- as.numeric(Site_StationID_meanmintemp$Station.Number)

Station_data_temp=NULL
for (ID in Station.Number){
  fn = paste0(Clim_FP, "IDCJAC0004_", print(ID), "_Data12.csv")
  data <- read.csv(fn) 
  data <- data %>% filter (Year >= 2024 & Year < 2025)
  data[4:15] <- apply(data[4:15], 2, function(x) as.character(x)) 
  data <- tidyr::pivot_longer(data, 
                              cols = Jan:Dec,
                              names_to = "Month",
                              values_to = "MeanMinTemp")
  data <- data %>% dplyr::select(Year, Month, MeanMinTemp, Station.Number)
  Station_data_temp <- rbind(Station_data_temp, data)
}

Station_data_temp$Station.Number <- as.numeric(Station_data_temp$Station.Number)
Station_data_temp <- left_join(Station_data_temp, Site_StationID_meanmintemp) %>% 
  dplyr::select(-c(Station.Number))

Station_data <- left_join(Station_data, Station_data_temp)
Station_data$MeanMinTemp <- as.numeric(Station_data$MeanMinTemp)

##

Station_data$Month <- factor(
  Station_data$Month,
  levels = month.abb,           # Abbreviated month names
  ordered = TRUE
)

Station_data$Month_num <-  as.numeric(Station_data$Month)
#Station_data$Month_date <- as.Date(paste(2025, Station_data$Month_num, 1, sep = "-"))                             ## Check in future


Station_data <- Station_data %>%
  mutate(Year = as.numeric(Year) %% 100,
         Month = as.numeric(Month_num),  
        Season = case_when(
          Month %in% c(12, 1, 2) ~ "Summer",
          Month %in% 3:5         ~ "Autumn",
          Month %in% 6:8         ~ "Winter",
          Month %in% 9:11        ~ "Spring"
        )
  )

# Station_data_summary_current <- Station_data |> 
#   group_by(Season) |> 
#   filter(!is.na(Rainfall)) |> 
#   summarise (sum_rainfall = sum(Rainfall), 
#              mean_rainfall = mean(Rainfall),
#              mean_MeanMaxTemp = mean(MeanMaxTemp), 
#              mean_MeanMinTemp = mean(MeanMinTemp),
#              sd_rainfall = sd(Rainfall),
#              sd_MeanMaxTemp = sd(MeanMaxTemp),
#              sd_MeanMinTemp = sd(MeanMinTemp)) 

Station_data_summary_current <- Station_data |> 
  mutate(
    Quarter_Rainfall = Rainfall + 
      lead(Rainfall, n = 1, default = first(Rainfall)) + 
      lead(Rainfall, n = 2, default = nth(Rainfall, 2))
  ) %>%
  summarise(
    BIO16_Wettest_Quarter = max(Quarter_Rainfall),
    BIO17_Driest_Quarter = min(Quarter_Rainfall),
    .groups = "drop"
  )

####################################################################################################

# Building CMIP future projection
library(geodata)

years_clust <- c("2061-2080")
ssp_list <- c("585")

for (cluster in years_clust){
  for (ssp in ssp_list){
    climate_tile <- cmip6_tile(model = "MPI-ESM1-2-HR", ssp = ssp, time = cluster, var = "bioc", path = "Data_climate/", lon=longitude, lat = latitude)
  }
}

extracted_values <- terra::extract(climate_tile, data.frame(lon = as.numeric(longitude), lat = as.numeric(latitude)))

# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation

BIO10 <- extracted_values$`wc2.1_30s_bioc_MPI-ESM1-2-HR_ssp585_2061-2080_10`
BIO11 <- extracted_values$`wc2.1_30s_bioc_MPI-ESM1-2-HR_ssp585_2061-2080_11`
BIO12 <- extracted_values$`wc2.1_30s_bioc_MPI-ESM1-2-HR_ssp585_2061-2080_12`

Station_data_summary_current

BIO12
