## Analysis of parameter runs

# Read in
param_sets <- read.csv(file="ParamTesting/param_sets_2025-09-04.csv"); colnames(param_sets)[ncol(param_sets)] <- "param_iteration"
run_status <- read.csv(file="ParamTesting/run_status_2025-09-04.csv")
run_res <- read.csv(file="ParamTesting/run_res_2025-09-04.csv")
run_res_LS <- read.csv(file="ParamTesting/run_res_LS_2025-09-04.csv")

## Did any runs fail
unique(run_status$status)

## Run res across all params
run_res$timeperiod <- factor(run_res$timeperiod, levels = c("Before", "Soon", "After"))
run_res <- run_res %>% mutate(timeperiod_num = case_when(
  timeperiod == "Before" ~ 1,
  timeperiod == "Soon" ~ 2,
  timeperiod == "After" ~ 3
)) 
run_res_param <- left_join(run_res, param_sets)

ggplot(run_res_param, aes(y=pop_struct, x=MR_death_impact, colour=age_impact)) +
  #geom_histogram() +
  geom_point() +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="Population structure (age)")

ggplot(run_res_param, aes(y=pop_size, x=MR_death_impact, colour=age_impact)) +
  #geom_histogram() +
  geom_point() +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="Population structure (size)")

ggplot() +
  geom_point(data=run_res_param, aes(y=mean_MR, x=MR_death_impact, colour=age_impact)) +
  geom_errorbar(data=run_res_param, aes(ymin=mean_MR-sd_MR, ymax=mean_MR+sd_MR, x=MR_death_impact, colour=age_impact)) +
  facet_wrap (~timeperiod) +
  theme_bw() +
  labs(title="MR (mean +/- sd)")



ggplot(run_res_param, aes(y=pop_struct, x=timeperiod_num, group=param_iteration, colour=MR_death_impact)) +
  #geom_histogram() +
  geom_line() +
  theme_bw() +
  labs("Population size")
