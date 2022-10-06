# preparations --------------------------------------
rm(list = ls())
library(tidyverse)
library(skimr)
# library(data.table)
setwd("~/R/Multilab_Analysis")

# write function for summary --------------------------------------
# adjusted from skim() function
mean_ML = function(x) mean(x, na.rm = TRUE)
sd_ML = function(x) sd(x, na.rm = TRUE)
median_ML = function(x) median(x, na.rm = TRUE)
min_ML = function(x) min(x, na.rm = TRUE)
max_ML = function(x) max(x, na.rm = TRUE)

ML_skim <- skim_with(numeric = sfl(n = length, mean = mean_ML, sd = sd_ML, median = median_ML, min =  min_ML, max = max_ML), append = FALSE)

# import data --------------------------------------
data_all_TC10 <- read.csv("data/eyetracking/Exp01_Exp03_GazeBehavior_TC10_Rready.csv", encoding = "UTF-8")
names(data_all_TC10) <- c("Exp", "HMI", "VPNr", "TC10_comment", "TC10_valid", "TC10_EB_triggered", 
                          "TC10_lvl_notification", "TC10_lvl_end", "TC10_TimeToL0", "TC10_nth_gaze_to_ic", 
                          "TC10_n_gazes_till_EB_or_TO", "TC10_ic_glance_allocation_time", "TC10_ic_glance_at_start", 
                          "TC10_ic_n_glances_after_start", "TC10_ic_total_duration", "TC10_ic_mean_duration", 
                          "TC10_ic_max_duration", "TC10_ic_1st_glance_duration_incl_start", 
                          "TC10_ic_1st_glance_duration_without_start", "TC10_street_glance_at_start", 
                          "TC10_street_n_glances_after_start", "TC10_street_total_duration", 
                          "TC10_street_mean_duration", "TC10_street_max_duration", 
                          "TC10_street_1st_glance_duration_incl_start", 
                          "TC10_street_1st_glance_duration_without_start", "TC10_surt_glance_at_start", 
                          "TC10_surt_n_glances_after_start", "TC10_surt_total_duration", "TC10_surt_mean_duration", 
                          "TC10_surt_max_duration", "TC10_surt_1st_glance_duration_incl_start", 
                          "TC10_surt_1st_glance_duration_without_start", "TC10_wheel_glance_at_start", 
                          "TC10_wheel_n_glances_after_start", "TC10_wheel_total_duration", "TC10_wheel_mean_duration", 
                          "TC10_wheel_max_duration", "TC10_wheel_1st_glance_duration_incl_start", 
                          "TC10_wheel_1st_glance_duration_without_start")

data_all_TC10 <- data_all_TC10 %>%
  mutate(TC10_EB_triggered = ifelse(TC10_valid == 0, NA, TC10_EB_triggered)) %>%
  mutate(TC10_lvl_notification = ifelse(TC10_valid == 0, NA, TC10_lvl_notification)) %>%
  mutate(TC10_lvl_end = ifelse(TC10_valid == 0, NA, TC10_lvl_end)) %>%
  mutate(TC10_TimeToL0 = ifelse(TC10_valid == 0, NA, TC10_TimeToL0)) %>%
  mutate(TC10_nth_gaze_to_ic = ifelse(TC10_valid == 0, NA, TC10_nth_gaze_to_ic)) %>%
  mutate(TC10_n_gazes_till_EB_or_TO = ifelse(TC10_valid == 0, NA, TC10_n_gazes_till_EB_or_TO)) %>%
  mutate(TC10_ic_glance_allocation_time = ifelse(TC10_valid == 0, NA, TC10_ic_glance_allocation_time)) %>%
  mutate(TC10_ic_glance_at_start = ifelse(TC10_valid == 0, NA, TC10_ic_glance_at_start)) %>% # AOI ic
  mutate(TC10_ic_n_glances_after_start = ifelse(TC10_valid == 0, NA, TC10_ic_n_glances_after_start)) %>%
  mutate(TC10_ic_total_duration = ifelse(TC10_valid == 0, NA, TC10_ic_total_duration)) %>%
  mutate(TC10_ic_mean_duration = ifelse(TC10_valid == 0, NA, TC10_ic_mean_duration)) %>%
  mutate(TC10_ic_max_duration = ifelse(TC10_valid == 0, NA, TC10_ic_max_duration)) %>%
  mutate(TC10_ic_1st_glance_duration_incl_start = ifelse(TC10_valid == 0, NA, TC10_ic_1st_glance_duration_incl_start)) %>%
  mutate(TC10_ic_1st_glance_duration_without_start = ifelse(TC10_valid == 0, NA, TC10_ic_1st_glance_duration_without_start)) %>%
  mutate(TC10_street_glance_at_start = ifelse(TC10_valid == 0, NA, TC10_street_glance_at_start)) %>%  # AOI street
  mutate(TC10_street_n_glances_after_start = ifelse(TC10_valid == 0, NA, TC10_street_n_glances_after_start)) %>%
  mutate(TC10_street_total_duration = ifelse(TC10_valid == 0, NA, TC10_street_total_duration)) %>%
  mutate(TC10_street_mean_duration = ifelse(TC10_valid == 0, NA, TC10_street_mean_duration)) %>%
  mutate(TC10_street_max_duration = ifelse(TC10_valid == 0, NA, TC10_street_max_duration)) %>%
  mutate(TC10_street_1st_glance_duration_incl_start = ifelse(TC10_valid == 0, NA, TC10_street_1st_glance_duration_incl_start)) %>%
  mutate(TC10_street_1st_glance_duration_without_start = ifelse(TC10_valid == 0, NA, TC10_street_1st_glance_duration_without_start)) %>%
  mutate(TC10_surt_glance_at_start = ifelse(TC10_valid == 0, NA, TC10_surt_glance_at_start)) %>%  # AOI surt
  mutate(TC10_surt_n_glances_after_start = ifelse(TC10_valid == 0, NA, TC10_surt_n_glances_after_start)) %>%
  mutate(TC10_surt_total_duration = ifelse(TC10_valid == 0, NA, TC10_surt_total_duration)) %>%
  mutate(TC10_surt_mean_duration = ifelse(TC10_valid == 0, NA, TC10_surt_mean_duration)) %>%
  mutate(TC10_surt_max_duration = ifelse(TC10_valid == 0, NA, TC10_surt_max_duration)) %>%
  mutate(TC10_surt_1st_glance_duration_incl_start = ifelse(TC10_valid == 0, NA, TC10_surt_1st_glance_duration_incl_start)) %>%
  mutate(TC10_surt_1st_glance_duration_without_start = ifelse(TC10_valid == 0, NA, TC10_surt_1st_glance_duration_without_start)) %>%
  mutate(TC10_wheel_glance_at_start = ifelse(TC10_valid == 0, NA, TC10_wheel_glance_at_start)) %>% # AOI wheel
  mutate(TC10_wheel_n_glances_after_start = ifelse(TC10_valid == 0, NA, TC10_wheel_n_glances_after_start)) %>%
  mutate(TC10_wheel_total_duration = ifelse(TC10_valid == 0, NA, TC10_wheel_total_duration)) %>%
  mutate(TC10_wheel_mean_duration = ifelse(TC10_valid == 0, NA, TC10_wheel_mean_duration)) %>%
  mutate(TC10_wheel_max_duration = ifelse(TC10_valid == 0, NA, TC10_wheel_max_duration)) %>%
  mutate(TC10_wheel_1st_glance_duration_incl_start = ifelse(TC10_valid == 0, NA, TC10_wheel_1st_glance_duration_incl_start)) %>%
  mutate(TC10_wheel_1st_glance_duration_without_start = ifelse(TC10_valid == 0, NA, TC10_wheel_1st_glance_duration_without_start))

data_all_TC12 <- read.csv("data/eyetracking/Exp01_Exp03_GazeBehavior_TC12_Rready.csv", encoding = "UTF-8")
names(data_all_TC12) <- c("Exp", "HMI", "VPNr", "TC12_comment", "TC12_valid", 
                        "TC12_EB_triggered", "TC12_lvl_notification", "TC12_lvl_end", "TC12_TimeToL0", 
                        "TC12_nth_gaze_to_ic", "TC12_n_gazes_till_EB_or_TO", "TC12_ic_glance_allocation_time", 
                        "TC12_ic_glance_at_start", "TC12_ic_n_glances_after_start", "TC12_ic_total_duration", 
                        "TC12_ic_mean_duration", "TC12_ic_max_duration", "TC12_ic_1st_glance_duration_incl_start", 
                        "TC12_ic_1st_glance_duration_without_start", "TC12_street_glance_at_start", 
                        "TC12_street_n_glances_after_start", "TC12_street_total_duration", "TC12_street_mean_duration", 
                        "TC12_street_max_duration", "TC12_street_1st_glance_duration_incl_start", 
                        "TC12_street_1st_glance_duration_without_start", "TC12_surt_glance_at_start", 
                        "TC12_surt_n_glances_after_start", "TC12_surt_total_duration", "TC12_surt_mean_duration", 
                        "TC12_surt_max_duration", "TC12_surt_1st_glance_duration_incl_start", 
                        "TC12_surt_1st_glance_duration_without_start", "TC12_wheel_glance_at_start", 
                        "TC12_wheel_n_glances_after_start", "TC12_wheel_total_duration", "TC12_wheel_mean_duration", 
                        "TC12_wheel_max_duration", "TC12_wheel_1st_glance_duration_incl_start", 
                        "TC12_wheel_1st_glance_duration_without_start")

data_all_TC12 <- data_all_TC12 %>%
  mutate(TC12_EB_triggered = ifelse(TC12_valid == 0, NA, TC12_EB_triggered)) %>%
  mutate(TC12_lvl_notification = ifelse(TC12_valid == 0, NA, TC12_lvl_notification)) %>%
  mutate(TC12_lvl_end = ifelse(TC12_valid == 0, NA, TC12_lvl_end)) %>%
  mutate(TC12_TimeToL0 = ifelse(TC12_valid == 0, NA, TC12_TimeToL0)) %>%
  mutate(TC12_nth_gaze_to_ic = ifelse(TC12_valid == 0, NA, TC12_nth_gaze_to_ic)) %>%
  mutate(TC12_n_gazes_till_EB_or_TO = ifelse(TC12_valid == 0, NA, TC12_n_gazes_till_EB_or_TO)) %>%
  mutate(TC12_ic_glance_allocation_time = ifelse(TC12_valid == 0, NA, TC12_ic_glance_allocation_time)) %>%
  mutate(TC12_ic_glance_at_start = ifelse(TC12_valid == 0, NA, TC12_ic_glance_at_start)) %>% # AOI ic
  mutate(TC12_ic_n_glances_after_start = ifelse(TC12_valid == 0, NA, TC12_ic_n_glances_after_start)) %>%
  mutate(TC12_ic_total_duration = ifelse(TC12_valid == 0, NA, TC12_ic_total_duration)) %>%
  mutate(TC12_ic_mean_duration = ifelse(TC12_valid == 0, NA, TC12_ic_mean_duration)) %>%
  mutate(TC12_ic_max_duration = ifelse(TC12_valid == 0, NA, TC12_ic_max_duration)) %>%
  mutate(TC12_ic_1st_glance_duration_incl_start = ifelse(TC12_valid == 0, NA, TC12_ic_1st_glance_duration_incl_start)) %>%
  mutate(TC12_ic_1st_glance_duration_without_start = ifelse(TC12_valid == 0, NA, TC12_ic_1st_glance_duration_without_start)) %>%
  mutate(TC12_street_glance_at_start = ifelse(TC12_valid == 0, NA, TC12_street_glance_at_start)) %>%  # AOI street
  mutate(TC12_street_n_glances_after_start = ifelse(TC12_valid == 0, NA, TC12_street_n_glances_after_start)) %>%
  mutate(TC12_street_total_duration = ifelse(TC12_valid == 0, NA, TC12_street_total_duration)) %>%
  mutate(TC12_street_mean_duration = ifelse(TC12_valid == 0, NA, TC12_street_mean_duration)) %>%
  mutate(TC12_street_max_duration = ifelse(TC12_valid == 0, NA, TC12_street_max_duration)) %>%
  mutate(TC12_street_1st_glance_duration_incl_start = ifelse(TC12_valid == 0, NA, TC12_street_1st_glance_duration_incl_start)) %>%
  mutate(TC12_street_1st_glance_duration_without_start = ifelse(TC12_valid == 0, NA, TC12_street_1st_glance_duration_without_start)) %>%
  mutate(TC12_surt_glance_at_start = ifelse(TC12_valid == 0, NA, TC12_surt_glance_at_start)) %>%  # AOI surt
  mutate(TC12_surt_n_glances_after_start = ifelse(TC12_valid == 0, NA, TC12_surt_n_glances_after_start)) %>%
  mutate(TC12_surt_total_duration = ifelse(TC12_valid == 0, NA, TC12_surt_total_duration)) %>%
  mutate(TC12_surt_mean_duration = ifelse(TC12_valid == 0, NA, TC12_surt_mean_duration)) %>%
  mutate(TC12_surt_max_duration = ifelse(TC12_valid == 0, NA, TC12_surt_max_duration)) %>%
  mutate(TC12_surt_1st_glance_duration_incl_start = ifelse(TC12_valid == 0, NA, TC12_surt_1st_glance_duration_incl_start)) %>%
  mutate(TC12_surt_1st_glance_duration_without_start = ifelse(TC12_valid == 0, NA, TC12_surt_1st_glance_duration_without_start)) %>%
  mutate(TC12_wheel_glance_at_start = ifelse(TC12_valid == 0, NA, TC12_wheel_glance_at_start)) %>% # AOI wheel
  mutate(TC12_wheel_n_glances_after_start = ifelse(TC12_valid == 0, NA, TC12_wheel_n_glances_after_start)) %>%
  mutate(TC12_wheel_total_duration = ifelse(TC12_valid == 0, NA, TC12_wheel_total_duration)) %>%
  mutate(TC12_wheel_mean_duration = ifelse(TC12_valid == 0, NA, TC12_wheel_mean_duration)) %>%
  mutate(TC12_wheel_max_duration = ifelse(TC12_valid == 0, NA, TC12_wheel_max_duration)) %>%
  mutate(TC12_wheel_1st_glance_duration_incl_start = ifelse(TC12_valid == 0, NA, TC12_wheel_1st_glance_duration_incl_start)) %>%
  mutate(TC12_wheel_1st_glance_duration_without_start = ifelse(TC12_valid == 0, NA, TC12_wheel_1st_glance_duration_without_start))

data_all_TO <- left_join(data_all_TC10, data_all_TC12, by = c("Exp", "HMI", "VPNr")) %>%
  filter(Exp != 1 | VPNr != 64) %>%
  filter(Exp != 2 | VPNr != 12) %>%
  filter(Exp != 2 | VPNr != 22) %>%
  filter(Exp != 2 | VPNr != 55) %>%
  filter(Exp != 2 | VPNr != 68)

data_all_AR <- read.csv("data/eyetracking/Exp01_Exp03_AR_TC1,4,7_Rready.csv", encoding = "UTF-8")
names(data_all_AR) <- c("Exp", "HMI", "VPNr", "TC01_duration", "TC01_AR_surt", "TC01_AR_street", 
                        "TC01_AR_ic", "TC01_AR_wheel", "TC01_AR_sum", "TC01_valid", "TC01_comment", 
                        "TC04_duration", "TC04_AR_surt", "TC04_AR_street", "TC04_AR_ic", "TC04_AR_wheel", 
                        "TC04_AR_sum", "TC04_valid", "TC04_comment", "TC07_duration", "TC07_AR_surt", 
                        "TC07_AR_street", "TC07_AR_ic", "TC07_AR_wheel", "TC07_AR_sum", "TC07_valid", 
                        "TC07_comment", "TC01_lvl_start", "TC01_lvl_end", "TC01_prop_L0", "TC01_prop_L2", 
                        "TC01_prop_L3", "TC04_lvl_start", "TC04_lvl_end", "TC04_prop_L0", "TC04_prop_L2", 
                        "TC04_prop_L3", "TC07_lvl_start", "TC07_lvl_end", "TC07_prop_L0", "TC07_prop_L2", 
                        "TC07_prop_L3")
data_all_AR <- data_all_AR %>%
  mutate(TC01_duration = ifelse(TC01_valid == 0, NA, TC01_duration)) %>% # TC01
  mutate(TC01_AR_surt = ifelse(TC01_valid == 0, NA, TC01_AR_surt)) %>%
  mutate(TC01_AR_street = ifelse(TC01_valid == 0, NA, TC01_AR_street)) %>%
  mutate(TC01_AR_ic = ifelse(TC01_valid == 0, NA, TC01_AR_ic)) %>%
  mutate(TC01_AR_wheel = ifelse(TC01_valid == 0, NA, TC01_AR_wheel)) %>%
  mutate(TC01_AR_sum = ifelse(TC01_valid == 0, NA, TC01_AR_sum)) %>%
  mutate(TC01_lvl_start = ifelse(TC01_valid == 0, NA, TC01_lvl_start)) %>%
  mutate(TC01_lvl_end = ifelse(TC01_valid == 0, NA, TC01_lvl_end)) %>%
  mutate(TC01_prop_L0 = ifelse(TC01_valid == 0, NA, TC01_prop_L0)) %>%
  mutate(TC01_prop_L2 = ifelse(TC01_valid == 0, NA, TC01_prop_L2)) %>%
  mutate(TC01_prop_L3 = ifelse(TC01_valid == 0, NA, TC01_prop_L3)) %>%
  mutate(TC04_duration = ifelse(TC04_valid == 0, NA, TC04_duration)) %>%  # TC04
  mutate(TC04_AR_surt = ifelse(TC04_valid == 0, NA, TC04_AR_surt)) %>%
  mutate(TC04_AR_street = ifelse(TC04_valid == 0, NA, TC04_AR_street)) %>%
  mutate(TC04_AR_ic = ifelse(TC04_valid == 0, NA, TC04_AR_ic)) %>%
  mutate(TC04_AR_wheel = ifelse(TC04_valid == 0, NA, TC04_AR_wheel)) %>%
  mutate(TC04_AR_sum = ifelse(TC04_valid == 0, NA, TC04_AR_sum)) %>%
  mutate(TC04_lvl_start = ifelse(TC04_valid == 0, NA, TC04_lvl_start)) %>%
  mutate(TC04_lvl_end = ifelse(TC04_valid == 0, NA, TC04_lvl_end)) %>%
  mutate(TC04_prop_L0 = ifelse(TC04_valid == 0, NA, TC04_prop_L0)) %>%
  mutate(TC04_prop_L2 = ifelse(TC04_valid == 0, NA, TC04_prop_L2)) %>%
  mutate(TC04_prop_L3 = ifelse(TC04_valid == 0, NA, TC04_prop_L3)) %>%
  mutate(TC07_duration = ifelse(TC07_valid == 0, NA, TC07_duration)) %>% # TC07
  mutate(TC07_AR_surt = ifelse(TC07_valid == 0, NA, TC07_AR_surt)) %>%
  mutate(TC07_AR_street = ifelse(TC07_valid == 0, NA, TC07_AR_street)) %>%
  mutate(TC07_AR_ic = ifelse(TC07_valid == 0, NA, TC07_AR_ic)) %>%
  mutate(TC07_AR_wheel = ifelse(TC07_valid == 0, NA, TC07_AR_wheel)) %>%
  mutate(TC07_AR_sum = ifelse(TC07_valid == 0, NA, TC07_AR_sum)) %>%
  mutate(TC07_lvl_start = ifelse(TC07_valid == 0, NA, TC07_lvl_start)) %>%
  mutate(TC07_lvl_end = ifelse(TC07_valid == 0, NA, TC07_lvl_end)) %>%
  mutate(TC07_prop_L0 = ifelse(TC07_valid == 0, NA, TC07_prop_L0)) %>%
  mutate(TC07_prop_L2 = ifelse(TC07_valid == 0, NA, TC07_prop_L2)) %>%
  mutate(TC07_prop_L3 = ifelse(TC07_valid == 0, NA, TC07_prop_L3))

data_all_AR <- data_all_AR %>%
  filter(Exp != 1 | VPNr != 64) %>%
  filter(Exp != 2 | VPNr != 12) %>%
  filter(Exp != 2 | VPNr != 22) %>%
  filter(Exp != 2 | VPNr != 55) %>%
  filter(Exp != 2 | VPNr != 68)


# notes on variables --- data_all_TO --------------------------------------
# ## meta variables, n = 9  ##
#     Exp
#     HMI
#     VPNr
#     TC..._valid (n = 2)
#     TC..._lvl_... (n = 4)

# ## count data, n = 22
#     TC..._EB_triggered (n = 2)
#     TC..._nth_gaze_to_ic (n = 2)
#     TC..._n_gazes_till_EB_or_TO (n = 2)
#     TC..._AOI_glance_at_start (n = 8)
#     TC..._AOI_n_glances_after_start (n = 8)

# ## interval data, n = 44
#     TC..._TimeToL0 (n = 2)
#     TC..._ic_glance_allocation_time (n = 2)

#     TC..._AOI_total_duration (n = 8)
#     TC..._AOI_mean_duration (n = 8)
#     TC..._AOI_max_duration (n = 8)
#     TC..._AOI_1st_glance_duration_incl_start (n = 8)
#     TC..._AOI_1st_glance_duration_without_start (n = 8)

# ## other (text,), n = 2
#     TC..._comment (n = 2)

# notes on variables --- data_all_AR --------------------------------------
# ## meta variables, n = 21  ##
#     Exp
#     HMI
#     VPNr
#     TC..._duration (n = 3)
#     TC..._valid (n = 3)
#     TC..._lvl... (n = 6)
#     TC..._prop_L... (n = 9)

# ## interval data, n = 15
#     TC..._AR_surt (n = 3)
#     TC..._AR_street (n = 3)
#     TC..._AR_ic (n = 3)
#     TC..._AR_wheel (n = 3)
#     TC..._AR_sum (n = 3)

# ## other (text,), n = 3
#     TC..._comment (n = 3)
# summaries TO --------------------------------------
# subset count data 
vars_TO_count <- c("TC10_valid", "TC10_lvl_notification", "TC10_lvl_end", 
                "TC10_EB_triggered", "TC10_nth_gaze_to_ic", "TC10_n_gazes_till_EB_or_TO",
                "TC10_ic_glance_at_start", "TC10_ic_n_glances_after_start",
                "TC10_street_glance_at_start", "TC10_street_n_glances_after_start",
                "TC10_surt_glance_at_start", "TC10_surt_n_glances_after_start",
                "TC10_wheel_glance_at_start", "TC10_wheel_n_glances_after_start",
                "TC12_valid", "TC12_lvl_notification", "TC12_lvl_end", 
                "TC12_EB_triggered", "TC12_nth_gaze_to_ic", "TC12_n_gazes_till_EB_or_TO",
                "TC12_ic_glance_at_start", "TC12_ic_n_glances_after_start",
                "TC12_street_glance_at_start", "TC12_street_n_glances_after_start",
                "TC12_surt_glance_at_start", "TC12_surt_n_glances_after_start",
                "TC12_wheel_glance_at_start", "TC12_wheel_n_glances_after_start")

subset_TO_count_factors <- data_all_TO %>%
  select(Exp, HMI,
         vars_TO_count) %>%
  mutate_at(vars_TO_count, factor)

subset_TO_count_nofactors <- data_all_TO %>%
  select(Exp, HMI,
         vars_TO_count)
        
# summary count data
summary_TO_count_factors <- subset_TO_count_factors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_TO_count_nofactors <- subset_TO_count_nofactors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_TO_count <- left_join(summary_TO_count_nofactors, summary_TO_count_factors)

# subset interval data
subset_TO_interval <- data_all_TO %>%
  select(Exp, HMI,
         ends_with(c("TimeToL0", "ic_glance_allocation_time",
                     "_total_duration", "_mean_duration", "_max_duration", 
                     "_glance_duration_incl_start", "_glance_duration_without_start")))

# summary interval data
summary_TO_interval <- subset_TO_interval %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

# create summary for TO
summary_TO <- bind_rows(summary_TO_count, summary_TO_interval)

summary_headers <- c("variable", "Exp", "HMI", "n_missing", "n", "mean", "sd", "median", "min", "max", "frequencies")
names(summary_TO) <- summary_headers

summ_TO <- summary_TO %>%
  mutate(n_notNA = n - n_missing, .after = n) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 2)) %>%
  mutate(max = round(max, 2))

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "summ_TO", "vars_TO_count",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))
# summaries AR --------------------------------------
# subset count data 
vars_AR_count <- c("TC01_valid", "TC01_lvl_start", "TC01_lvl_end",
                "TC04_valid", "TC04_lvl_start", "TC04_lvl_end",
                "TC07_valid", "TC07_lvl_start", "TC07_lvl_end")

subset_AR_count_factors <- data_all_AR %>%
  select(Exp, HMI,
         vars_AR_count) %>%
  mutate_at(vars_AR_count, factor)

subset_AR_count_nofactors <- data_all_AR %>%
  select(Exp, HMI,
         vars_AR_count)

# summary count data
summary_AR_count_factors <- subset_AR_count_factors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_AR_count_nofactors <- subset_AR_count_nofactors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_AR_count <- left_join(summary_AR_count_nofactors, summary_AR_count_factors)

# subset interval data
subset_AR_interval <- data_all_AR %>%
  select(Exp, HMI,
         ends_with(c("duration")),
         starts_with(c("TC01_AR_", "TC01_prop_", "TC04_AR_", "TC04_prop_", "TC07_AR_", "TC07_prop_")))

# summary interval data
summary_AR_interval <- subset_AR_interval %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

# create summary for TO
summary_AR <- bind_rows(summary_AR_count, summary_AR_interval)

summary_headers <- c("variable", "Exp", "HMI", "n_missing", "n", "mean", "sd", "median", "min", "max", "frequencies")
names(summary_AR) <- summary_headers

summ_AR <- summary_AR %>%
  mutate(n_notNA = n - n_missing, .after = n) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 2)) %>%
  mutate(max = round(max, 2))

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "summ_TO", "summ_AR", "vars_TO_count", "vars_AR_count",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

# frequency tables of nominal and ordinal data: functions ----------------------
notemptycount_ML <- function(x) {  (sum(x!="", na.rm = TRUE))}

count0_ML <- function(x) {  (sum(x==0, na.rm = TRUE))}
count1_ML <- function(x) {  (sum(x==1, na.rm = TRUE))}
count2_ML <- function(x) {  (sum(x==2, na.rm = TRUE))}
count3_ML <- function(x) {  (sum(x==3, na.rm = TRUE))}
count4_ML <- function(x) {  (sum(x==4, na.rm = TRUE))}
count5_ML <- function(x) {  (sum(x==5, na.rm = TRUE))}
count6_ML <- function(x) {  (sum(x==6, na.rm = TRUE))}
count7_ML <- function(x) {  (sum(x==7, na.rm = TRUE))}
count8_ML <- function(x) {  (sum(x==8, na.rm = TRUE))}
count9_ML <- function(x) {  (sum(x==9, na.rm = TRUE))}
count10_ML <- function(x) {  (sum(x==10, na.rm = TRUE))}
count11_ML <- function(x) {  (sum(x==11, na.rm = TRUE))}
count12_ML <- function(x) {  (sum(x==12, na.rm = TRUE))}
count13_ML <- function(x) {  (sum(x==13, na.rm = TRUE))}

# frequency tables of nominal and ordinal data: TO ----------------------
## 1HC
TO_1HC <- data_all_TO %>%
  filter(Exp == 1 & HMI == "HC") %>%
  select(all_of(vars_TO_count))

TO_1HC0 <- as.data.frame(sapply(TO_1HC, count0_ML))
TO_1HC1 <- as.data.frame(sapply(TO_1HC, count1_ML))
TO_1HC2 <- as.data.frame(sapply(TO_1HC, count2_ML))
TO_1HC3 <- as.data.frame(sapply(TO_1HC, count3_ML))
TO_1HC4 <- as.data.frame(sapply(TO_1HC, count4_ML))
TO_1HC5 <- as.data.frame(sapply(TO_1HC, count5_ML))
TO_1HC6 <- as.data.frame(sapply(TO_1HC, count6_ML))
TO_1HC7 <- as.data.frame(sapply(TO_1HC, count7_ML))
TO_1HC8 <- as.data.frame(sapply(TO_1HC, count8_ML))
TO_1HC9 <- as.data.frame(sapply(TO_1HC, count9_ML))
TO_1HC10 <- as.data.frame(sapply(TO_1HC, count10_ML))
TO_1HC11 <- as.data.frame(sapply(TO_1HC, count11_ML))
TO_1HC12 <- as.data.frame(sapply(TO_1HC, count12_ML))
TO_1HC13 <- as.data.frame(sapply(TO_1HC, count13_ML))
TO_1HCn <- as.data.frame(sapply(TO_1HC, notemptycount_ML))

TO_freq_1HC <- data.frame(TO_1HCn, TO_1HC0, TO_1HC1, TO_1HC2, TO_1HC3, TO_1HC4, TO_1HC5, TO_1HC6, 
                          TO_1HC7, TO_1HC8, TO_1HC9, TO_1HC10, TO_1HC11, TO_1HC12, TO_1HC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 1, HMI = "HC", variable = vars_TO_count, .before = "n")

## 1LC
TO_1LC <- data_all_TO %>%
  filter(Exp == 1 & HMI == "LC") %>%
  select(all_of(vars_TO_count))

TO_1LC0 <- as.data.frame(sapply(TO_1LC, count0_ML))
TO_1LC1 <- as.data.frame(sapply(TO_1LC, count1_ML))
TO_1LC2 <- as.data.frame(sapply(TO_1LC, count2_ML))
TO_1LC3 <- as.data.frame(sapply(TO_1LC, count3_ML))
TO_1LC4 <- as.data.frame(sapply(TO_1LC, count4_ML))
TO_1LC5 <- as.data.frame(sapply(TO_1LC, count5_ML))
TO_1LC6 <- as.data.frame(sapply(TO_1LC, count6_ML))
TO_1LC7 <- as.data.frame(sapply(TO_1LC, count7_ML))
TO_1LC8 <- as.data.frame(sapply(TO_1LC, count8_ML))
TO_1LC9 <- as.data.frame(sapply(TO_1LC, count9_ML))
TO_1LC10 <- as.data.frame(sapply(TO_1LC, count10_ML))
TO_1LC11 <- as.data.frame(sapply(TO_1LC, count11_ML))
TO_1LC12 <- as.data.frame(sapply(TO_1LC, count12_ML))
TO_1LC13 <- as.data.frame(sapply(TO_1LC, count13_ML))
TO_1LCn <- as.data.frame(sapply(TO_1LC, notemptycount_ML))

TO_freq_1LC <- data.frame(TO_1LCn, TO_1LC0, TO_1LC1, TO_1LC2, TO_1LC3, TO_1LC4, TO_1LC5, TO_1LC6, 
                          TO_1LC7, TO_1LC8, TO_1LC9, TO_1LC10, TO_1LC11, TO_1LC12, TO_1LC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 1, HMI = "LC", variable = vars_TO_count, .before = "n")

## 2HC
TO_2HC <- data_all_TO %>%
  filter(Exp == 2 & HMI == "HC") %>%
  select(all_of(vars_TO_count))

TO_2HC0 <- as.data.frame(sapply(TO_2HC, count0_ML))
TO_2HC1 <- as.data.frame(sapply(TO_2HC, count1_ML))
TO_2HC2 <- as.data.frame(sapply(TO_2HC, count2_ML))
TO_2HC3 <- as.data.frame(sapply(TO_2HC, count3_ML))
TO_2HC4 <- as.data.frame(sapply(TO_2HC, count4_ML))
TO_2HC5 <- as.data.frame(sapply(TO_2HC, count5_ML))
TO_2HC6 <- as.data.frame(sapply(TO_2HC, count6_ML))
TO_2HC7 <- as.data.frame(sapply(TO_2HC, count7_ML))
TO_2HC8 <- as.data.frame(sapply(TO_2HC, count8_ML))
TO_2HC9 <- as.data.frame(sapply(TO_2HC, count9_ML))
TO_2HC10 <- as.data.frame(sapply(TO_2HC, count10_ML))
TO_2HC11 <- as.data.frame(sapply(TO_2HC, count11_ML))
TO_2HC12 <- as.data.frame(sapply(TO_2HC, count12_ML))
TO_2HC13 <- as.data.frame(sapply(TO_2HC, count13_ML))
TO_2HCn <- as.data.frame(sapply(TO_2HC, notemptycount_ML))

TO_freq_2HC <- data.frame(TO_2HCn, TO_2HC0, TO_2HC1, TO_2HC2, TO_2HC3, TO_2HC4, TO_2HC5, TO_2HC6, 
                          TO_2HC7, TO_2HC8, TO_2HC9, TO_2HC10, TO_2HC11, TO_2HC12, TO_2HC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 2, HMI = "HC", variable = vars_TO_count, .before = "n")

## 2LC
TO_2LC <- data_all_TO %>%
  filter(Exp == 2 & HMI == "LC") %>%
  select(all_of(vars_TO_count))

TO_2LC0 <- as.data.frame(sapply(TO_2LC, count0_ML))
TO_2LC1 <- as.data.frame(sapply(TO_2LC, count1_ML))
TO_2LC2 <- as.data.frame(sapply(TO_2LC, count2_ML))
TO_2LC3 <- as.data.frame(sapply(TO_2LC, count3_ML))
TO_2LC4 <- as.data.frame(sapply(TO_2LC, count4_ML))
TO_2LC5 <- as.data.frame(sapply(TO_2LC, count5_ML))
TO_2LC6 <- as.data.frame(sapply(TO_2LC, count6_ML))
TO_2LC7 <- as.data.frame(sapply(TO_2LC, count7_ML))
TO_2LC8 <- as.data.frame(sapply(TO_2LC, count8_ML))
TO_2LC9 <- as.data.frame(sapply(TO_2LC, count9_ML))
TO_2LC10 <- as.data.frame(sapply(TO_2LC, count10_ML))
TO_2LC11 <- as.data.frame(sapply(TO_2LC, count11_ML))
TO_2LC12 <- as.data.frame(sapply(TO_2LC, count12_ML))
TO_2LC13 <- as.data.frame(sapply(TO_2LC, count13_ML))
TO_2LCn <- as.data.frame(sapply(TO_2LC, notemptycount_ML))

TO_freq_2LC <- data.frame(TO_2LCn, TO_2LC0, TO_2LC1, TO_2LC2, TO_2LC3, TO_2LC4, TO_2LC5, TO_2LC6, 
                          TO_2LC7, TO_2LC8, TO_2LC9, TO_2LC10, TO_2LC11, TO_2LC12, TO_2LC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 2, HMI = "LC", variable = vars_TO_count, .before = "n")

## 3HC
TO_3HC <- data_all_TO %>%
  filter(Exp == 3 & HMI == "HC") %>%
  select(all_of(vars_TO_count))

TO_3HC0 <- as.data.frame(sapply(TO_3HC, count0_ML))
TO_3HC1 <- as.data.frame(sapply(TO_3HC, count1_ML))
TO_3HC2 <- as.data.frame(sapply(TO_3HC, count2_ML))
TO_3HC3 <- as.data.frame(sapply(TO_3HC, count3_ML))
TO_3HC4 <- as.data.frame(sapply(TO_3HC, count4_ML))
TO_3HC5 <- as.data.frame(sapply(TO_3HC, count5_ML))
TO_3HC6 <- as.data.frame(sapply(TO_3HC, count6_ML))
TO_3HC7 <- as.data.frame(sapply(TO_3HC, count7_ML))
TO_3HC8 <- as.data.frame(sapply(TO_3HC, count8_ML))
TO_3HC9 <- as.data.frame(sapply(TO_3HC, count9_ML))
TO_3HC10 <- as.data.frame(sapply(TO_3HC, count10_ML))
TO_3HC11 <- as.data.frame(sapply(TO_3HC, count11_ML))
TO_3HC12 <- as.data.frame(sapply(TO_3HC, count12_ML))
TO_3HC13 <- as.data.frame(sapply(TO_3HC, count13_ML))
TO_3HCn <- as.data.frame(sapply(TO_3HC, notemptycount_ML))

TO_freq_3HC <- data.frame(TO_3HCn, TO_3HC0, TO_3HC1, TO_3HC2, TO_3HC3, TO_3HC4, TO_3HC5, TO_3HC6, 
                          TO_3HC7, TO_3HC8, TO_3HC9, TO_3HC10, TO_3HC11, TO_3HC12, TO_3HC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 3, HMI = "HC", variable = vars_TO_count, .before = "n")

## 3LC
TO_3LC <- data_all_TO %>%
  filter(Exp == 3 & HMI == "LC") %>%
  select(all_of(vars_TO_count))

TO_3LC0 <- as.data.frame(sapply(TO_3LC, count0_ML))
TO_3LC1 <- as.data.frame(sapply(TO_3LC, count1_ML))
TO_3LC2 <- as.data.frame(sapply(TO_3LC, count2_ML))
TO_3LC3 <- as.data.frame(sapply(TO_3LC, count3_ML))
TO_3LC4 <- as.data.frame(sapply(TO_3LC, count4_ML))
TO_3LC5 <- as.data.frame(sapply(TO_3LC, count5_ML))
TO_3LC6 <- as.data.frame(sapply(TO_3LC, count6_ML))
TO_3LC7 <- as.data.frame(sapply(TO_3LC, count7_ML))
TO_3LC8 <- as.data.frame(sapply(TO_3LC, count8_ML))
TO_3LC9 <- as.data.frame(sapply(TO_3LC, count9_ML))
TO_3LC10 <- as.data.frame(sapply(TO_3LC, count10_ML))
TO_3LC11 <- as.data.frame(sapply(TO_3LC, count11_ML))
TO_3LC12 <- as.data.frame(sapply(TO_3LC, count12_ML))
TO_3LC13 <- as.data.frame(sapply(TO_3LC, count13_ML))
TO_3LCn <- as.data.frame(sapply(TO_3LC, notemptycount_ML))

TO_freq_3LC <- data.frame(TO_3LCn, TO_3LC0, TO_3LC1, TO_3LC2, TO_3LC3, TO_3LC4, TO_3LC5, TO_3LC6, 
                          TO_3LC7, TO_3LC8, TO_3LC9, TO_3LC10, TO_3LC11, TO_3LC12, TO_3LC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 3, HMI = "LC", variable = vars_TO_count, .before = "n")

## combine all 6 conditions/subgroups
freq_TO <- bind_rows(TO_freq_1HC, TO_freq_1LC, TO_freq_2HC, TO_freq_2LC, TO_freq_3HC, TO_freq_3LC)
rownames(freq_TO) <- c()

# frequency tables of nominal and ordinal data: AR ----------------------
## 1HC
AR_1HC <- data_all_AR %>%
  filter(Exp == 1 & HMI == "HC") %>%
  select(all_of(vars_AR_count))

AR_1HC0 <- as.data.frame(sapply(AR_1HC, count0_ML))
AR_1HC1 <- as.data.frame(sapply(AR_1HC, count1_ML))
AR_1HC2 <- as.data.frame(sapply(AR_1HC, count2_ML))
AR_1HC3 <- as.data.frame(sapply(AR_1HC, count3_ML))
AR_1HC4 <- as.data.frame(sapply(AR_1HC, count4_ML))
AR_1HC5 <- as.data.frame(sapply(AR_1HC, count5_ML))
AR_1HC6 <- as.data.frame(sapply(AR_1HC, count6_ML))
AR_1HC7 <- as.data.frame(sapply(AR_1HC, count7_ML))
AR_1HC8 <- as.data.frame(sapply(AR_1HC, count8_ML))
AR_1HC9 <- as.data.frame(sapply(AR_1HC, count9_ML))
AR_1HC10 <- as.data.frame(sapply(AR_1HC, count10_ML))
AR_1HC11 <- as.data.frame(sapply(AR_1HC, count11_ML))
AR_1HC12 <- as.data.frame(sapply(AR_1HC, count12_ML))
AR_1HC13 <- as.data.frame(sapply(AR_1HC, count13_ML))
AR_1HCn <- as.data.frame(sapply(AR_1HC, notemptycount_ML))

AR_freq_1HC <- data.frame(AR_1HCn, AR_1HC0, AR_1HC1, AR_1HC2, AR_1HC3, AR_1HC4, AR_1HC5, AR_1HC6, 
                          AR_1HC7, AR_1HC8, AR_1HC9, AR_1HC10, AR_1HC11, AR_1HC12, AR_1HC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 1, HMI = "HC", variable = vars_AR_count, .before = "n")

## 1LC
AR_1LC <- data_all_AR %>%
  filter(Exp == 1 & HMI == "LC") %>%
  select(all_of(vars_AR_count))

AR_1LC0 <- as.data.frame(sapply(AR_1LC, count0_ML))
AR_1LC1 <- as.data.frame(sapply(AR_1LC, count1_ML))
AR_1LC2 <- as.data.frame(sapply(AR_1LC, count2_ML))
AR_1LC3 <- as.data.frame(sapply(AR_1LC, count3_ML))
AR_1LC4 <- as.data.frame(sapply(AR_1LC, count4_ML))
AR_1LC5 <- as.data.frame(sapply(AR_1LC, count5_ML))
AR_1LC6 <- as.data.frame(sapply(AR_1LC, count6_ML))
AR_1LC7 <- as.data.frame(sapply(AR_1LC, count7_ML))
AR_1LC8 <- as.data.frame(sapply(AR_1LC, count8_ML))
AR_1LC9 <- as.data.frame(sapply(AR_1LC, count9_ML))
AR_1LC10 <- as.data.frame(sapply(AR_1LC, count10_ML))
AR_1LC11 <- as.data.frame(sapply(AR_1LC, count11_ML))
AR_1LC12 <- as.data.frame(sapply(AR_1LC, count12_ML))
AR_1LC13 <- as.data.frame(sapply(AR_1LC, count13_ML))
AR_1LCn <- as.data.frame(sapply(AR_1LC, notemptycount_ML))

AR_freq_1LC <- data.frame(AR_1LCn, AR_1LC0, AR_1LC1, AR_1LC2, AR_1LC3, AR_1LC4, AR_1LC5, AR_1LC6, 
                          AR_1LC7, AR_1LC8, AR_1LC9, AR_1LC10, AR_1LC11, AR_1LC12, AR_1LC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 1, HMI = "LC", variable = vars_AR_count, .before = "n")

## 2HC
AR_2HC <- data_all_AR %>%
  filter(Exp == 2 & HMI == "HC") %>%
  select(all_of(vars_AR_count))

AR_2HC0 <- as.data.frame(sapply(AR_2HC, count0_ML))
AR_2HC1 <- as.data.frame(sapply(AR_2HC, count1_ML))
AR_2HC2 <- as.data.frame(sapply(AR_2HC, count2_ML))
AR_2HC3 <- as.data.frame(sapply(AR_2HC, count3_ML))
AR_2HC4 <- as.data.frame(sapply(AR_2HC, count4_ML))
AR_2HC5 <- as.data.frame(sapply(AR_2HC, count5_ML))
AR_2HC6 <- as.data.frame(sapply(AR_2HC, count6_ML))
AR_2HC7 <- as.data.frame(sapply(AR_2HC, count7_ML))
AR_2HC8 <- as.data.frame(sapply(AR_2HC, count8_ML))
AR_2HC9 <- as.data.frame(sapply(AR_2HC, count9_ML))
AR_2HC10 <- as.data.frame(sapply(AR_2HC, count10_ML))
AR_2HC11 <- as.data.frame(sapply(AR_2HC, count11_ML))
AR_2HC12 <- as.data.frame(sapply(AR_2HC, count12_ML))
AR_2HC13 <- as.data.frame(sapply(AR_2HC, count13_ML))
AR_2HCn <- as.data.frame(sapply(AR_2HC, notemptycount_ML))

AR_freq_2HC <- data.frame(AR_2HCn, AR_2HC0, AR_2HC1, AR_2HC2, AR_2HC3, AR_2HC4, AR_2HC5, AR_2HC6, 
                          AR_2HC7, AR_2HC8, AR_2HC9, AR_2HC10, AR_2HC11, AR_2HC12, AR_2HC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 2, HMI = "HC", variable = vars_AR_count, .before = "n")

## 2LC
AR_2LC <- data_all_AR %>%
  filter(Exp == 2 & HMI == "LC") %>%
  select(all_of(vars_AR_count))

AR_2LC0 <- as.data.frame(sapply(AR_2LC, count0_ML))
AR_2LC1 <- as.data.frame(sapply(AR_2LC, count1_ML))
AR_2LC2 <- as.data.frame(sapply(AR_2LC, count2_ML))
AR_2LC3 <- as.data.frame(sapply(AR_2LC, count3_ML))
AR_2LC4 <- as.data.frame(sapply(AR_2LC, count4_ML))
AR_2LC5 <- as.data.frame(sapply(AR_2LC, count5_ML))
AR_2LC6 <- as.data.frame(sapply(AR_2LC, count6_ML))
AR_2LC7 <- as.data.frame(sapply(AR_2LC, count7_ML))
AR_2LC8 <- as.data.frame(sapply(AR_2LC, count8_ML))
AR_2LC9 <- as.data.frame(sapply(AR_2LC, count9_ML))
AR_2LC10 <- as.data.frame(sapply(AR_2LC, count10_ML))
AR_2LC11 <- as.data.frame(sapply(AR_2LC, count11_ML))
AR_2LC12 <- as.data.frame(sapply(AR_2LC, count12_ML))
AR_2LC13 <- as.data.frame(sapply(AR_2LC, count13_ML))
AR_2LCn <- as.data.frame(sapply(AR_2LC, notemptycount_ML))

AR_freq_2LC <- data.frame(AR_2LCn, AR_2LC0, AR_2LC1, AR_2LC2, AR_2LC3, AR_2LC4, AR_2LC5, AR_2LC6, 
                          AR_2LC7, AR_2LC8, AR_2LC9, AR_2LC10, AR_2LC11, AR_2LC12, AR_2LC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 2, HMI = "LC", variable = vars_AR_count, .before = "n")

## 3HC
AR_3HC <- data_all_AR %>%
  filter(Exp == 3 & HMI == "HC") %>%
  select(all_of(vars_AR_count))

AR_3HC0 <- as.data.frame(sapply(AR_3HC, count0_ML))
AR_3HC1 <- as.data.frame(sapply(AR_3HC, count1_ML))
AR_3HC2 <- as.data.frame(sapply(AR_3HC, count2_ML))
AR_3HC3 <- as.data.frame(sapply(AR_3HC, count3_ML))
AR_3HC4 <- as.data.frame(sapply(AR_3HC, count4_ML))
AR_3HC5 <- as.data.frame(sapply(AR_3HC, count5_ML))
AR_3HC6 <- as.data.frame(sapply(AR_3HC, count6_ML))
AR_3HC7 <- as.data.frame(sapply(AR_3HC, count7_ML))
AR_3HC8 <- as.data.frame(sapply(AR_3HC, count8_ML))
AR_3HC9 <- as.data.frame(sapply(AR_3HC, count9_ML))
AR_3HC10 <- as.data.frame(sapply(AR_3HC, count10_ML))
AR_3HC11 <- as.data.frame(sapply(AR_3HC, count11_ML))
AR_3HC12 <- as.data.frame(sapply(AR_3HC, count12_ML))
AR_3HC13 <- as.data.frame(sapply(AR_3HC, count13_ML))
AR_3HCn <- as.data.frame(sapply(AR_3HC, notemptycount_ML))

AR_freq_3HC <- data.frame(AR_3HCn, AR_3HC0, AR_3HC1, AR_3HC2, AR_3HC3, AR_3HC4, AR_3HC5, AR_3HC6, 
                          AR_3HC7, AR_3HC8, AR_3HC9, AR_3HC10, AR_3HC11, AR_3HC12, AR_3HC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 3, HMI = "HC", variable = vars_AR_count, .before = "n")

## 3LC
AR_3LC <- data_all_AR %>%
  filter(Exp == 3 & HMI == "LC") %>%
  select(all_of(vars_AR_count))

AR_3LC0 <- as.data.frame(sapply(AR_3LC, count0_ML))
AR_3LC1 <- as.data.frame(sapply(AR_3LC, count1_ML))
AR_3LC2 <- as.data.frame(sapply(AR_3LC, count2_ML))
AR_3LC3 <- as.data.frame(sapply(AR_3LC, count3_ML))
AR_3LC4 <- as.data.frame(sapply(AR_3LC, count4_ML))
AR_3LC5 <- as.data.frame(sapply(AR_3LC, count5_ML))
AR_3LC6 <- as.data.frame(sapply(AR_3LC, count6_ML))
AR_3LC7 <- as.data.frame(sapply(AR_3LC, count7_ML))
AR_3LC8 <- as.data.frame(sapply(AR_3LC, count8_ML))
AR_3LC9 <- as.data.frame(sapply(AR_3LC, count9_ML))
AR_3LC10 <- as.data.frame(sapply(AR_3LC, count10_ML))
AR_3LC11 <- as.data.frame(sapply(AR_3LC, count11_ML))
AR_3LC12 <- as.data.frame(sapply(AR_3LC, count12_ML))
AR_3LC13 <- as.data.frame(sapply(AR_3LC, count13_ML))
AR_3LCn <- as.data.frame(sapply(AR_3LC, notemptycount_ML))

AR_freq_3LC <- data.frame(AR_3LCn, AR_3LC0, AR_3LC1, AR_3LC2, AR_3LC3, AR_3LC4, AR_3LC5, AR_3LC6, 
                          AR_3LC7, AR_3LC8, AR_3LC9, AR_3LC10, AR_3LC11, AR_3LC12, AR_3LC13) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  add_column(Exp = 3, HMI = "LC", variable = vars_AR_count, .before = "n")

## combine all 6 conditions/subgroups
freq_AR <- bind_rows(AR_freq_1HC, AR_freq_1LC, AR_freq_2HC, AR_freq_2LC, AR_freq_3HC, AR_freq_3LC)
rownames(freq_AR) <- c()

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "summ_TO", "summ_AR", "freq_TO", "freq_AR",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

#### save results ####
write_excel_csv(summ_TO, "data/results/summary_ET_TO.csv")
write_excel_csv(freq_TO, "data/results/frequencies_ET_TO.csv")
write_excel_csv(summ_AR, "data/results/summary_ET_AR.csv")
write_excel_csv(freq_AR, "data/results/frequencies_ET_AR.csv")

write_excel_csv(data_all_TO, "data/eyetracking/data_all_TO.csv")
write_excel_csv(data_all_AR, "data/eyetracking/data_all_AR.csv")