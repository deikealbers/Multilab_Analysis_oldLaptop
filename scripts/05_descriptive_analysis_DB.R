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
## data_all_TO
data_all_TC10 <- read.csv("data/drivingdata/all_TransitionTC10.csv", encoding = "UTF-8") %>%
  select(1:14, 28:34) # max 7 actions, omit actions 8-20

names(data_all_TC10) <- c("Exp", "VPNr", 
                          "TC10_Aut_Lvl_Sta", "TC10_Aut_Lvl_End", "TC10_Aut_Lvl_Notification", 
                          "TC10_TimeToFirstAction", "TC10_TimeToL0", 
                          "TC10_Action_1", "TC10_Action_2", "TC10_Action_3", "TC10_Action_4", 
                          "TC10_Action_5", "TC10_Action_6", "TC10_Action_7", 
                          "TC10_TimeToAction_1", "TC10_TimeToAction_2", "TC10_TimeToAction_3", "TC10_TimeToAction_4", 
                          "TC10_TimeToAction_5", "TC10_TimeToAction_6", "TC10_TimeToAction_7")

data_all_TC12 <- read.csv("data/drivingdata/all_TransitionTC12.csv", encoding = "UTF-8") %>%
  select(1:11, 28:31) # max 7 actions, omit actions 5-20

names(data_all_TC12) <- c("Exp", "VPNr", 
                          "TC12_Aut_Lvl_Sta", "TC12_Aut_Lvl_End", "TC12_Aut_Lvl_Notification", 
                          "TC12_TimeToFirstAction", "TC12_TimeToL0", 
                          "TC12_Action_1", "TC12_Action_2", "TC12_Action_3", "TC12_Action_4", 
                          "TC12_TimeToAction_1", "TC12_TimeToAction_2", "TC12_TimeToAction_3", "TC12_TimeToAction_4")

data_all_TO <- left_join(data_all_TC10, data_all_TC12, by = c("Exp", "VPNr")) %>%
  mutate(HMI = ifelse(VPNr < 50, "HC", "LC"), .before = "VPNr")

## data_all_Deact
data_all_Deact <- read.csv("data/drivingdata/all_DeactivationTC06.csv", encoding = "UTF-8") %>%
  rename(VPNr = VP) %>%
  mutate(HMI = ifelse(VPNr < 50, "HC", "LC"), .before = "VPNr")

## data_all_L2Hoff
data_all_L2Hoff <- read.csv("data/drivingdata/all_L2HoffDetections.csv", encoding = "UTF-8") %>%
  rename(VPNr = VP) %>%
  mutate(HMI = ifelse(VPNr < 50, "HC", "LC"), .before = "VPNr") %>%
  select(-c(Hoff_WarningDuration_10, Hoff_WarningDuration_11, Hoff_WarningDuration_12))

# remove not needed data
remove(data_all_TC10, data_all_TC12)


# data corrections --------------------------------------------------------
# ignore activation of L3 for now
data_all_Deact <- data_all_Deact %>%
  mutate(Deactivation = ifelse(Deactivation == -1, 0, Deactivation))

data_all_TO <- data_all_TO %>%
  mutate(TC10_TimeToAction_1 = ifelse(TC10_TimeToFirstAction == -2.4, TC10_TimeToAction_2, TC10_TimeToAction_1)) %>%
  mutate(TC10_TimeToAction_2 = ifelse(TC10_TimeToFirstAction == -2.4, NaN, TC10_TimeToAction_2)) %>%
  mutate(TC10_Action_1 = ifelse(TC10_TimeToFirstAction == -2.4, TC10_Action_2, TC10_Action_1)) %>%
  mutate(TC10_Action_2 = ifelse(TC10_TimeToFirstAction == -2.4, NaN, TC10_Action_2)) %>%
  mutate(TC10_TimeToFirstAction = ifelse(TC10_TimeToFirstAction == -2.4, TC10_TimeToAction_1, TC10_TimeToFirstAction))

data_all_TO <- data_all_TO %>%
  mutate(TC10_Aut_Lvl_End = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Aut_Lvl_End)) %>%
  mutate(TC10_TimeToFirstAction = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToFirstAction)) %>%
  mutate(TC10_TimeToL0 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToL0)) %>%
  mutate(TC10_Action_1 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_1)) %>%
  mutate(TC10_Action_2 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_2)) %>%
  mutate(TC10_Action_3 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_3)) %>%
  mutate(TC10_Action_4 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_4)) %>%
  mutate(TC10_Action_5 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_5)) %>%
  mutate(TC10_Action_6 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_6)) %>%
  mutate(TC10_Action_7 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_Action_7)) %>%
  mutate(TC10_TimeToAction_1 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_1)) %>%
  mutate(TC10_TimeToAction_2 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_2)) %>%
  mutate(TC10_TimeToAction_3 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_3)) %>%
  mutate(TC10_TimeToAction_4 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_4)) %>%
  mutate(TC10_TimeToAction_5 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_5)) %>%
  mutate(TC10_TimeToAction_6 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_6)) %>%
  mutate(TC10_TimeToAction_7 = ifelse(TC10_Aut_Lvl_Notification != 3, NA, TC10_TimeToAction_7)) %>%
  mutate(TC12_Aut_Lvl_End = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_Aut_Lvl_End)) %>%
  mutate(TC12_TimeToFirstAction = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_TimeToFirstAction)) %>%
  mutate(TC12_TimeToL0 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_TimeToL0)) %>%
  mutate(TC12_Action_1 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_Action_1)) %>%
  mutate(TC12_Action_2 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_Action_2)) %>%
  mutate(TC12_Action_3 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_Action_3)) %>%
  mutate(TC12_Action_4 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_Action_4)) %>%
  mutate(TC12_TimeToAction_1 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_TimeToAction_1)) %>%
  mutate(TC12_TimeToAction_2 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_TimeToAction_2)) %>%
  mutate(TC12_TimeToAction_3 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_TimeToAction_3)) %>%
  mutate(TC12_TimeToAction_4 = ifelse(TC12_Aut_Lvl_Notification != 3, NA, TC12_TimeToAction_4))

data_all_L2Hoff <- data_all_L2Hoff %>%
  mutate(Hoff_n_warnings_s1 = ifelse(Hoff_n_warnings == 0, 0, Hoff_n_warnings_s1)) %>%
  mutate(Hoff_n_warnings_s2 = ifelse(Hoff_n_warnings == 0, 0, Hoff_n_warnings_s2)) %>%
  mutate(Hoff_n_warnings_s3 = ifelse(Hoff_n_warnings == 0, 0, Hoff_n_warnings_s3))

data_all_L2Hoff <- data_all_L2Hoff %>%
  rowwise() %>% 
  mutate(Hoff_WarningDuration_mean = mean(c_across(Hoff_WarningDuration_1:Hoff_WarningDuration_9), na.rm = TRUE), .before = "Hoff_WarningDuration_1")

data_all_Deact[sapply(data_all_Deact, is.nan)] <- NA
data_all_L2Hoff[sapply(data_all_L2Hoff, is.nan)] <- NA
data_all_TO[sapply(data_all_TO, is.nan)] <- NA

# notes on variables -------------------------------------------------------
  # data_all_Deact
    # ## meta variables, n = 5
      # Exp
      # HMI
      # VPNr
      # Aut_Lvl_... (n = 2)
    # ## count data, n = 1
      # Deactivation
  # data_all_L2Hoff
    # ## meta variables, n = 3
      # Exp
      # HMI
      # VPNr
    # ## count data, n = 4
      # Hoff_n_... (n = 4)
    # ## interval data, n = 9
      # Hoff_WarningDuration_... (n = 9)
  # data_all_TO
    # ## meta variables, n = 9
      # Exp
      # HMI
      # VPNr
      # TC..._Aut_Lvl_... (n = 6)
    # ## count data, n = 11
      # TC..._Action_ (n = 7 + 4)
    # ## interval data, n = 15
      # TC..._TimeToFirstAction (n = 2)
      # TC..._TimeToL0 (n = 2)
      # TC..._TimeToAction_... (n = 7 + 4)

# summaries Deact --------------------------------------
# subset count data 
subset_Deact_count_factors <- data_all_Deact %>%
  select(Exp, HMI, Deactivation) %>%
  mutate_at("Deactivation", factor)

subset_Deact_count_nofactors <- data_all_Deact %>%
  select(Exp, HMI, Deactivation)

# summary count data
summary_Deact_count_factors <- subset_Deact_count_factors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_Deact_count_nofactors <- subset_Deact_count_nofactors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_Deact <- left_join(summary_Deact_count_nofactors, summary_Deact_count_factors)

summary_headers <- c("variable", "Exp", "HMI", "n_missing", "n", "mean", "sd", "median", "min", "max", "frequencies")
names(summary_Deact) <- summary_headers

summ_Deact <- summary_Deact %>%
  mutate(n_notNA = n - n_missing, .after = n) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 2)) %>%
  mutate(max = round(max, 2))

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all_Deact", "data_all_L2Hoff", "data_all_TO", 
                        "summ_Deact", 
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

# summaries L2Hoff --------------------------------------
# subset count data 
vars_L2Hoff_count <- c("Hoff_n_warnings", "Hoff_n_warnings_s1", 
                       "Hoff_n_warnings_s2","Hoff_n_warnings_s3")

subset_L2Hoff_count_factors <- data_all_L2Hoff %>%
  select(Exp, HMI,
         vars_L2Hoff_count) %>%
  mutate_at(vars_L2Hoff_count, factor)

subset_L2Hoff_count_nofactors <- data_all_L2Hoff %>%
  select(Exp, HMI,
         vars_L2Hoff_count)

# summary count data
summary_L2Hoff_count_factors <- subset_L2Hoff_count_factors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_L2Hoff_count_nofactors <- subset_L2Hoff_count_nofactors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_L2Hoff_count <- left_join(summary_L2Hoff_count_nofactors, summary_L2Hoff_count_factors)

# subset interval data
subset_L2Hoff_interval <- data_all_L2Hoff %>%
  select(Exp, HMI,
         starts_with(c("Hoff_WarningDuration_")))

# summary interval data
summary_L2Hoff_interval <- subset_L2Hoff_interval %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

# create summary for L2Hoff
summary_L2Hoff <- bind_rows(summary_L2Hoff_count, summary_L2Hoff_interval)

summary_headers <- c("variable", "Exp", "HMI", "n_missing", "n", "mean", "sd", "median", "min", "max", "frequencies")
names(summary_L2Hoff) <- summary_headers

summ_L2Hoff <- summary_L2Hoff %>%
  mutate(n_notNA = n - n_missing, .after = n) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 2)) %>%
  mutate(max = round(max, 2))


#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all_Deact", "data_all_L2Hoff", "data_all_TO", 
                        "summ_Deact", "summ_L2Hoff", "vars_L2Hoff_count", 
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

# summaries TO --------------------------------------
# subset count data 
vars_TO_count <- c("TC10_Action_1", "TC10_Action_2", "TC10_Action_3", "TC10_Action_4",
                   "TC10_Action_5", "TC10_Action_6", "TC10_Action_7",
                   "TC12_Action_1", "TC12_Action_2", "TC12_Action_3", "TC12_Action_4")

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
         ends_with(c("_TimeToFirstAction", "_TimeToL0")),
         starts_with(c("TC10_TimeToAction_", "TC12_TimeToAction_")))

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
rm(list=setdiff(ls(), c("data_all_Deact", "data_all_L2Hoff", "data_all_TO", 
                        "summ_Deact", "summ_L2Hoff", "vars_L2Hoff_count", "summ_TO", "vars_TO_count", 
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

# frequency tables of nominal and ordinal data: Deact ----------------------
## 1HC
Deact_1HC <- data_all_Deact %>%
  filter(Exp == 1 & HMI == "HC") %>%
  select(Deactivation)

Deact_1HC0 <- as.data.frame(sapply(Deact_1HC, count0_ML))
Deact_1HC1 <- as.data.frame(sapply(Deact_1HC, count1_ML))
Deact_1HC2 <- as.data.frame(sapply(Deact_1HC, count2_ML))
Deact_1HC3 <- as.data.frame(sapply(Deact_1HC, count3_ML))
Deact_1HC4 <- as.data.frame(sapply(Deact_1HC, count4_ML))
Deact_1HC5 <- as.data.frame(sapply(Deact_1HC, count5_ML))
Deact_1HC6 <- as.data.frame(sapply(Deact_1HC, count6_ML))
Deact_1HC7 <- as.data.frame(sapply(Deact_1HC, count7_ML))
Deact_1HC8 <- as.data.frame(sapply(Deact_1HC, count8_ML))
Deact_1HC9 <- as.data.frame(sapply(Deact_1HC, count9_ML))

Deact_1HCn <- as.data.frame(sapply(Deact_1HC, notemptycount_ML))

Deact_freq_1HC <- data.frame(Deact_1HCn, Deact_1HC0, Deact_1HC1, Deact_1HC2, Deact_1HC3, Deact_1HC4, Deact_1HC5, Deact_1HC6, 
                             Deact_1HC7, Deact_1HC8, Deact_1HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 1, HMI = "HC", variable = "Deactivation", .before = "n")

## 1LC
Deact_1LC <- data_all_Deact %>%
  filter(Exp == 1 & HMI == "LC") %>%
  select(Deactivation)

Deact_1LC0 <- as.data.frame(sapply(Deact_1LC, count0_ML))
Deact_1LC1 <- as.data.frame(sapply(Deact_1LC, count1_ML))
Deact_1LC2 <- as.data.frame(sapply(Deact_1LC, count2_ML))
Deact_1LC3 <- as.data.frame(sapply(Deact_1LC, count3_ML))
Deact_1LC4 <- as.data.frame(sapply(Deact_1LC, count4_ML))
Deact_1LC5 <- as.data.frame(sapply(Deact_1LC, count5_ML))
Deact_1LC6 <- as.data.frame(sapply(Deact_1LC, count6_ML))
Deact_1LC7 <- as.data.frame(sapply(Deact_1LC, count7_ML))
Deact_1LC8 <- as.data.frame(sapply(Deact_1LC, count8_ML))
Deact_1LC9 <- as.data.frame(sapply(Deact_1LC, count9_ML))
Deact_1LCn <- as.data.frame(sapply(Deact_1LC, notemptycount_ML))

Deact_freq_1LC <- data.frame(Deact_1LCn, Deact_1LC0, Deact_1LC1, Deact_1LC2, Deact_1LC3, Deact_1LC4, Deact_1LC5, Deact_1LC6, 
                             Deact_1LC7, Deact_1LC8, Deact_1LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 1, HMI = "LC", variable = "Deactivation", .before = "n")

## 2HC
Deact_2HC <- data_all_Deact %>%
  filter(Exp == 2 & HMI == "HC") %>%
  select(Deactivation)

Deact_2HC0 <- as.data.frame(sapply(Deact_2HC, count0_ML))
Deact_2HC1 <- as.data.frame(sapply(Deact_2HC, count1_ML))
Deact_2HC2 <- as.data.frame(sapply(Deact_2HC, count2_ML))
Deact_2HC3 <- as.data.frame(sapply(Deact_2HC, count3_ML))
Deact_2HC4 <- as.data.frame(sapply(Deact_2HC, count4_ML))
Deact_2HC5 <- as.data.frame(sapply(Deact_2HC, count5_ML))
Deact_2HC6 <- as.data.frame(sapply(Deact_2HC, count6_ML))
Deact_2HC7 <- as.data.frame(sapply(Deact_2HC, count7_ML))
Deact_2HC8 <- as.data.frame(sapply(Deact_2HC, count8_ML))
Deact_2HC9 <- as.data.frame(sapply(Deact_2HC, count9_ML))
Deact_2HCn <- as.data.frame(sapply(Deact_2HC, notemptycount_ML))

Deact_freq_2HC <- data.frame(Deact_2HCn, Deact_2HC0, Deact_2HC1, Deact_2HC2, Deact_2HC3, Deact_2HC4, Deact_2HC5, Deact_2HC6, 
                             Deact_2HC7, Deact_2HC8, Deact_2HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 2, HMI = "HC", variable = "Deactivation", .before = "n")

## 2LC
Deact_2LC <- data_all_Deact %>%
  filter(Exp == 2 & HMI == "LC") %>%
  select(Deactivation)

Deact_2LC0 <- as.data.frame(sapply(Deact_2LC, count0_ML))
Deact_2LC1 <- as.data.frame(sapply(Deact_2LC, count1_ML))
Deact_2LC2 <- as.data.frame(sapply(Deact_2LC, count2_ML))
Deact_2LC3 <- as.data.frame(sapply(Deact_2LC, count3_ML))
Deact_2LC4 <- as.data.frame(sapply(Deact_2LC, count4_ML))
Deact_2LC5 <- as.data.frame(sapply(Deact_2LC, count5_ML))
Deact_2LC6 <- as.data.frame(sapply(Deact_2LC, count6_ML))
Deact_2LC7 <- as.data.frame(sapply(Deact_2LC, count7_ML))
Deact_2LC8 <- as.data.frame(sapply(Deact_2LC, count8_ML))
Deact_2LC9 <- as.data.frame(sapply(Deact_2LC, count9_ML))
Deact_2LCn <- as.data.frame(sapply(Deact_2LC, notemptycount_ML))

Deact_freq_2LC <- data.frame(Deact_2LCn, Deact_2LC0, Deact_2LC1, Deact_2LC2, Deact_2LC3, Deact_2LC4, Deact_2LC5, Deact_2LC6, 
                             Deact_2LC7, Deact_2LC8, Deact_2LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 2, HMI = "LC", variable = "Deactivation", .before = "n")

## 3HC
Deact_3HC <- data_all_Deact %>%
  filter(Exp == 3 & HMI == "HC") %>%
  select(Deactivation)

Deact_3HC0 <- as.data.frame(sapply(Deact_3HC, count0_ML))
Deact_3HC1 <- as.data.frame(sapply(Deact_3HC, count1_ML))
Deact_3HC2 <- as.data.frame(sapply(Deact_3HC, count2_ML))
Deact_3HC3 <- as.data.frame(sapply(Deact_3HC, count3_ML))
Deact_3HC4 <- as.data.frame(sapply(Deact_3HC, count4_ML))
Deact_3HC5 <- as.data.frame(sapply(Deact_3HC, count5_ML))
Deact_3HC6 <- as.data.frame(sapply(Deact_3HC, count6_ML))
Deact_3HC7 <- as.data.frame(sapply(Deact_3HC, count7_ML))
Deact_3HC8 <- as.data.frame(sapply(Deact_3HC, count8_ML))
Deact_3HC9 <- as.data.frame(sapply(Deact_3HC, count9_ML))
Deact_3HCn <- as.data.frame(sapply(Deact_3HC, notemptycount_ML))

Deact_freq_3HC <- data.frame(Deact_3HCn, Deact_3HC0, Deact_3HC1, Deact_3HC2, Deact_3HC3, Deact_3HC4, Deact_3HC5, Deact_3HC6, 
                             Deact_3HC7, Deact_3HC8, Deact_3HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 3, HMI = "HC", variable = "Deactivation", .before = "n")

## 3LC
Deact_3LC <- data_all_Deact %>%
  filter(Exp == 3 & HMI == "LC") %>%
  select(Deactivation)

Deact_3LC0 <- as.data.frame(sapply(Deact_3LC, count0_ML))
Deact_3LC1 <- as.data.frame(sapply(Deact_3LC, count1_ML))
Deact_3LC2 <- as.data.frame(sapply(Deact_3LC, count2_ML))
Deact_3LC3 <- as.data.frame(sapply(Deact_3LC, count3_ML))
Deact_3LC4 <- as.data.frame(sapply(Deact_3LC, count4_ML))
Deact_3LC5 <- as.data.frame(sapply(Deact_3LC, count5_ML))
Deact_3LC6 <- as.data.frame(sapply(Deact_3LC, count6_ML))
Deact_3LC7 <- as.data.frame(sapply(Deact_3LC, count7_ML))
Deact_3LC8 <- as.data.frame(sapply(Deact_3LC, count8_ML))
Deact_3LC9 <- as.data.frame(sapply(Deact_3LC, count9_ML))
Deact_3LCn <- as.data.frame(sapply(Deact_3LC, notemptycount_ML))

Deact_freq_3LC <- data.frame(Deact_3LCn, Deact_3LC0, Deact_3LC1, Deact_3LC2, Deact_3LC3, Deact_3LC4, Deact_3LC5, Deact_3LC6, 
                             Deact_3LC7, Deact_3LC8, Deact_3LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 3, HMI = "LC", variable = "Deactivation", .before = "n")

## combine all 6 conditions/subgroups
freq_Deact <- bind_rows(Deact_freq_1HC, Deact_freq_1LC, Deact_freq_2HC, Deact_freq_2LC, Deact_freq_3HC, Deact_freq_3LC)
rownames(freq_Deact) <- c()

# frequency tables of nominal and ordinal data: L2Hoff ----------------------
## 1HC
L2Hoff_1HC <- data_all_L2Hoff %>%
  filter(Exp == 1 & HMI == "HC") %>%
  select(all_of(vars_L2Hoff_count))

L2Hoff_1HC0 <- as.data.frame(sapply(L2Hoff_1HC, count0_ML))
L2Hoff_1HC1 <- as.data.frame(sapply(L2Hoff_1HC, count1_ML))
L2Hoff_1HC2 <- as.data.frame(sapply(L2Hoff_1HC, count2_ML))
L2Hoff_1HC3 <- as.data.frame(sapply(L2Hoff_1HC, count3_ML))
L2Hoff_1HC4 <- as.data.frame(sapply(L2Hoff_1HC, count4_ML))
L2Hoff_1HC5 <- as.data.frame(sapply(L2Hoff_1HC, count5_ML))
L2Hoff_1HC6 <- as.data.frame(sapply(L2Hoff_1HC, count6_ML))
L2Hoff_1HC7 <- as.data.frame(sapply(L2Hoff_1HC, count7_ML))
L2Hoff_1HC8 <- as.data.frame(sapply(L2Hoff_1HC, count8_ML))
L2Hoff_1HC9 <- as.data.frame(sapply(L2Hoff_1HC, count9_ML))

L2Hoff_1HCn <- as.data.frame(sapply(L2Hoff_1HC, notemptycount_ML))

L2Hoff_freq_1HC <- data.frame(L2Hoff_1HCn, L2Hoff_1HC0, L2Hoff_1HC1, L2Hoff_1HC2, L2Hoff_1HC3, L2Hoff_1HC4, L2Hoff_1HC5, L2Hoff_1HC6, 
                              L2Hoff_1HC7, L2Hoff_1HC8, L2Hoff_1HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 1, HMI = "HC", variable = vars_L2Hoff_count, .before = "n")

## 1LC
L2Hoff_1LC <- data_all_L2Hoff %>%
  filter(Exp == 1 & HMI == "LC") %>%
  select(all_of(vars_L2Hoff_count))

L2Hoff_1LC0 <- as.data.frame(sapply(L2Hoff_1LC, count0_ML))
L2Hoff_1LC1 <- as.data.frame(sapply(L2Hoff_1LC, count1_ML))
L2Hoff_1LC2 <- as.data.frame(sapply(L2Hoff_1LC, count2_ML))
L2Hoff_1LC3 <- as.data.frame(sapply(L2Hoff_1LC, count3_ML))
L2Hoff_1LC4 <- as.data.frame(sapply(L2Hoff_1LC, count4_ML))
L2Hoff_1LC5 <- as.data.frame(sapply(L2Hoff_1LC, count5_ML))
L2Hoff_1LC6 <- as.data.frame(sapply(L2Hoff_1LC, count6_ML))
L2Hoff_1LC7 <- as.data.frame(sapply(L2Hoff_1LC, count7_ML))
L2Hoff_1LC8 <- as.data.frame(sapply(L2Hoff_1LC, count8_ML))
L2Hoff_1LC9 <- as.data.frame(sapply(L2Hoff_1LC, count9_ML))
L2Hoff_1LCn <- as.data.frame(sapply(L2Hoff_1LC, notemptycount_ML))

L2Hoff_freq_1LC <- data.frame(L2Hoff_1LCn, L2Hoff_1LC0, L2Hoff_1LC1, L2Hoff_1LC2, L2Hoff_1LC3, L2Hoff_1LC4, L2Hoff_1LC5, L2Hoff_1LC6, 
                              L2Hoff_1LC7, L2Hoff_1LC8, L2Hoff_1LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 1, HMI = "LC", variable = vars_L2Hoff_count, .before = "n")

## 2HC
L2Hoff_2HC <- data_all_L2Hoff %>%
  filter(Exp == 2 & HMI == "HC") %>%
  select(all_of(vars_L2Hoff_count))

L2Hoff_2HC0 <- as.data.frame(sapply(L2Hoff_2HC, count0_ML))
L2Hoff_2HC1 <- as.data.frame(sapply(L2Hoff_2HC, count1_ML))
L2Hoff_2HC2 <- as.data.frame(sapply(L2Hoff_2HC, count2_ML))
L2Hoff_2HC3 <- as.data.frame(sapply(L2Hoff_2HC, count3_ML))
L2Hoff_2HC4 <- as.data.frame(sapply(L2Hoff_2HC, count4_ML))
L2Hoff_2HC5 <- as.data.frame(sapply(L2Hoff_2HC, count5_ML))
L2Hoff_2HC6 <- as.data.frame(sapply(L2Hoff_2HC, count6_ML))
L2Hoff_2HC7 <- as.data.frame(sapply(L2Hoff_2HC, count7_ML))
L2Hoff_2HC8 <- as.data.frame(sapply(L2Hoff_2HC, count8_ML))
L2Hoff_2HC9 <- as.data.frame(sapply(L2Hoff_2HC, count9_ML))
L2Hoff_2HCn <- as.data.frame(sapply(L2Hoff_2HC, notemptycount_ML))

L2Hoff_freq_2HC <- data.frame(L2Hoff_2HCn, L2Hoff_2HC0, L2Hoff_2HC1, L2Hoff_2HC2, L2Hoff_2HC3, L2Hoff_2HC4, L2Hoff_2HC5, L2Hoff_2HC6, 
                              L2Hoff_2HC7, L2Hoff_2HC8, L2Hoff_2HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 2, HMI = "HC", variable = vars_L2Hoff_count, .before = "n")

## 2LC
L2Hoff_2LC <- data_all_L2Hoff %>%
  filter(Exp == 2 & HMI == "LC") %>%
  select(all_of(vars_L2Hoff_count))

L2Hoff_2LC0 <- as.data.frame(sapply(L2Hoff_2LC, count0_ML))
L2Hoff_2LC1 <- as.data.frame(sapply(L2Hoff_2LC, count1_ML))
L2Hoff_2LC2 <- as.data.frame(sapply(L2Hoff_2LC, count2_ML))
L2Hoff_2LC3 <- as.data.frame(sapply(L2Hoff_2LC, count3_ML))
L2Hoff_2LC4 <- as.data.frame(sapply(L2Hoff_2LC, count4_ML))
L2Hoff_2LC5 <- as.data.frame(sapply(L2Hoff_2LC, count5_ML))
L2Hoff_2LC6 <- as.data.frame(sapply(L2Hoff_2LC, count6_ML))
L2Hoff_2LC7 <- as.data.frame(sapply(L2Hoff_2LC, count7_ML))
L2Hoff_2LC8 <- as.data.frame(sapply(L2Hoff_2LC, count8_ML))
L2Hoff_2LC9 <- as.data.frame(sapply(L2Hoff_2LC, count9_ML))
L2Hoff_2LCn <- as.data.frame(sapply(L2Hoff_2LC, notemptycount_ML))

L2Hoff_freq_2LC <- data.frame(L2Hoff_2LCn, L2Hoff_2LC0, L2Hoff_2LC1, L2Hoff_2LC2, L2Hoff_2LC3, L2Hoff_2LC4, L2Hoff_2LC5, L2Hoff_2LC6, 
                              L2Hoff_2LC7, L2Hoff_2LC8, L2Hoff_2LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 2, HMI = "LC", variable = vars_L2Hoff_count, .before = "n")

## 3HC
L2Hoff_3HC <- data_all_L2Hoff %>%
  filter(Exp == 3 & HMI == "HC") %>%
  select(all_of(vars_L2Hoff_count))

L2Hoff_3HC0 <- as.data.frame(sapply(L2Hoff_3HC, count0_ML))
L2Hoff_3HC1 <- as.data.frame(sapply(L2Hoff_3HC, count1_ML))
L2Hoff_3HC2 <- as.data.frame(sapply(L2Hoff_3HC, count2_ML))
L2Hoff_3HC3 <- as.data.frame(sapply(L2Hoff_3HC, count3_ML))
L2Hoff_3HC4 <- as.data.frame(sapply(L2Hoff_3HC, count4_ML))
L2Hoff_3HC5 <- as.data.frame(sapply(L2Hoff_3HC, count5_ML))
L2Hoff_3HC6 <- as.data.frame(sapply(L2Hoff_3HC, count6_ML))
L2Hoff_3HC7 <- as.data.frame(sapply(L2Hoff_3HC, count7_ML))
L2Hoff_3HC8 <- as.data.frame(sapply(L2Hoff_3HC, count8_ML))
L2Hoff_3HC9 <- as.data.frame(sapply(L2Hoff_3HC, count9_ML))
L2Hoff_3HCn <- as.data.frame(sapply(L2Hoff_3HC, notemptycount_ML))

L2Hoff_freq_3HC <- data.frame(L2Hoff_3HCn, L2Hoff_3HC0, L2Hoff_3HC1, L2Hoff_3HC2, L2Hoff_3HC3, L2Hoff_3HC4, L2Hoff_3HC5, L2Hoff_3HC6, 
                              L2Hoff_3HC7, L2Hoff_3HC8, L2Hoff_3HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 3, HMI = "HC", variable = vars_L2Hoff_count, .before = "n")

## 3LC
L2Hoff_3LC <- data_all_L2Hoff %>%
  filter(Exp == 3 & HMI == "LC") %>%
  select(all_of(vars_L2Hoff_count))

L2Hoff_3LC0 <- as.data.frame(sapply(L2Hoff_3LC, count0_ML))
L2Hoff_3LC1 <- as.data.frame(sapply(L2Hoff_3LC, count1_ML))
L2Hoff_3LC2 <- as.data.frame(sapply(L2Hoff_3LC, count2_ML))
L2Hoff_3LC3 <- as.data.frame(sapply(L2Hoff_3LC, count3_ML))
L2Hoff_3LC4 <- as.data.frame(sapply(L2Hoff_3LC, count4_ML))
L2Hoff_3LC5 <- as.data.frame(sapply(L2Hoff_3LC, count5_ML))
L2Hoff_3LC6 <- as.data.frame(sapply(L2Hoff_3LC, count6_ML))
L2Hoff_3LC7 <- as.data.frame(sapply(L2Hoff_3LC, count7_ML))
L2Hoff_3LC8 <- as.data.frame(sapply(L2Hoff_3LC, count8_ML))
L2Hoff_3LC9 <- as.data.frame(sapply(L2Hoff_3LC, count9_ML))
L2Hoff_3LCn <- as.data.frame(sapply(L2Hoff_3LC, notemptycount_ML))

L2Hoff_freq_3LC <- data.frame(L2Hoff_3LCn, L2Hoff_3LC0, L2Hoff_3LC1, L2Hoff_3LC2, L2Hoff_3LC3, L2Hoff_3LC4, L2Hoff_3LC5, L2Hoff_3LC6, 
                              L2Hoff_3LC7, L2Hoff_3LC8, L2Hoff_3LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 3, HMI = "LC", variable = vars_L2Hoff_count, .before = "n")

## combine all 6 conditions/subgroups
freq_L2Hoff <- bind_rows(L2Hoff_freq_1HC, L2Hoff_freq_1LC, L2Hoff_freq_2HC, L2Hoff_freq_2LC, L2Hoff_freq_3HC, L2Hoff_freq_3LC)
rownames(freq_L2Hoff) <- c()

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

TO_1HCn <- as.data.frame(sapply(TO_1HC, notemptycount_ML))

TO_freq_1HC <- data.frame(TO_1HCn, TO_1HC0, TO_1HC1, TO_1HC2, TO_1HC3, TO_1HC4, TO_1HC5, TO_1HC6, 
                          TO_1HC7, TO_1HC8, TO_1HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
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
TO_1LCn <- as.data.frame(sapply(TO_1LC, notemptycount_ML))

TO_freq_1LC <- data.frame(TO_1LCn, TO_1LC0, TO_1LC1, TO_1LC2, TO_1LC3, TO_1LC4, TO_1LC5, TO_1LC6, 
                          TO_1LC7, TO_1LC8, TO_1LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
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
TO_2HCn <- as.data.frame(sapply(TO_2HC, notemptycount_ML))

TO_freq_2HC <- data.frame(TO_2HCn, TO_2HC0, TO_2HC1, TO_2HC2, TO_2HC3, TO_2HC4, TO_2HC5, TO_2HC6, 
                          TO_2HC7, TO_2HC8, TO_2HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
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
TO_2LCn <- as.data.frame(sapply(TO_2LC, notemptycount_ML))

TO_freq_2LC <- data.frame(TO_2LCn, TO_2LC0, TO_2LC1, TO_2LC2, TO_2LC3, TO_2LC4, TO_2LC5, TO_2LC6, 
                          TO_2LC7, TO_2LC8, TO_2LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
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
TO_3HCn <- as.data.frame(sapply(TO_3HC, notemptycount_ML))

TO_freq_3HC <- data.frame(TO_3HCn, TO_3HC0, TO_3HC1, TO_3HC2, TO_3HC3, TO_3HC4, TO_3HC5, TO_3HC6, 
                          TO_3HC7, TO_3HC8, TO_3HC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
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
TO_3LCn <- as.data.frame(sapply(TO_3LC, notemptycount_ML))

TO_freq_3LC <- data.frame(TO_3LCn, TO_3LC0, TO_3LC1, TO_3LC2, TO_3LC3, TO_3LC4, TO_3LC5, TO_3LC6, 
                          TO_3LC7, TO_3LC8, TO_3LC9) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) %>%
  add_column(Exp = 3, HMI = "LC", variable = vars_TO_count, .before = "n")

## combine all 6 conditions/subgroups
freq_TO <- bind_rows(TO_freq_1HC, TO_freq_1LC, TO_freq_2HC, TO_freq_2LC, TO_freq_3HC, TO_freq_3LC)
rownames(freq_TO) <- c()

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all_Deact", "data_all_L2Hoff", "data_all_TO", 
                        "summ_Deact", "summ_L2Hoff", "vars_L2Hoff_count", "summ_TO", "vars_TO_count", 
                        "freq_Deact", "freq_L2Hoff", "freq_TO",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

# combine tables ----------------------------------------------------------
summ_DB_all <- bind_rows(summ_Deact, summ_L2Hoff, summ_TO)
freq_DB_all <- bind_rows(freq_Deact, freq_L2Hoff, freq_TO)

#### save results ####
write_excel_csv(summ_DB_all, "data/results/summary_DB_all.csv")
write_excel_csv(freq_DB_all, "data/results/frequencies_DB_all.csv")

write_excel_csv(data_all_Deact, "data/drivingdata/data_all_Deact_corrected.csv")
write_excel_csv(data_all_L2Hoff, "data/drivingdata/data_all_L2Hoff_corrected.csv")
write_excel_csv(data_all_TO, "data/drivingdata/data_all_TO_corrected.csv")