#### script for TOST of usability questionnaires incl. UEQ q ####

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

# https://statsandr.com/blog/anova-in-r/#introduction
# ER_score
# 
# AttentionRatio_surt
# GlanceAllocationTime_ic
# 1st_glance_duration_ic
# 
# TimeToL0_max25

#### readme ####
# The function TOSTtwo gets notification "this function is defunct. Please use tsum_TOST instead"
# https://aaroncaldwell.us/TOSTERpkg/articles/IntroductionToTOSTER.html
# both functions seem to produce the same results. For now the old function will be kept.

# levene test for check of variance homogeneity

#### preparations ####
rm(list = ls())
library(tidyverse)
library(TOSTER)
library(car)
library(rstatix)
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
## SI + ER
data_ER_SI <- read.csv("data/preprocessed/multilab_subj_complete.csv", encoding = "UTF-8") %>%
  select(Exp, HMI, VPNr,
         LevelObserved_Rep_score, 
         EmailsAllow_Observed_score, HandsOffAllow_Observed_score,
         LevelObserved_Instr_score,
         AvailImplem_Rep_score, AvailReasonCorrect_score,
         TransProblems_score,
         ER_overall) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI))

## ET
data_all_TO <- read.csv("data/eyetracking/data_all_TO.csv", encoding = "UTF-8") %>%
  dplyr::rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI)) %>%
  mutate(GlanceAllocationTime_ic = (TC10_ic_glance_allocation_time + TC12_ic_glance_allocation_time) /2, .before = "TC10_comment") %>%
  mutate(FirstGlanceDuration_ic = (TC10_ic_1st_glance_duration_without_start + TC12_ic_1st_glance_duration_without_start) /2, .before = "TC10_comment")

data_all_AR <- read.csv("data/eyetracking/data_all_AR.csv", encoding = "UTF-8") %>%
  dplyr::rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI)) %>%
  mutate(L0_surt = 100 - TC01_AR_surt, .before = "TC01_duration") %>%
  mutate(L2_surt = 100 - TC07_AR_surt, .before = "TC01_duration") %>%
  mutate(L3_surt = TC04_AR_surt, .before = "TC01_duration") %>%
  mutate(allLvl_AR_surt = (L0_surt + L2_surt + L3_surt) /3, .before = "TC01_duration")
  
## DB
data_all_DB <- read.csv("data/drivingdata/data_all_TO_corrected.csv", encoding = "UTF-8") %>%
  dplyr::rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI)) %>%
  mutate(TC10_TimeToL0_max25 = ifelse(TC10_TimeToL0 > 25, 25, TC10_TimeToL0), .before = "TC10_TimeToL0") %>%
  mutate(TC12_TimeToL0_max25 = ifelse(TC12_TimeToL0 > 25, 25, TC12_TimeToL0), .before = "TC12_TimeToL0") %>%
  mutate(TC10_TimeToFirstAction_max25 = ifelse(TC10_TimeToFirstAction > 25, 25, TC10_TimeToFirstAction), .before = "TC10_TimeToFirstAction") %>%
  mutate(TC12_TimeToFirstAction_max25 = ifelse(TC12_TimeToFirstAction > 25, 25, TC12_TimeToFirstAction), .before = "TC12_TimeToFirstAction") %>%
  mutate(TimeToL0_max25 = (TC10_TimeToL0_max25 + TC12_TimeToL0_max25) /2, .before = "TC10_Aut_Lvl_Sta")


#### define  column names of results table ####
c_TOST_tab <- c("Test", "diff_means", "TOST_t1", "TOST_p1", "TOST_t2", "TOST_p2", "TOST_df", "alpha", 
                "low_eqbound", "high_eqbound", "low_eqbound_d", "high_eqbound_d", "LL_CI_TOST", "UL_CI_TOST", 
                "LL_CI_TTest", "UL_CI_TTest", "NHST_t", "NHST_p",
                "lev_Df_group", "lev_Df", "lev_F", "lev_p",
                "shap_Exp1", "shap_Exp2", "shap_W1", "shap_W2", "shap_p1", "shap_p2",
                "wTOST_W_NHST", "wTOST_W_TOSTlow", "wTOST_W_TOSTupp", 
                "wTOST_p_NHST", "wTOST_p_TOSTlow", "wTOST_p_TOSTupp",
                "wTOST_low_eqbound", "wTOST_high_eqbound", "wTOST_alpha",
                "wTOST_test", "wTOST_decision_combined")

c_lev <- c("lev_Df_group", "lev_Df", "lev_F", "lev_x1", "lev_p", "lev_x2")
c_shap <- c("shap_Exp1", "shap_Exp2", "shap_AV1", "shap_AV2", "shap_W1", "shap_W2", "shap_p1", "shap_p2")

c_wTOST <- c("wTOST_W_NHST", "wTOST_W_TOSTlow", "wTOST_W_TOSTupp", 
             "wTOST_p_NHST", "wTOST_p_TOSTlow", "wTOST_p_TOSTupp",
             "wTOST_low_eqbound", "wTOST_high_eqbound", "wTOST_alpha",
             "wTOST_test", "wTOST_hypothesis", 
             "wTOST_est_med", "wTOST_est_corr",
             "wTOST_lowci_med", "wTOST_lowci_corr",
             "wTOST_uppci_med", "wTOST_uppci_corr",
             "wTOST_conflevel_med", "wTOST_convlevel_corr",
             "wTOST_est_corr2", "wTOST_lowci_corr2", "wTOST_uppci_corr2",
             "wTOST_paired", "wTOST_shift", 
             "wTOST_decision_eq", "wTOST_decision_NHST", "wTOST_decision_combined")

#### define SESOI d ####
d <- 0.5

################################## ________ ER 12 + 23 ________ ########################################
#### prepare data sets ER ####
data_e1 <- data_ER_SI %>%
  filter(Exp == 1)
data_e2 <- data_ER_SI %>%
  filter(Exp == 2)
data_e3 <- data_ER_SI %>%
  filter(Exp == 3)

data_12 <- data_ER_SI %>%
  filter(Exp != 3)  
data_23 <- data_ER_SI %>%
  filter(Exp!= 1)
#### ER #### 
# levene (homogeneity)
lev_12 <- leveneTest(ER_overall ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(ER_overall ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(ER_overall)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(ER_overall)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$ER_overall
variable2 <- data_e2$ER_overall
variable3 <- data_e3$ER_overall
title_12 <- c("ER_12")
title_23 <- c("ER_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2)
sd2 <- sd(variable2)
n2 <- length(variable2)
m3 <- mean(variable3)
sd3 <- sd(variable3)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(ER_overall ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(ER_overall ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_tab12ready, TOST_tab23ready)

################################## ________ SI 12 + 23 ________ ########################################
#### prepare data sets SI ####
data_e1 <- data_ER_SI %>%
  filter(Exp == 1)
data_e2 <- data_ER_SI %>%
  filter(Exp == 2)
data_e3 <- data_ER_SI %>%
  filter(Exp == 3)

data_12 <- data_ER_SI %>%
  filter(Exp != 3)  
data_23 <- data_ER_SI %>%
  filter(Exp!= 1)
#### LevelObserved_Rep_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(LevelObserved_Rep_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(LevelObserved_Rep_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(LevelObserved_Rep_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(LevelObserved_Rep_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$LevelObserved_Rep_score
variable2 <- data_e2$LevelObserved_Rep_score
variable3 <- data_e3$LevelObserved_Rep_score
title_12 <- c("LevelObserved_Rep_score_12")
title_23 <- c("LevelObserved_Rep_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(LevelObserved_Rep_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(LevelObserved_Rep_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### EmailsAllow_Observed_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(EmailsAllow_Observed_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(EmailsAllow_Observed_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(EmailsAllow_Observed_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(EmailsAllow_Observed_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$EmailsAllow_Observed_score
variable2 <- data_e2$EmailsAllow_Observed_score
variable3 <- data_e3$EmailsAllow_Observed_score
title_12 <- c("EmailsAllow_Observed_score_12")
title_23 <- c("EmailsAllow_Observed_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(EmailsAllow_Observed_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(EmailsAllow_Observed_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### HandsOffAllow_Observed_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(HandsOffAllow_Observed_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(HandsOffAllow_Observed_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(HandsOffAllow_Observed_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(HandsOffAllow_Observed_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$HandsOffAllow_Observed_score
variable2 <- data_e2$HandsOffAllow_Observed_score
variable3 <- data_e3$HandsOffAllow_Observed_score
title_12 <- c("HandsOffAllow_Observed_score_12")
title_23 <- c("HandsOffAllow_Observed_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(HandsOffAllow_Observed_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(HandsOffAllow_Observed_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
#### LevelObserved_Instr_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(LevelObserved_Instr_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(LevelObserved_Instr_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(LevelObserved_Instr_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(LevelObserved_Instr_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$LevelObserved_Instr_score
variable2 <- data_e2$LevelObserved_Instr_score
variable3 <- data_e3$LevelObserved_Instr_score
title_12 <- c("LevelObserved_Instr_score_12")
title_23 <- c("LevelObserved_Instr_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(LevelObserved_Instr_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(LevelObserved_Instr_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
#### AvailImplem_Rep_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(AvailImplem_Rep_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(AvailImplem_Rep_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(AvailImplem_Rep_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(AvailImplem_Rep_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$AvailImplem_Rep_score
variable2 <- data_e2$AvailImplem_Rep_score
variable3 <- data_e3$AvailImplem_Rep_score
title_12 <- c("AvailImplem_Rep_score_12")
title_23 <- c("AvailImplem_Rep_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(AvailImplem_Rep_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(AvailImplem_Rep_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
#### AvailReasonCorrect_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(AvailReasonCorrect_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(AvailReasonCorrect_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(AvailReasonCorrect_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(AvailReasonCorrect_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$AvailReasonCorrect_score
variable2 <- data_e2$AvailReasonCorrect_score
variable3 <- data_e3$AvailReasonCorrect_score
title_12 <- c("AvailReasonCorrect_score_12")
title_23 <- c("AvailReasonCorrect_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(AvailReasonCorrect_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(AvailReasonCorrect_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
#### TransProblems_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(TransProblems_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(TransProblems_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(TransProblems_score)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(TransProblems_score)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$TransProblems_score
variable2 <- data_e2$TransProblems_score
variable3 <- data_e3$TransProblems_score
title_12 <- c("TransProblems_score_12")
title_23 <- c("TransProblems_score_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(TransProblems_score ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(TransProblems_score ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
################################## ________ ET 12 + 23 ________ ########################################
#### prepare data sets AttentionRatio_surt ####
data_e1 <- data_all_AR %>%
  filter(Exp == 1)
data_e2 <- data_all_AR %>%
  filter(Exp == 2)
data_e3 <- data_all_AR %>%
  filter(Exp == 3)

data_12 <- data_all_AR %>%
  filter(Exp != 3)  
data_23 <- data_all_AR %>%
  filter(Exp!= 1)
# #### L0_surt #### 
# # levene (homogeneity)
# lev_12 <- leveneTest(TC01_AR_surt ~ Exp, data = data_12)
# tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_lev_12) <- c_lev
# tab_lev_12 <- tab_lev_12 %>%
#   select(-c("lev_x1", "lev_x2"))
# 
# lev_23 <- leveneTest(TC01_AR_surt ~ Exp, data = data_23)
# tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_lev_23) <- c_lev
# tab_lev_23 <- tab_lev_23 %>%
#   select(-c("lev_x1", "lev_x2"))
# 
# # shapiro-wilk (normality)
# shap_12 <- data_12 %>%
#   group_by(Exp) %>%
#   shapiro_test(TC01_AR_surt)
# tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_shap_12) <- c_shap
# tab_shap_12 <- tab_shap_12 %>%
#   select(-c("shap_AV1", "shap_AV2"))
# 
# shap_23 <- data_23 %>%
#   group_by(Exp) %>%
#   shapiro_test(TC01_AR_surt)
# tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_shap_23) <- c_shap
# tab_shap_23 <- tab_shap_23 %>%
#   select(-c("shap_AV1", "shap_AV2"))
# 
# # adaptations are needed in the next 5 rows for the different tests
# variable1 <- data_e1$TC01_AR_surt
# variable2 <- data_e2$TC01_AR_surt
# variable3 <- data_e3$TC01_AR_surt
# title_12 <- c("L0_surt_12")
# title_23 <- c("L0_surt_23")
# ## define parameters
# m1 <- mean(variable1)
# sd1 <- sd(variable1)
# n1 <- length(variable1)
# m2 <- mean(variable2, na.rm = TRUE)
# sd2 <- sd(variable2, na.rm = TRUE)
# n2 <- length(variable2)
# m3 <- mean(variable3, na.rm = TRUE)
# sd3 <- sd(variable3, na.rm = TRUE)
# n3 <- length(variable3)
# ## calculate TOST
# TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
# TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
# ## transform results into dataframe
# TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# 
# # Wilcoxon TOST as alternative #
# d_raw12 = TOST_tab12$X9
# w_TOST_12 <- wilcox_TOST(TC01_AR_surt ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
# w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(w_TOST_tab12) <- c_wTOST
# w_TOST_tab12 <- w_TOST_tab12 %>%
#   select(-c(wTOST_hypothesis:wTOST_decision_NHST))
# 
# d_raw23 = TOST_tab23$X9
# w_TOST_23 <- wilcox_TOST(TC01_AR_surt ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
# w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(w_TOST_tab23) <- c_wTOST
# w_TOST_tab23 <- w_TOST_tab23 %>%
#   select(-c(wTOST_hypothesis:wTOST_decision_NHST))
# 
# ## prepare dataframe for accumulation
# TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
# names(TOST_tab12ready) <- c_TOST_tab
# TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
# names(TOST_tab23ready) <- c_TOST_tab
# ## build results table (first test doesnt include "TOST_results_table")
# TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
# 
# 
# #### L2_surt #### 
# # levene (homogeneity)
# lev_12 <- leveneTest(TC07_AR_surt ~ Exp, data = data_12)
# tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_lev_12) <- c_lev
# tab_lev_12 <- tab_lev_12 %>%
#   select(-c("lev_x1", "lev_x2"))
# 
# lev_23 <- leveneTest(TC07_AR_surt ~ Exp, data = data_23)
# tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_lev_23) <- c_lev
# tab_lev_23 <- tab_lev_23 %>%
#   select(-c("lev_x1", "lev_x2"))
# 
# # shapiro-wilk (normality)
# shap_12 <- data_12 %>%
#   group_by(Exp) %>%
#   shapiro_test(TC07_AR_surt)
# tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_shap_12) <- c_shap
# tab_shap_12 <- tab_shap_12 %>%
#   select(-c("shap_AV1", "shap_AV2"))
# 
# shap_23 <- data_23 %>%
#   group_by(Exp) %>%
#   shapiro_test(TC07_AR_surt)
# tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_shap_23) <- c_shap
# tab_shap_23 <- tab_shap_23 %>%
#   select(-c("shap_AV1", "shap_AV2"))
# 
# # adaptations are needed in the next 5 rows for the different tests
# variable1 <- data_e1$TC07_AR_surt
# variable2 <- data_e2$TC07_AR_surt
# variable3 <- data_e3$TC07_AR_surt
# title_12 <- c("L2_surt_12")
# title_23 <- c("L2_surt_23")
# ## define parameters
# m1 <- mean(variable1)
# sd1 <- sd(variable1)
# n1 <- length(variable1)
# m2 <- mean(variable2, na.rm = TRUE)
# sd2 <- sd(variable2, na.rm = TRUE)
# n2 <- length(variable2)
# m3 <- mean(variable3, na.rm = TRUE)
# sd3 <- sd(variable3, na.rm = TRUE)
# n3 <- length(variable3)
# ## calculate TOST
# TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
# TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
# ## transform results into dataframe
# TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# 
# # Wilcoxon TOST as alternative #
# d_raw12 = TOST_tab12$X9
# w_TOST_12 <- wilcox_TOST(TC07_AR_surt ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
# w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(w_TOST_tab12) <- c_wTOST
# w_TOST_tab12 <- w_TOST_tab12 %>%
#   select(-c(wTOST_hypothesis:wTOST_decision_NHST))
# 
# d_raw23 = TOST_tab23$X9
# w_TOST_23 <- wilcox_TOST(TC07_AR_surt ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
# w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(w_TOST_tab23) <- c_wTOST
# w_TOST_tab23 <- w_TOST_tab23 %>%
#   select(-c(wTOST_hypothesis:wTOST_decision_NHST))
# 
# ## prepare dataframe for accumulation
# TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
# names(TOST_tab12ready) <- c_TOST_tab
# TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
# names(TOST_tab23ready) <- c_TOST_tab
# ## build results table (first test doesnt include "TOST_results_table")
# TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
# 
# 
# #### L3_surt #### 
# # levene (homogeneity)
# lev_12 <- leveneTest(TC04_AR_surt ~ Exp, data = data_12)
# tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_lev_12) <- c_lev
# tab_lev_12 <- tab_lev_12 %>%
#   select(-c("lev_x1", "lev_x2"))
# 
# lev_23 <- leveneTest(TC04_AR_surt ~ Exp, data = data_23)
# tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_lev_23) <- c_lev
# tab_lev_23 <- tab_lev_23 %>%
#   select(-c("lev_x1", "lev_x2"))
# 
# # shapiro-wilk (normality)
# shap_12 <- data_12 %>%
#   group_by(Exp) %>%
#   shapiro_test(TC04_AR_surt)
# tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_shap_12) <- c_shap
# tab_shap_12 <- tab_shap_12 %>%
#   select(-c("shap_AV1", "shap_AV2"))
# 
# shap_23 <- data_23 %>%
#   group_by(Exp) %>%
#   shapiro_test(TC04_AR_surt)
# tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_shap_23) <- c_shap
# tab_shap_23 <- tab_shap_23 %>%
#   select(-c("shap_AV1", "shap_AV2"))
# 
# # adaptations are needed in the next 5 rows for the different tests
# variable1 <- data_e1$TC04_AR_surt
# variable2 <- data_e2$TC04_AR_surt
# variable3 <- data_e3$TC04_AR_surt
# title_12 <- c("L3_surt_12")
# title_23 <- c("L3_surt_23")
# ## define parameters
# m1 <- mean(variable1)
# sd1 <- sd(variable1)
# n1 <- length(variable1)
# m2 <- mean(variable2, na.rm = TRUE)
# sd2 <- sd(variable2, na.rm = TRUE)
# n2 <- length(variable2)
# m3 <- mean(variable3, na.rm = TRUE)
# sd3 <- sd(variable3, na.rm = TRUE)
# n3 <- length(variable3)
# ## calculate TOST
# TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
# TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
# ## transform results into dataframe
# TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# 
# # Wilcoxon TOST as alternative #
# d_raw12 = TOST_tab12$X9
# w_TOST_12 <- wilcox_TOST(TC04_AR_surt ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
# w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(w_TOST_tab12) <- c_wTOST
# w_TOST_tab12 <- w_TOST_tab12 %>%
#   select(-c(wTOST_hypothesis:wTOST_decision_NHST))
# 
# d_raw23 = TOST_tab23$X9
# w_TOST_23 <- wilcox_TOST(TC04_AR_surt ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
# w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(w_TOST_tab23) <- c_wTOST
# w_TOST_tab23 <- w_TOST_tab23 %>%
#   select(-c(wTOST_hypothesis:wTOST_decision_NHST))
# 
# ## prepare dataframe for accumulation
# TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
# names(TOST_tab12ready) <- c_TOST_tab
# TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
# names(TOST_tab23ready) <- c_TOST_tab
# ## build results table (first test doesnt include "TOST_results_table")
# TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)
# 

#### allLvl_AR_surt #### 
# levene (homogeneity)
lev_12 <- leveneTest(allLvl_AR_surt ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(allLvl_AR_surt ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(allLvl_AR_surt)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(allLvl_AR_surt)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$allLvl_AR_surt
variable2 <- data_e2$allLvl_AR_surt
variable3 <- data_e3$allLvl_AR_surt
title_12 <- c("allLvl_AR_surt_12")
title_23 <- c("allLvl_AR_surt_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(allLvl_AR_surt ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(allLvl_AR_surt ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)



#### prepare data sets Eye-Tracking for TO ####
data_e1 <- data_all_TO %>%
  filter(Exp == 1)
data_e2 <- data_all_TO %>%
  filter(Exp == 2)
data_e3 <- data_all_TO %>%
  filter(Exp == 3)

data_12 <- data_all_TO %>%
  filter(Exp != 3)  
data_23 <- data_all_TO %>%
  filter(Exp!= 1)
#### GlanceAllocationTime_ic #### 
# levene (homogeneity)
lev_12 <- leveneTest(GlanceAllocationTime_ic ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(GlanceAllocationTime_ic ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(GlanceAllocationTime_ic)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(GlanceAllocationTime_ic)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$GlanceAllocationTime_ic
variable2 <- data_e2$GlanceAllocationTime_ic
variable3 <- data_e3$GlanceAllocationTime_ic
title_12 <- c("GlanceAllocationTime_ic_12")
title_23 <- c("GlanceAllocationTime_ic_23")
## define parameters
m1 <- mean(variable1, na.rm = TRUE)
sd1 <- sd(variable1, na.rm = TRUE)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(GlanceAllocationTime_ic ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(GlanceAllocationTime_ic ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### FirstGlanceDuration_ic #### 
# levene (homogeneity)
lev_12 <- leveneTest(FirstGlanceDuration_ic ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(FirstGlanceDuration_ic ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(FirstGlanceDuration_ic)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(FirstGlanceDuration_ic)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$FirstGlanceDuration_ic
variable2 <- data_e2$FirstGlanceDuration_ic
variable3 <- data_e3$FirstGlanceDuration_ic
title_12 <- c("FirstGlanceDuration_ic_12")
title_23 <- c("FirstGlanceDuration_ic_23")
## define parameters
m1 <- mean(variable1, na.rm = TRUE)
sd1 <- sd(variable1, na.rm = TRUE)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(FirstGlanceDuration_ic ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(FirstGlanceDuration_ic ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

################################## ________ DB 12 + 23 ________ ########################################
#### prepare data sets TimeToL0_max25 ####
data_e1 <- data_all_DB %>%
  filter(Exp == 1)
data_e2 <- data_all_DB %>%
  filter(Exp == 2)
data_e3 <- data_all_DB %>%
  filter(Exp == 3)

data_12 <- data_all_DB %>%
  filter(Exp != 3)  
data_23 <- data_all_DB %>%
  filter(Exp!= 1)
#### TimeToL0_max25 #### 
# levene (homogeneity)
lev_12 <- leveneTest(TimeToL0_max25 ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(TimeToL0_max25 ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# shapiro-wilk (normality)
shap_12 <- data_12 %>%
  group_by(Exp) %>%
  shapiro_test(TimeToL0_max25)
tab_shap_12 <- data.frame(matrix(unlist(shap_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_12) <- c_shap
tab_shap_12 <- tab_shap_12 %>%
  select(-c("shap_AV1", "shap_AV2"))

shap_23 <- data_23 %>%
  group_by(Exp) %>%
  shapiro_test(TimeToL0_max25)
tab_shap_23 <- data.frame(matrix(unlist(shap_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shap_23) <- c_shap
tab_shap_23 <- tab_shap_23 %>%
  select(-c("shap_AV1", "shap_AV2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$TimeToL0_max25
variable2 <- data_e2$TimeToL0_max25
variable3 <- data_e3$TimeToL0_max25
title_12 <- c("TimeToL0_max25_12")
title_23 <- c("TimeToL0_max25_23")
## define parameters
m1 <- mean(variable1)
sd1 <- sd(variable1)
n1 <- length(variable1)
m2 <- mean(variable2, na.rm = TRUE)
sd2 <- sd(variable2, na.rm = TRUE)
n2 <- length(variable2)
m3 <- mean(variable3, na.rm = TRUE)
sd3 <- sd(variable3, na.rm = TRUE)
n3 <- length(variable3)
## calculate TOST
TOST_12 <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, low_eqbound_d = -d, high_eqbound_d = d)
TOST_23 <- TOSTtwo(m1 = m2, m2 = m3, sd1 = sd2, sd2 = sd3, n1 = n2, n2 = n3, low_eqbound_d = -d, high_eqbound_d = d)
## transform results into dataframe
TOST_tab12 <- data.frame(matrix(unlist(TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
TOST_tab23 <- data.frame(matrix(unlist(TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)

# Wilcoxon TOST as alternative #
d_raw12 = TOST_tab12$X9
w_TOST_12 <- wilcox_TOST(TimeToL0_max25 ~ Exp, data = data_12, low_eqbound = - d_raw12, high_eqbound = d_raw12)
w_TOST_tab12 <- data.frame(matrix(unlist(w_TOST_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab12) <- c_wTOST
w_TOST_tab12 <- w_TOST_tab12 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

d_raw23 = TOST_tab23$X9
w_TOST_23 <- wilcox_TOST(TimeToL0_max25 ~ Exp, data = data_23, low_eqbound = - d_raw23, high_eqbound = d_raw23)
w_TOST_tab23 <- data.frame(matrix(unlist(w_TOST_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(w_TOST_tab23) <- c_wTOST
w_TOST_tab23 <- w_TOST_tab23 %>%
  select(-c(wTOST_hypothesis:wTOST_decision_NHST))

## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12, tab_shap_12, w_TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23, tab_shap_23, w_TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

################################## ________ save ________ ########################################
write_excel_csv(TOST_results_table, "data/results/TOST_ER,SI,ET,DB.csv")
rm(list=setdiff(ls(), c("TOST_results_table")))
