#### script for the short interviews ####

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

# includes levels of automation & comparisons of 
# observed level vs instructed        --> LevelObserved_Rep_score          ### actually belongs to driving behavior
# observed level vs reported          --> LevelObserved_Instr_score
# Allocation of driving Task          --> BothAllow_Observed_score (analyses added for E-mail and HandsOff, only both should be reported though)
# # of reported Transition problems   --> TransProblems_score
# correct notice availability change  --> AvailImplem_Rep_score

#### readme ####
# The function TOSTtwo gets notification "this function is defunct. Please use tsum_TOST instead"
# https://aaroncaldwell.us/TOSTERpkg/articles/IntroductionToTOSTER.html
# both functions seem to produce the same results. For now the old function will be kept.

#### preparations ####
rm(list = ls())
library(tidyverse)
library(TOSTER);
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
data_Q <- data_all %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI))

data_e1 <- data_Q %>%
  filter(Exp == 1) %>%
  dplyr::select(c(Exp, HMI, VPNr, LevelObserved_Rep_score, LevelObserved_Instr_score, BothAllow_Observed_score, 
           EmailsAllow_Observed_score, HandsOffAllow_Observed_score, TransProblems_score, AvailImplem_Rep_score))

data_e2 <- data_Q %>%
  filter(Exp == 2) %>%
  dplyr::select(c(Exp, HMI, VPNr, LevelObserved_Rep_score, LevelObserved_Instr_score, BothAllow_Observed_score, 
                  EmailsAllow_Observed_score, HandsOffAllow_Observed_score, TransProblems_score, AvailImplem_Rep_score))

data_e3 <- data_Q %>%
  filter(Exp == 3) %>%
  dplyr::select(c(Exp, HMI, VPNr, LevelObserved_Rep_score, LevelObserved_Instr_score, BothAllow_Observed_score, 
           EmailsAllow_Observed_score, HandsOffAllow_Observed_score, TransProblems_score, AvailImplem_Rep_score))

#### define  column names of results table ####
c_TOST_tab <- c("Test", "diff_means", "TOST_t1", "TOST_p1", "TOST_t2", "TOST_p2", "TOST_df", "alpha", 
                "low_eqbound", "high_eqbound", "low_eqbound_d", "high_eqbound_d", "LL_CI_TOST", "UL_CI_TOST", 
                "LL_CI_TTest", "UL_CI_TTest", "NHST_t", "NHST_p")

#### define SESOI d ####
d <- 0.5

#### LevelObserved_Rep_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$LevelObserved_Rep_score
variable2 <- data_e2$LevelObserved_Rep_score
variable3 <- data_e3$LevelObserved_Rep_score
title_12 <- c("LevelObserved_Rep_12")
title_23 <- c("LevelObserved_Rep_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_tab12ready, TOST_tab23ready)

#### LevelObserved_Instr_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$LevelObserved_Instr_score
variable2 <- data_e2$LevelObserved_Instr_score
variable3 <- data_e3$LevelObserved_Instr_score
title_12 <- c("LevelObserved_Instr_12")
title_23 <- c("LevelObserved_Instr_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### BothAllow_Observed_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$BothAllow_Observed_score
variable2 <- data_e2$BothAllow_Observed_score
variable3 <- data_e3$BothAllow_Observed_score
title_12 <- c("BothAllow_Observed_12")
title_23 <- c("BothAllow_Observed_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### EmailsAllow_Observed_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$EmailsAllow_Observed_score
variable2 <- data_e2$EmailsAllow_Observed_score
variable3 <- data_e3$EmailsAllow_Observed_score
title_12 <- c("EmailsAllow_Observed_12")
title_23 <- c("EmailsAllow_Observed_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### HandsOffAllow_Observed_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$HandsOffAllow_Observed_score
variable2 <- data_e2$HandsOffAllow_Observed_score
variable3 <- data_e3$HandsOffAllow_Observed_score
title_12 <- c("HandsOffAllow_Observed_12")
title_23 <- c("HandsOffAllow_Observed_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### TransProblems_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$TransProblems_score
variable2 <- data_e2$TransProblems_score
variable3 <- data_e3$TransProblems_score
title_12 <- c("TransProblems_12")
title_23 <- c("TransProblems_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### AvailImplem_Rep_score #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$AvailImplem_Rep_score
variable2 <- data_e2$AvailImplem_Rep_score
variable3 <- data_e3$AvailImplem_Rep_score
title_12 <- c("AvailImplem_Rep_12")
title_23 <- c("AvailImplem_Rep_23")
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
## prepare dataframe for accumulation
TOST_tab12ready <- cbind(title_12, TOST_tab12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### save data ####
write_excel_csv(TOST_results_table, "data/processed/TOST_shortInterviews.csv")
rm(list = ls())
