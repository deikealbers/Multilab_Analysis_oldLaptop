#### script for TOST of usability questionnaires incl. UEQ q ####

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

# https://statsandr.com/blog/anova-in-r/#introduction
# SUS_score
# UMUX_score
# Trust
# Acceptance
# UEQ_Attractiveness
# UEQ_Perspicuity
# UEQ_Efficiency
# UEQ_Dependability
# UEQ_Novelty
# UEQ_Stimulation

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
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
data_Q <- data_all %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI))

#### prepare data sets ####
data_e1 <- data_Q %>%
  filter(Exp == 1)
data_e2 <- data_Q %>%
  filter(Exp == 2)
data_e3 <- data_Q %>%
  filter(Exp == 3)

data_12 <- data_Q %>%
  filter(Exp != 3)  
data_23 <- data_Q %>%
  filter(Exp!= 1)

#### define  column names of results table ####
c_TOST_tab <- c("Test", "diff_means", "TOST_t1", "TOST_p1", "TOST_t2", "TOST_p2", "TOST_df", "alpha", 
                "low_eqbound", "high_eqbound", "low_eqbound_d", "high_eqbound_d", "LL_CI_TOST", "UL_CI_TOST", 
                "LL_CI_TTest", "UL_CI_TTest", "NHST_t", "NHST_p",
                "lev_Df_group", "lev_Df", "lev_F", "lev_p")

c_lev <- c("lev_Df_group", "lev_Df", "lev_F", "lev_x1", "lev_p", "lev_x2")

#### define SESOI d ####
d <- 0.5

#### SUS_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(SUS_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(SUS_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))

# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$SUS_score
variable2 <- data_e2$SUS_score
variable3 <- data_e3$SUS_score
title_12 <- c("SUS_12")
title_23 <- c("SUS_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_tab12ready, TOST_tab23ready)

#### UMUX_score #### 
# levene (homogeneity)
lev_12 <- leveneTest(UMUX_score ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UMUX_score ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UMUX_score
variable2 <- data_e2$UMUX_score
variable3 <- data_e3$UMUX_score
title_12 <- c("UMUX_12")
title_23 <- c("UMUX_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### Trust #### 
# levene (homogeneity)
lev_12 <- leveneTest(Trust ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(Trust ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$Trust
variable2 <- data_e2$Trust
variable3 <- data_e3$Trust
title_12 <- c("Trust_12")
title_23 <- c("Trust_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### Acceptance #### 
# levene (homogeneity)
lev_12 <- leveneTest(Acceptance ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(Acceptance ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$Acceptance
variable2 <- data_e2$Acceptance
variable3 <- data_e3$Acceptance
title_12 <- c("Acceptance_12")
title_23 <- c("Acceptance_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### UEQ_Attractiveness #### 
# levene (homogeneity)
lev_12 <- leveneTest(UEQ_Attractiveness ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UEQ_Attractiveness ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UEQ_Attractiveness
variable2 <- data_e2$UEQ_Attractiveness
variable3 <- data_e3$UEQ_Attractiveness
title_12 <- c("UEQ_Attractiveness_12")
title_23 <- c("UEQ_Attractiveness_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### UEQ_Perspicuity #### 
# levene (homogeneity)
lev_12 <- leveneTest(UEQ_Perspicuity ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UEQ_Perspicuity ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UEQ_Perspicuity
variable2 <- data_e2$UEQ_Perspicuity
variable3 <- data_e3$UEQ_Perspicuity
title_12 <- c("UEQ_Perspicuity_12")
title_23 <- c("UEQ_Perspicuity_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### UEQ_Efficiency #### 
# levene (homogeneity)
lev_12 <- leveneTest(UEQ_Efficiency ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UEQ_Efficiency ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UEQ_Efficiency
variable2 <- data_e2$UEQ_Efficiency
variable3 <- data_e3$UEQ_Efficiency
title_12 <- c("UEQ_Efficiency_12")
title_23 <- c("UEQ_Efficiency_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### UEQ_Dependability #### 
# levene (homogeneity)
lev_12 <- leveneTest(UEQ_Dependability ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UEQ_Dependability ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UEQ_Dependability
variable2 <- data_e2$UEQ_Dependability
variable3 <- data_e3$UEQ_Dependability
title_12 <- c("UEQ_Dependability_12")
title_23 <- c("UEQ_Dependability_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### UEQ_Novelty #### 
# levene (homogeneity)
lev_12 <- leveneTest(UEQ_Novelty ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UEQ_Novelty ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UEQ_Novelty
variable2 <- data_e2$UEQ_Novelty
variable3 <- data_e3$UEQ_Novelty
title_12 <- c("UEQ_Novelty_12")
title_23 <- c("UEQ_Novelty_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### UEQ_Stimulation #### 
# levene (homogeneity)
lev_12 <- leveneTest(UEQ_Stimulation ~ Exp, data = data_12)
tab_lev_12 <- data.frame(matrix(unlist(lev_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_12) <- c_lev
tab_lev_12 <- tab_lev_12 %>%
  select(-c("lev_x1", "lev_x2"))

lev_23 <- leveneTest(UEQ_Stimulation ~ Exp, data = data_23)
tab_lev_23 <- data.frame(matrix(unlist(lev_23), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_23) <- c_lev
tab_lev_23 <- tab_lev_23 %>%
  select(-c("lev_x1", "lev_x2"))
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$UEQ_Stimulation
variable2 <- data_e2$UEQ_Stimulation
variable3 <- data_e3$UEQ_Stimulation
title_12 <- c("UEQ_Stimulation_12")
title_23 <- c("UEQ_Stimulation_23")
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
TOST_tab12ready <- cbind(title_12, TOST_tab12, tab_lev_12)
names(TOST_tab12ready) <- c_TOST_tab
TOST_tab23ready <- cbind(title_23, TOST_tab23, tab_lev_23)
names(TOST_tab23ready) <- c_TOST_tab
## build results table (first test doesnt include "TOST_results_table")
TOST_results_table <- bind_rows(TOST_results_table, TOST_tab12ready, TOST_tab23ready)

#### save data ####
write_excel_csv(TOST_results_table, "data/processed/TOST_UsabQ_inclUEQ.csv")
