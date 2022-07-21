#### script for descriptive analysis, anova and TOST testing of experimenter rating
# https://statsandr.com/blog/anova-in-r/#introduction

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

#### preparations ####
rm(list = ls())
library(tidyverse)
library(ggplot2); 
library(car)
library(compute.es); 
library(ggplot2); 
library(multcomp);
library(pastecs); 
library(reshape); 
library(dplyr)
# library(WRS);
library(DescTools);
library(TOSTER);
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
data_Q <- data_all %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI))

data <- data_Q %>%
  dplyr::select(c(Exp, HMI, VPNr, ends_with("_ER"))) %>%
  add_column(ER_overall = (.$TC01_ER + .$TC02_ER + .$TC03_ER + .$TC04_ER +
               .$TC05_ER + .$TC06_ER + .$TC07_ER + .$TC08_ER +
               .$TC09_ER + .$TC10_ER + .$TC11_ER + .$TC12_ER)/12)

#### procedure ####
# 1) Test for requirements
#     - DV is a metric variable --> most of DV are Likert scales, may be treated as metric variables
#     - IV is categorical --> HMI (2 grades) + Exp (2-3 grades)
#     - independence between groups
#     - DV shows normal distribution --> if n>25 not problematic; exp3 and some of other DV need testing
#         --> Shapiro-Wilk test [shapiro.test(anovatest$residuals)]
#              ANOVA is quite robust to small deviations from normality. This means that it is not an issue (from the perspective of
#              the interpretation of the ANOVA results) if a small number of points deviates slightly from the normality,
#              normality tests are sometimes quite conservative, meaning that the null hypothesis of normality may be rejected
#              due to a limited deviation from normality. This is especially the case with large samples as power of the test increases
#              with the sample size.
##     - Homogeneity of variances: Groups are from populations with approximately identical variances of the dependent variable
#         --> Levene's test [leveneTest(DV ~ IV[*IV], data = dataset)]
#### readme ####
# The function TOSTtwo gets notification "this function is defunct. Please use tsum_TOST instead"
# https://aaroncaldwell.us/TOSTERpkg/articles/IntroductionToTOSTER.html
# both functions seem to produce the same results. For now the old function will be kept.


#### data sets ####
# data_12 is for comparison sim (GER) vs test track (GER)
# data_23 is for comparison test track (GER) vs test track (USA)
# anova
data_12 <- data %>%
  filter(Exp == '1' | Exp == '2')
data_23 <- data %>%
  filter(Exp == '2' | Exp == '3')

# TOST
data_e1 <- data_Q %>%
  filter(Exp == 1)
data_e2 <- data_Q %>%
  filter(Exp == 2)
data_e3 <- data_Q %>%
  filter(Exp == 3)

#### anova ####

#### define  column names of anova results table
c_lev <- c("lev_Df_group", "lev_Df", "lev_F", "lev_x1", "lev_p", "lev_x2")
c_shap <- c("shap_W", "shap_p" , "method", "data")
c_aov_t2 <- c("eta_sq_Exp", "eta_sq_HMI", "eta_sq_Interaction", "eta_sq_Residuals",
              "eta_sq_part_Exp", "eta_sq_part_HMI", "eta_sq_part_Interaction", "eta_sq_part_Residuals",
              "SumSq_Exp", "SumSq_HMI", "SumSq_Interaction", "SumSq_Residuals", 
              "Df_Exp", "Df_HMI", "Df_Interaction", "Df_Residuals", 
              "MeanSq_Exp", "MeanSq_HMI", "MeanSq_Interaction", "MeanSq_Residuals", 
              "F_Exp", "F_HMI", "F_Interaction", "F_Residuals", 
              "p_Exp", "p_HMI", "p_Interaction", "p_Residuals") # used for type II (-> aov_t2) anovas; here default, since main effects are focus of research

#### anova ERo 12 ####
# anova
anova_ERo <- aov(ER_overall ~ Exp * HMI, data = data_12)
sum_anova_ERo <- Anova(anova_ERo, type="II")
tab_aov_ERo <- data.frame(matrix(unlist(EtaSq(anova_ERo, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_ERo) <- c_aov_t2
# levene (homogeneity)
levene_ERo <- leveneTest(ER_overall ~ Exp * HMI, data = data_12)
tab_levene_ERo <- data.frame(matrix(unlist(levene_ERo), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_ERo) <- c_lev
# shapiro (normality)
anova_ERo_res <- residuals(object = anova_ERo)
shapiro_ERo <- shapiro.test(x = anova_ERo_res)
tab_shapiro_ERo <- data.frame(matrix(unlist(shapiro_ERo), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_ERo) <- c_shap
# create data frame  
Test <- c("ERo 12")
tab_aov_ERo <- cbind(Test, tab_aov_ERo, tab_levene_ERo, tab_shapiro_ERo)

# combine data frames of anovas 
ER_anova_12 <- tab_aov_ERo

#### anova ERo 23 ####
# anova
anova_ERo <- aov(ER_overall ~ Exp * HMI, data = data_23)
sum_anova_ERo <- Anova(anova_ERo, type="II")
tab_aov_ERo <- data.frame(matrix(unlist(EtaSq(anova_ERo, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_ERo) <- c_aov_t2
# levene (homogeneity)
levene_ERo <- leveneTest(ER_overall ~ Exp * HMI, data = data_23)
tab_levene_ERo <- data.frame(matrix(unlist(levene_ERo), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_ERo) <- c_lev
# shapiro (normality)
anova_ERo_res <- residuals(object = anova_ERo)
shapiro_ERo <- shapiro.test(x = anova_ERo_res)
tab_shapiro_ERo <- data.frame(matrix(unlist(shapiro_ERo), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_ERo) <- c_shap
# create data frame  
Test <- c("ERo 23")
tab_aov_ERo <- cbind(Test, tab_aov_ERo, tab_levene_ERo, tab_shapiro_ERo)

# combine data frames of anovas 
ER_anova_23 <- tab_aov_ERo

#### combine anova 12 and anova 23 results ####
ER_anova <- bind_rows(ER_anova_12, ER_anova_23)

#### remove not needed data ####
rm(list=setdiff(ls(), c("ER_anova", "data_Q", "data_e1", "data_e2", "data_e3")))



#### TOST ####

#### define  column names of TOST results table ####
c_TOST_tab <- c("Test", "diff_means", "TOST_t1", "TOST_p1", "TOST_t2", "TOST_p2", "TOST_df", "alpha", 
                "low_eqbound", "high_eqbound", "low_eqbound_d", "high_eqbound_d", "LL_CI_TOST", "UL_CI_TOST", 
                "LL_CI_TTest", "UL_CI_TTest", "NHST_t", "NHST_p")

#### define TOST SESOI d ####
d <- 0.5

#### ER_overall #### 
# adaptations are needed in the next 5 rows for the different tests
variable1 <- data_e1$ER_overall
variable2 <- data_e2$ER_overall
variable3 <- data_e3$ER_overall
title_12 <- c("ERo_12")
title_23 <- c("ERo_23")
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
ER_TOST <- bind_rows(TOST_tab12ready, TOST_tab23ready)

#### remove not needed data ####
rm(list=setdiff(ls(), c("ER_anova", "ER_TOST", "data_Q")))

#### save data ####
write_excel_csv(ER_anova, "data/processed/anova_ER.csv")
write_excel_csv(ER_TOST, "data/processed/TOST_ER.csv")