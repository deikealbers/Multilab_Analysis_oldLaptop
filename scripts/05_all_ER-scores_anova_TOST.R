#### script for descriptive analysis, anova and TOST testing of experimenter rating
# https://statsandr.com/blog/anova-in-r/#introduction

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

#### preparations ####
rm(list = ls())
library(tidyverse)
library(ggplot2); 
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


#### data sets ####
# data_12 is for comparison sim (GER) vs test track (GER)
# data_23 is for comparison test track (GER) vs test track (USA)

data_12 <- data %>%
  filter(Exp == '1' | Exp == '2')

data_23 <- data %>%
  filter(Exp == '2' | Exp == '3')

#### define  column names of results table
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

#### save data ####
write_excel_csv(ER_anova, "data/processed/anova_ER.csv")
rm(list=setdiff(ls(), c("ER_anova", "data_Q", "data_12", "data_23", "fun_mean")))