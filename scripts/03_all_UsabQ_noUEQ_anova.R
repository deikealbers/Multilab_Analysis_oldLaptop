#### script for anova testing of usability questionnaires ####
# builds on script 02_all_UsabQ_scores.R
# https://statsandr.com/blog/anova-in-r/#introduction
    # SUS_score
    # UMUX_score
    # Trust
    # Acceptance

#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
library(compute.es); 
library(ggplot2); 
library(multcomp);
library(pastecs); 
library(reshape); 
# library(WRS);
library(DescTools);
setwd("~/R/Multilab")

#### load dataset ####
load("data/processed/R_data_all_Q.RData")

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

data_12 <- data_Q %>%
  filter(Exp == '1' | Exp == '2')

data_23 <- data_Q %>%
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

#### anova SUS 12 ####
# anova
anova_SUS <- aov(SUS_score ~ Exp * HMI, data = data_12)
sum_anova_SUS <- Anova(anova_SUS, type="II")
tab_aov_SUS <- data.frame(matrix(unlist(EtaSq(anova_SUS, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_SUS) <- c_aov_t2
# levene (homogeneity)
levene_SUS <- leveneTest(SUS_score ~ Exp * HMI, data = data_12)
tab_levene_SUS <- data.frame(matrix(unlist(levene_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_SUS) <- c_lev
# shapiro (normality)
anova_SUS_res <- residuals(object = anova_SUS)
shapiro_SUS <- shapiro.test(x = anova_SUS_res)
tab_shapiro_SUS <- data.frame(matrix(unlist(shapiro_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_SUS) <- c_shap
# create data frame  
Test <- c("SUS 12")
tab_aov_SUS <- cbind(Test, tab_aov_SUS, tab_levene_SUS, tab_shapiro_SUS)

#### anova UMUX 12 ####
# anova
anova_UMUX <- aov(UMUX_score ~ Exp * HMI, data = data_12)
sum_anova_UMUX <- Anova(anova_UMUX, type="II")
tab_aov_UMUX <- data.frame(matrix(unlist(EtaSq(anova_UMUX, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UMUX) <- c_aov_t2
# levene (homogeneity)
levene_UMUX <- leveneTest(UMUX_score ~ Exp * HMI, data = data_12)
tab_levene_UMUX <- data.frame(matrix(unlist(levene_UMUX), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UMUX) <- c_lev
# shapiro (normality)
anova_UMUX_res <- residuals(object = anova_UMUX)
shapiro_UMUX <- shapiro.test(x = anova_UMUX_res)
tab_shapiro_UMUX <- data.frame(matrix(unlist(shapiro_UMUX), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UMUX) <- c_shap
# create data frame  
Test <- c("UMUX 12")
tab_aov_UMUX <- cbind(Test, tab_aov_UMUX, tab_levene_UMUX, tab_shapiro_UMUX)

#### anova Trust 12 ####
# anova
anova_Trust <- aov(Trust ~ Exp * HMI, data = data_12)
sum_anova_Trust <- Anova(anova_Trust, type="II")
tab_aov_Trust <- data.frame(matrix(unlist(EtaSq(anova_Trust, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_Trust) <- c_aov_t2
# levene (homogeneity)
levene_Trust <- leveneTest(Trust ~ Exp * HMI, data = data_12)
tab_levene_Trust <- data.frame(matrix(unlist(levene_Trust), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_Trust) <- c_lev
# shapiro (normality)
anova_Trust_res <- residuals(object = anova_Trust)
shapiro_Trust <- shapiro.test(x = anova_Trust_res)
tab_shapiro_Trust <- data.frame(matrix(unlist(shapiro_Trust), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_Trust) <- c_shap
# create data frame  
Test <- c("Trust 12")
tab_aov_Trust <- cbind(Test, tab_aov_Trust, tab_levene_Trust, tab_shapiro_Trust)

#### anova Acceptance 12 ####
# anova
anova_Acceptance <- aov(Acceptance ~ Exp * HMI, data = data_12)
sum_anova_Acceptance <- Anova(anova_Acceptance, type="II")
tab_aov_Acceptance <- data.frame(matrix(unlist(EtaSq(anova_Acceptance, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_Acceptance) <- c_aov_t2
# levene (homogeneity)
levene_Acceptance <- leveneTest(Acceptance ~ Exp * HMI, data = data_12)
tab_levene_Acceptance <- data.frame(matrix(unlist(levene_Acceptance), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_Acceptance) <- c_lev
# shapiro (normality)
anova_Acceptance_res <- residuals(object = anova_Acceptance)
shapiro_Acceptance <- shapiro.test(x = anova_Acceptance_res)
tab_shapiro_Acceptance <- data.frame(matrix(unlist(shapiro_Acceptance), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_Acceptance) <- c_shap
# create data frame  
Test <- c("Acceptance 12")
tab_aov_Acceptance <- cbind(Test, tab_aov_Acceptance, tab_levene_Acceptance, tab_shapiro_Acceptance)

#### gather anova 12 results #### 
# combine data frames of anovas 
UsabQ_anova_12 <- bind_rows(tab_aov_SUS, tab_aov_UMUX, tab_aov_Trust, tab_aov_Acceptance)

#### anova SUS 23 ####
# anova
anova_SUS <- aov(SUS_score ~ Exp * HMI, data = data_23)
sum_anova_SUS <- Anova(anova_SUS, type="II")
tab_aov_SUS <- data.frame(matrix(unlist(EtaSq(anova_SUS, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_SUS) <- c_aov_t2
# levene (homogeneity)
levene_SUS <- leveneTest(SUS_score ~ Exp * HMI, data = data_23)
tab_levene_SUS <- data.frame(matrix(unlist(levene_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_SUS) <- c_lev
# shapiro (normality)
anova_SUS_res <- residuals(object = anova_SUS)
shapiro_SUS <- shapiro.test(x = anova_SUS_res)
tab_shapiro_SUS <- data.frame(matrix(unlist(shapiro_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_SUS) <- c_shap
# create data frame  
Test <- c("SUS 23")
tab_aov_SUS <- cbind(Test, tab_aov_SUS, tab_levene_SUS, tab_shapiro_SUS)

#### anova UMUX 23 ####
# anova
anova_UMUX <- aov(UMUX_score ~ Exp * HMI, data = data_23)
sum_anova_UMUX <- Anova(anova_UMUX, type="II")
tab_aov_UMUX <- data.frame(matrix(unlist(EtaSq(anova_UMUX, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UMUX) <- c_aov_t2
# levene (homogeneity)
levene_UMUX <- leveneTest(UMUX_score ~ Exp * HMI, data = data_23)
tab_levene_UMUX <- data.frame(matrix(unlist(levene_UMUX), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UMUX) <- c_lev
# shapiro (normality)
anova_UMUX_res <- residuals(object = anova_UMUX)
shapiro_UMUX <- shapiro.test(x = anova_UMUX_res)
tab_shapiro_UMUX <- data.frame(matrix(unlist(shapiro_UMUX), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UMUX) <- c_shap
# create data frame  
Test <- c("UMUX 23")
tab_aov_UMUX <- cbind(Test, tab_aov_UMUX, tab_levene_UMUX, tab_shapiro_UMUX)

#### anova Trust 23 ####
# anova
anova_Trust <- aov(Trust ~ Exp * HMI, data = data_23)
sum_anova_Trust <- Anova(anova_Trust, type="II")
tab_aov_Trust <- data.frame(matrix(unlist(EtaSq(anova_Trust, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_Trust) <- c_aov_t2
# levene (homogeneity)
levene_Trust <- leveneTest(Trust ~ Exp * HMI, data = data_23)
tab_levene_Trust <- data.frame(matrix(unlist(levene_Trust), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_Trust) <- c_lev
# shapiro (normality)
anova_Trust_res <- residuals(object = anova_Trust)
shapiro_Trust <- shapiro.test(x = anova_Trust_res)
tab_shapiro_Trust <- data.frame(matrix(unlist(shapiro_Trust), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_Trust) <- c_shap
# create data frame  
Test <- c("Trust 23")
tab_aov_Trust <- cbind(Test, tab_aov_Trust, tab_levene_Trust, tab_shapiro_Trust)

#### anova Acceptance 23 ####
# anova
anova_Acceptance <- aov(Acceptance ~ Exp * HMI, data = data_23)
sum_anova_Acceptance <- Anova(anova_Acceptance, type="II")
tab_aov_Acceptance <- data.frame(matrix(unlist(EtaSq(anova_Acceptance, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_Acceptance) <- c_aov_t2
# levene (homogeneity)
levene_Acceptance <- leveneTest(Acceptance ~ Exp * HMI, data = data_23)
tab_levene_Acceptance <- data.frame(matrix(unlist(levene_Acceptance), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_Acceptance) <- c_lev
# shapiro (normality)
anova_Acceptance_res <- residuals(object = anova_Acceptance)
shapiro_Acceptance <- shapiro.test(x = anova_Acceptance_res)
tab_shapiro_Acceptance <- data.frame(matrix(unlist(shapiro_Acceptance), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_Acceptance) <- c_shap
# create data frame  
Test <- c("Acceptance 23")
tab_aov_Acceptance <- cbind(Test, tab_aov_Acceptance, tab_levene_Acceptance, tab_shapiro_Acceptance)

#### gather anova 23 results #### 
# combine data frames of anovas 
UsabQ_anova_23 <- bind_rows(tab_aov_SUS, tab_aov_UMUX, tab_aov_Trust, tab_aov_Acceptance)

#### combine anova 12 and anova 23 results ####
UsabQ_anova <- bind_rows(UsabQ_anova_12, UsabQ_anova_23)

#### save data ####
write_excel_csv(UsabQ_anova, "data/processed/anova_UsabQ_noUEQ.csv")
rm(list=setdiff(ls(), c("UsabQ_anova", "data_Q", "data_12", "data_23", "fun_mean")))