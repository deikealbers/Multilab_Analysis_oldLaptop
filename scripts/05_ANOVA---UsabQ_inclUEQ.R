#### script for anova testing of usability questionnaires ####

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

# type III analysis with sum contrasts

#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
# library(compute.es)
library(DescTools);
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
data_Q <- data_all %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI))

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

options(contrasts=c("contr.sum", "contr.poly"))

################################## ________ anova 12 ________ ########################################
#### anova SUS 12 ####
# anova
anova_SUS <- aov(SUS_score ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_SUS <- Anova(anova_SUS, type="III")
tab_aov_SUS <- data.frame(matrix(unlist(EtaSq(anova_SUS, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UMUX <- aov(UMUX_score ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UMUX <- Anova(anova_UMUX, type="III")
tab_aov_UMUX <- data.frame(matrix(unlist(EtaSq(anova_UMUX, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_Trust <- aov(Trust ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_Trust <- Anova(anova_Trust, type="III")
tab_aov_Trust <- data.frame(matrix(unlist(EtaSq(anova_Trust, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_Acceptance <- aov(Acceptance ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_Acceptance <- Anova(anova_Acceptance, type="III")
tab_aov_Acceptance <- data.frame(matrix(unlist(EtaSq(anova_Acceptance, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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

#### anova UEQ_Attractiveness 12 ####
# anova
anova_UEQ_Attractiveness <- aov(UEQ_Attractiveness ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UEQ_Attractiveness <- Anova(anova_UEQ_Attractiveness, type="III")
tab_aov_UEQ_Attractiveness <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Attractiveness, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Attractiveness) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Attractiveness <- leveneTest(UEQ_Attractiveness ~ Exp * HMI, data = data_12)
tab_levene_UEQ_Attractiveness <- data.frame(matrix(unlist(levene_UEQ_Attractiveness), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Attractiveness) <- c_lev
# shapiro (normality)
anova_UEQ_Attractiveness_res <- residuals(object = anova_UEQ_Attractiveness)
shapiro_UEQ_Attractiveness <- shapiro.test(x = anova_UEQ_Attractiveness_res)
tab_shapiro_UEQ_Attractiveness <- data.frame(matrix(unlist(shapiro_UEQ_Attractiveness), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Attractiveness) <- c_shap
# create data frame  
Test <- c("UEQ_Attractiveness 12")
tab_aov_UEQ_Attractiveness <- cbind(Test, tab_aov_UEQ_Attractiveness, tab_levene_UEQ_Attractiveness, tab_shapiro_UEQ_Attractiveness)

#### anova UEQ_Perspicuity 12 ####
# anova
anova_UEQ_Perspicuity <- aov(UEQ_Perspicuity ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UEQ_Perspicuity <- Anova(anova_UEQ_Perspicuity, type="III")
tab_aov_UEQ_Perspicuity <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Perspicuity, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Perspicuity) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Perspicuity <- leveneTest(UEQ_Perspicuity ~ Exp * HMI, data = data_12)
tab_levene_UEQ_Perspicuity <- data.frame(matrix(unlist(levene_UEQ_Perspicuity), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Perspicuity) <- c_lev
# shapiro (normality)
anova_UEQ_Perspicuity_res <- residuals(object = anova_UEQ_Perspicuity)
shapiro_UEQ_Perspicuity <- shapiro.test(x = anova_UEQ_Perspicuity_res)
tab_shapiro_UEQ_Perspicuity <- data.frame(matrix(unlist(shapiro_UEQ_Perspicuity), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Perspicuity) <- c_shap
# create data frame  
Test <- c("UEQ_Perspicuity 12")
tab_aov_UEQ_Perspicuity <- cbind(Test, tab_aov_UEQ_Perspicuity, tab_levene_UEQ_Perspicuity, tab_shapiro_UEQ_Perspicuity)

#### anova UEQ_Efficiency 12 ####
# anova
anova_UEQ_Efficiency <- aov(UEQ_Efficiency ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UEQ_Efficiency <- Anova(anova_UEQ_Efficiency, type="III")
tab_aov_UEQ_Efficiency <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Efficiency, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Efficiency) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Efficiency <- leveneTest(UEQ_Efficiency ~ Exp * HMI, data = data_12)
tab_levene_UEQ_Efficiency <- data.frame(matrix(unlist(levene_UEQ_Efficiency), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Efficiency) <- c_lev
# shapiro (normality)
anova_UEQ_Efficiency_res <- residuals(object = anova_UEQ_Efficiency)
shapiro_UEQ_Efficiency <- shapiro.test(x = anova_UEQ_Efficiency_res)
tab_shapiro_UEQ_Efficiency <- data.frame(matrix(unlist(shapiro_UEQ_Efficiency), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Efficiency) <- c_shap
# create data frame  
Test <- c("UEQ_Efficiency 12")
tab_aov_UEQ_Efficiency <- cbind(Test, tab_aov_UEQ_Efficiency, tab_levene_UEQ_Efficiency, tab_shapiro_UEQ_Efficiency)

#### anova UEQ_Dependability 12 ####
# anova
anova_UEQ_Dependability <- aov(UEQ_Dependability ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UEQ_Dependability <- Anova(anova_UEQ_Dependability, type="III")
tab_aov_UEQ_Dependability <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Dependability, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Dependability) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Dependability <- leveneTest(UEQ_Dependability ~ Exp * HMI, data = data_12)
tab_levene_UEQ_Dependability <- data.frame(matrix(unlist(levene_UEQ_Dependability), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Dependability) <- c_lev
# shapiro (normality)
anova_UEQ_Dependability_res <- residuals(object = anova_UEQ_Dependability)
shapiro_UEQ_Dependability <- shapiro.test(x = anova_UEQ_Dependability_res)
tab_shapiro_UEQ_Dependability <- data.frame(matrix(unlist(shapiro_UEQ_Dependability), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Dependability) <- c_shap
# create data frame  
Test <- c("UEQ_Dependability 12")
tab_aov_UEQ_Dependability <- cbind(Test, tab_aov_UEQ_Dependability, tab_levene_UEQ_Dependability, tab_shapiro_UEQ_Dependability)

#### anova UEQ_Novelty 12 ####
# anova
anova_UEQ_Novelty <- aov(UEQ_Novelty ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UEQ_Novelty <- Anova(anova_UEQ_Novelty, type="III")
tab_aov_UEQ_Novelty <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Novelty, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Novelty) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Novelty <- leveneTest(UEQ_Novelty ~ Exp * HMI, data = data_12)
tab_levene_UEQ_Novelty <- data.frame(matrix(unlist(levene_UEQ_Novelty), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Novelty) <- c_lev
# shapiro (normality)
anova_UEQ_Novelty_res <- residuals(object = anova_UEQ_Novelty)
shapiro_UEQ_Novelty <- shapiro.test(x = anova_UEQ_Novelty_res)
tab_shapiro_UEQ_Novelty <- data.frame(matrix(unlist(shapiro_UEQ_Novelty), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Novelty) <- c_shap
# create data frame  
Test <- c("UEQ_Novelty 12")
tab_aov_UEQ_Novelty <- cbind(Test, tab_aov_UEQ_Novelty, tab_levene_UEQ_Novelty, tab_shapiro_UEQ_Novelty)

#### anova UEQ_Stimulation 12 ####
# anova
anova_UEQ_Stimulation <- aov(UEQ_Stimulation ~ Exp * HMI, data = data_12, contrasts = "contr.sum")
sum_anova_UEQ_Stimulation <- Anova(anova_UEQ_Stimulation, type="III")
tab_aov_UEQ_Stimulation <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Stimulation, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Stimulation) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Stimulation <- leveneTest(UEQ_Stimulation ~ Exp * HMI, data = data_12)
tab_levene_UEQ_Stimulation <- data.frame(matrix(unlist(levene_UEQ_Stimulation), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Stimulation) <- c_lev
# shapiro (normality)
anova_UEQ_Stimulation_res <- residuals(object = anova_UEQ_Stimulation)
shapiro_UEQ_Stimulation <- shapiro.test(x = anova_UEQ_Stimulation_res)
tab_shapiro_UEQ_Stimulation <- data.frame(matrix(unlist(shapiro_UEQ_Stimulation), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Stimulation) <- c_shap
# create data frame  
Test <- c("UEQ_Stimulation 12")
tab_aov_UEQ_Stimulation <- cbind(Test, tab_aov_UEQ_Stimulation, tab_levene_UEQ_Stimulation, tab_shapiro_UEQ_Stimulation)


#### gather anova 12 results #### 
# combine data frames of anovas 
UsabQ_anova_12 <- bind_rows(tab_aov_SUS, tab_aov_UMUX, 
                            tab_aov_Trust, tab_aov_Acceptance, 
                            tab_aov_UEQ_Attractiveness, tab_aov_UEQ_Perspicuity, tab_aov_UEQ_Efficiency, 
                            tab_aov_UEQ_Dependability, tab_aov_UEQ_Novelty, tab_aov_UEQ_Stimulation)

################################## ________ anova 23 ________ ########################################
#### anova SUS 23 ####
# anova
anova_SUS <- aov(SUS_score ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_SUS <- Anova(anova_SUS, type="III")
tab_aov_SUS <- data.frame(matrix(unlist(EtaSq(anova_SUS, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UMUX <- aov(UMUX_score ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UMUX <- Anova(anova_UMUX, type="III")
tab_aov_UMUX <- data.frame(matrix(unlist(EtaSq(anova_UMUX, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_Trust <- aov(Trust ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_Trust <- Anova(anova_Trust, type="III")
tab_aov_Trust <- data.frame(matrix(unlist(EtaSq(anova_Trust, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_Acceptance <- aov(Acceptance ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_Acceptance <- Anova(anova_Acceptance, type="III")
tab_aov_Acceptance <- data.frame(matrix(unlist(EtaSq(anova_Acceptance, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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

#### anova UEQ_Attractiveness 23 ####
# anova
anova_UEQ_Attractiveness <- aov(UEQ_Attractiveness ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UEQ_Attractiveness <- Anova(anova_UEQ_Attractiveness, type="III")
tab_aov_UEQ_Attractiveness <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Attractiveness, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Attractiveness) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Attractiveness <- leveneTest(UEQ_Attractiveness ~ Exp * HMI, data = data_23)
tab_levene_UEQ_Attractiveness <- data.frame(matrix(unlist(levene_UEQ_Attractiveness), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Attractiveness) <- c_lev
# shapiro (normality)
anova_UEQ_Attractiveness_res <- residuals(object = anova_UEQ_Attractiveness)
shapiro_UEQ_Attractiveness <- shapiro.test(x = anova_UEQ_Attractiveness_res)
tab_shapiro_UEQ_Attractiveness <- data.frame(matrix(unlist(shapiro_UEQ_Attractiveness), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Attractiveness) <- c_shap
# create data frame  
Test <- c("UEQ_Attractiveness 23")
tab_aov_UEQ_Attractiveness <- cbind(Test, tab_aov_UEQ_Attractiveness, tab_levene_UEQ_Attractiveness, tab_shapiro_UEQ_Attractiveness)

#### anova UEQ_Perspicuity 23 ####
# anova
anova_UEQ_Perspicuity <- aov(UEQ_Perspicuity ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UEQ_Perspicuity <- Anova(anova_UEQ_Perspicuity, type="III")
tab_aov_UEQ_Perspicuity <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Perspicuity, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Perspicuity) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Perspicuity <- leveneTest(UEQ_Perspicuity ~ Exp * HMI, data = data_23)
tab_levene_UEQ_Perspicuity <- data.frame(matrix(unlist(levene_UEQ_Perspicuity), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Perspicuity) <- c_lev
# shapiro (normality)
anova_UEQ_Perspicuity_res <- residuals(object = anova_UEQ_Perspicuity)
shapiro_UEQ_Perspicuity <- shapiro.test(x = anova_UEQ_Perspicuity_res)
tab_shapiro_UEQ_Perspicuity <- data.frame(matrix(unlist(shapiro_UEQ_Perspicuity), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Perspicuity) <- c_shap
# create data frame  
Test <- c("UEQ_Perspicuity 23")
tab_aov_UEQ_Perspicuity <- cbind(Test, tab_aov_UEQ_Perspicuity, tab_levene_UEQ_Perspicuity, tab_shapiro_UEQ_Perspicuity)

#### anova UEQ_Efficiency 23 ####
# anova
anova_UEQ_Efficiency <- aov(UEQ_Efficiency ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UEQ_Efficiency <- Anova(anova_UEQ_Efficiency, type="III")
tab_aov_UEQ_Efficiency <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Efficiency, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Efficiency) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Efficiency <- leveneTest(UEQ_Efficiency ~ Exp * HMI, data = data_23)
tab_levene_UEQ_Efficiency <- data.frame(matrix(unlist(levene_UEQ_Efficiency), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Efficiency) <- c_lev
# shapiro (normality)
anova_UEQ_Efficiency_res <- residuals(object = anova_UEQ_Efficiency)
shapiro_UEQ_Efficiency <- shapiro.test(x = anova_UEQ_Efficiency_res)
tab_shapiro_UEQ_Efficiency <- data.frame(matrix(unlist(shapiro_UEQ_Efficiency), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Efficiency) <- c_shap
# create data frame  
Test <- c("UEQ_Efficiency 23")
tab_aov_UEQ_Efficiency <- cbind(Test, tab_aov_UEQ_Efficiency, tab_levene_UEQ_Efficiency, tab_shapiro_UEQ_Efficiency)

#### anova UEQ_Dependability 23 ####
# anova
anova_UEQ_Dependability <- aov(UEQ_Dependability ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UEQ_Dependability <- Anova(anova_UEQ_Dependability, type="III")
tab_aov_UEQ_Dependability <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Dependability, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Dependability) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Dependability <- leveneTest(UEQ_Dependability ~ Exp * HMI, data = data_23)
tab_levene_UEQ_Dependability <- data.frame(matrix(unlist(levene_UEQ_Dependability), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Dependability) <- c_lev
# shapiro (normality)
anova_UEQ_Dependability_res <- residuals(object = anova_UEQ_Dependability)
shapiro_UEQ_Dependability <- shapiro.test(x = anova_UEQ_Dependability_res)
tab_shapiro_UEQ_Dependability <- data.frame(matrix(unlist(shapiro_UEQ_Dependability), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Dependability) <- c_shap
# create data frame  
Test <- c("UEQ_Dependability 23")
tab_aov_UEQ_Dependability <- cbind(Test, tab_aov_UEQ_Dependability, tab_levene_UEQ_Dependability, tab_shapiro_UEQ_Dependability)

#### anova UEQ_Novelty 23 ####
# anova
anova_UEQ_Novelty <- aov(UEQ_Novelty ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UEQ_Novelty <- Anova(anova_UEQ_Novelty, type="III")
tab_aov_UEQ_Novelty <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Novelty, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Novelty) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Novelty <- leveneTest(UEQ_Novelty ~ Exp * HMI, data = data_23)
tab_levene_UEQ_Novelty <- data.frame(matrix(unlist(levene_UEQ_Novelty), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Novelty) <- c_lev
# shapiro (normality)
anova_UEQ_Novelty_res <- residuals(object = anova_UEQ_Novelty)
shapiro_UEQ_Novelty <- shapiro.test(x = anova_UEQ_Novelty_res)
tab_shapiro_UEQ_Novelty <- data.frame(matrix(unlist(shapiro_UEQ_Novelty), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Novelty) <- c_shap
# create data frame  
Test <- c("UEQ_Novelty 23")
tab_aov_UEQ_Novelty <- cbind(Test, tab_aov_UEQ_Novelty, tab_levene_UEQ_Novelty, tab_shapiro_UEQ_Novelty)

#### anova UEQ_Stimulation 23 ####
# anova
anova_UEQ_Stimulation <- aov(UEQ_Stimulation ~ Exp * HMI, data = data_23, contrasts = "contr.sum")
sum_anova_UEQ_Stimulation <- Anova(anova_UEQ_Stimulation, type="III")
tab_aov_UEQ_Stimulation <- data.frame(matrix(unlist(EtaSq(anova_UEQ_Stimulation, type = 3, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_UEQ_Stimulation) <- c_aov_t2
# levene (homogeneity)
levene_UEQ_Stimulation <- leveneTest(UEQ_Stimulation ~ Exp * HMI, data = data_23)
tab_levene_UEQ_Stimulation <- data.frame(matrix(unlist(levene_UEQ_Stimulation), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_UEQ_Stimulation) <- c_lev
# shapiro (normality)
anova_UEQ_Stimulation_res <- residuals(object = anova_UEQ_Stimulation)
shapiro_UEQ_Stimulation <- shapiro.test(x = anova_UEQ_Stimulation_res)
tab_shapiro_UEQ_Stimulation <- data.frame(matrix(unlist(shapiro_UEQ_Stimulation), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_UEQ_Stimulation) <- c_shap
# create data frame  
Test <- c("UEQ_Stimulation 23")
tab_aov_UEQ_Stimulation <- cbind(Test, tab_aov_UEQ_Stimulation, tab_levene_UEQ_Stimulation, tab_shapiro_UEQ_Stimulation)

#### gather anova 23 results #### 
# combine data frames of anovas 
UsabQ_anova_23 <- bind_rows(tab_aov_SUS, tab_aov_UMUX, 
                            tab_aov_Trust, tab_aov_Acceptance,
                            tab_aov_UEQ_Attractiveness, tab_aov_UEQ_Perspicuity, tab_aov_UEQ_Efficiency, 
                            tab_aov_UEQ_Dependability, tab_aov_UEQ_Novelty, tab_aov_UEQ_Stimulation)

#### combine anova 12 and anova 23 results ####
UsabQ_anova <- bind_rows(UsabQ_anova_12, UsabQ_anova_23)

################################## ________ save ________ ########################################
write_excel_csv(UsabQ_anova, "data/results/ANOVA_UsabQ.csv")
rm(list=setdiff(ls(), c("UsabQ_anova", "data_Q", "data_12", "data_23")))