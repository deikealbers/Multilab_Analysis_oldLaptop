#### script for the short interviews ####
# builds on script 04_all_shortInterviews.R
# includes levels of automation & comparisons of 
    # observed level vs instructed        --> LevelObserved_Rep_score          ### actually belongs to driving behavior
    # observed level vs reported          --> LevelObserved_Instr_score
    # Allocation of driving Task          --> BothAllow_Observed_score (analyses added for E-mail and HandsOff, only both should be reported though)
    # # of reported Transition problems   --> TransProblems_score
    # correct notice availability change  --> AvailImplem_Rep_score

#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
library(compute.es); 
library(ggplot2); 
library(multcomp);
library(pastecs); 
library(reshape); 
library(dplyr)
# library(WRS);
library(DescTools);
setwd("~/R/Multilab")

#### procedure ####
# 1) Test for requirements
#     - DV is a metric variable
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

load("data/processed/data_all_scores-shortInterviews.RData")

data_12 <- data_scores %>%
  filter(Exp == '1' | Exp == '2') %>%
  dplyr::select(c(Exp, HMI, VPNr, LevelObserved_Rep_score, LevelObserved_Instr_score, EmailsAllow_Observed_score, 
           HandsOffAllow_Observed_score, BothAllow_Observed_score, TransProblems_score, AvailImplem_Rep_score))

data_23 <- data_scores %>%
  filter(Exp == '2' | Exp == '3') %>%
  dplyr::select(c(Exp, HMI, VPNr, LevelObserved_Rep_score, LevelObserved_Instr_score, EmailsAllow_Observed_score, 
           HandsOffAllow_Observed_score, BothAllow_Observed_score, TransProblems_score, AvailImplem_Rep_score))

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

#### anova LevelObserved_Rep 12 ####
# anova
anova_LevelObserved_Rep <- aov(LevelObserved_Rep_score ~ Exp * HMI, data = data_12)
sum_anova_LevelObserved_Rep <- Anova(anova_LevelObserved_Rep, type="II")
tab_aov_LevelObserved_Rep <- data.frame(matrix(unlist(EtaSq(anova_LevelObserved_Rep, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_LevelObserved_Rep) <- c_aov_t2
# levene (homogeneity)
levene_LevelObserved_Rep <- leveneTest(LevelObserved_Rep_score ~ Exp * HMI, data = data_12)
tab_levene_LevelObserved_Rep <- data.frame(matrix(unlist(levene_LevelObserved_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_LevelObserved_Rep) <- c_lev
# shapiro (normality)
anova_LevelObserved_Rep_res <- residuals(object = anova_LevelObserved_Rep)
shapiro_LevelObserved_Rep <- shapiro.test(x = anova_LevelObserved_Rep_res)
tab_shapiro_LevelObserved_Rep <- data.frame(matrix(unlist(shapiro_LevelObserved_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_LevelObserved_Rep) <- c_shap
# create data frame  
Test <- c("LevelObserved_Rep 12")
tab_aov_LevelObserved_Rep <- cbind(Test, tab_aov_LevelObserved_Rep, tab_levene_LevelObserved_Rep, tab_shapiro_LevelObserved_Rep)

#### anova LevelObserved_Instr 12 ####
# anova
anova_LevelObserved_Instr <- aov(LevelObserved_Instr_score ~ Exp * HMI, data = data_12)
sum_anova_LevelObserved_Instr <- Anova(anova_LevelObserved_Instr, type="II")
tab_aov_LevelObserved_Instr <- data.frame(matrix(unlist(EtaSq(anova_LevelObserved_Instr, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_LevelObserved_Instr) <- c_aov_t2
# levene (homogeneity)
levene_LevelObserved_Instr <- leveneTest(LevelObserved_Instr_score ~ Exp * HMI, data = data_12)
tab_levene_LevelObserved_Instr <- data.frame(matrix(unlist(levene_LevelObserved_Instr), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_LevelObserved_Instr) <- c_lev
# shapiro (normality)
anova_LevelObserved_Instr_res <- residuals(object = anova_LevelObserved_Instr)
shapiro_LevelObserved_Instr <- shapiro.test(x = anova_LevelObserved_Instr_res)
tab_shapiro_LevelObserved_Instr <- data.frame(matrix(unlist(shapiro_LevelObserved_Instr), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_LevelObserved_Instr) <- c_shap
# create data frame  
Test <- c("LevelObserved_Instr 12")
tab_aov_LevelObserved_Instr <- cbind(Test, tab_aov_LevelObserved_Instr, tab_levene_LevelObserved_Instr, tab_shapiro_LevelObserved_Instr)

#### anova BothAllow_Observed 12 ####
# anova
anova_BothAllow_Observed <- aov(BothAllow_Observed_score ~ Exp * HMI, data = data_12)
sum_anova_BothAllow_Observed <- Anova(anova_BothAllow_Observed, type="II")
tab_aov_BothAllow_Observed <- data.frame(matrix(unlist(EtaSq(anova_BothAllow_Observed, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_BothAllow_Observed) <- c_aov_t2
# levene (homogeneity)
levene_BothAllow_Observed <- leveneTest(BothAllow_Observed_score ~ Exp * HMI, data = data_12)
tab_levene_BothAllow_Observed <- data.frame(matrix(unlist(levene_BothAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_BothAllow_Observed) <- c_lev
# shapiro (normality)
anova_BothAllow_Observed_res <- residuals(object = anova_BothAllow_Observed)
shapiro_BothAllow_Observed <- shapiro.test(x = anova_BothAllow_Observed_res)
tab_shapiro_BothAllow_Observed <- data.frame(matrix(unlist(shapiro_BothAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_BothAllow_Observed) <- c_shap
# create data frame  
Test <- c("BothAllow_Observed 12")
tab_aov_BothAllow_Observed <- cbind(Test, tab_aov_BothAllow_Observed, tab_levene_BothAllow_Observed, tab_shapiro_BothAllow_Observed)

#### anova EmailsAllow_Observed 12 ####
# anova
anova_EmailsAllow_Observed <- aov(EmailsAllow_Observed_score ~ Exp * HMI, data = data_12)
sum_anova_EmailsAllow_Observed <- Anova(anova_EmailsAllow_Observed, type="II")
tab_aov_EmailsAllow_Observed <- data.frame(matrix(unlist(EtaSq(anova_EmailsAllow_Observed, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_EmailsAllow_Observed) <- c_aov_t2
# levene (homogeneity)
levene_EmailsAllow_Observed <- leveneTest(EmailsAllow_Observed_score ~ Exp * HMI, data = data_12)
tab_levene_EmailsAllow_Observed <- data.frame(matrix(unlist(levene_EmailsAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_EmailsAllow_Observed) <- c_lev
# shapiro (normality)
anova_EmailsAllow_Observed_res <- residuals(object = anova_EmailsAllow_Observed)
shapiro_EmailsAllow_Observed <- shapiro.test(x = anova_EmailsAllow_Observed_res)
tab_shapiro_EmailsAllow_Observed <- data.frame(matrix(unlist(shapiro_EmailsAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_EmailsAllow_Observed) <- c_shap
# create data frame  
Test <- c("EmailsAllow_Observed 12")
tab_aov_EmailsAllow_Observed <- cbind(Test, tab_aov_EmailsAllow_Observed, tab_levene_EmailsAllow_Observed, tab_shapiro_EmailsAllow_Observed)

#### anova HandsOffAllow_Observed 12 ####
# anova
anova_HandsOffAllow_Observed <- aov(HandsOffAllow_Observed_score ~ Exp * HMI, data = data_12)
sum_anova_HandsOffAllow_Observed <- Anova(anova_HandsOffAllow_Observed, type="II")
tab_aov_HandsOffAllow_Observed <- data.frame(matrix(unlist(EtaSq(anova_HandsOffAllow_Observed, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_HandsOffAllow_Observed) <- c_aov_t2
# levene (homogeneity)
levene_HandsOffAllow_Observed <- leveneTest(HandsOffAllow_Observed_score ~ Exp * HMI, data = data_12)
tab_levene_HandsOffAllow_Observed <- data.frame(matrix(unlist(levene_HandsOffAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_HandsOffAllow_Observed) <- c_lev
# shapiro (normality)
anova_HandsOffAllow_Observed_res <- residuals(object = anova_HandsOffAllow_Observed)
shapiro_HandsOffAllow_Observed <- shapiro.test(x = anova_HandsOffAllow_Observed_res)
tab_shapiro_HandsOffAllow_Observed <- data.frame(matrix(unlist(shapiro_HandsOffAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_HandsOffAllow_Observed) <- c_shap
# create data frame  
Test <- c("HandsOffAllow_Observed 12")
tab_aov_HandsOffAllow_Observed <- cbind(Test, tab_aov_HandsOffAllow_Observed, tab_levene_HandsOffAllow_Observed, tab_shapiro_HandsOffAllow_Observed)

#### anova TransProblems 12 ####
# anova
anova_TransProblems <- aov(TransProblems_score ~ Exp * HMI, data = data_12)
sum_anova_TransProblems <- Anova(anova_TransProblems, type="II")
tab_aov_TransProblems <- data.frame(matrix(unlist(EtaSq(anova_TransProblems, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_TransProblems) <- c_aov_t2
# levene (homogeneity)
levene_TransProblems <- leveneTest(TransProblems_score ~ Exp * HMI, data = data_12)
tab_levene_TransProblems <- data.frame(matrix(unlist(levene_TransProblems), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_TransProblems) <- c_lev
# shapiro (normality)
anova_TransProblems_res <- residuals(object = anova_TransProblems)
shapiro_TransProblems <- shapiro.test(x = anova_TransProblems_res)
tab_shapiro_TransProblems <- data.frame(matrix(unlist(shapiro_TransProblems), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_TransProblems) <- c_shap
# create data frame  
Test <- c("TransProblems 12")
tab_aov_TransProblems <- cbind(Test, tab_aov_TransProblems, tab_levene_TransProblems, tab_shapiro_TransProblems)

#### anova AvailImplem_Rep 12 ####
# anova
anova_AvailImplem_Rep <- aov(AvailImplem_Rep_score ~ Exp * HMI, data = data_12)
sum_anova_AvailImplem_Rep <- Anova(anova_AvailImplem_Rep, type="II")
tab_aov_AvailImplem_Rep <- data.frame(matrix(unlist(EtaSq(anova_AvailImplem_Rep, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_AvailImplem_Rep) <- c_aov_t2
# levene (homogeneity)
levene_AvailImplem_Rep <- leveneTest(AvailImplem_Rep_score ~ Exp * HMI, data = data_12)
tab_levene_AvailImplem_Rep <- data.frame(matrix(unlist(levene_AvailImplem_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_AvailImplem_Rep) <- c_lev
# shapiro (normality)
anova_AvailImplem_Rep_res <- residuals(object = anova_AvailImplem_Rep)
shapiro_AvailImplem_Rep <- shapiro.test(x = anova_AvailImplem_Rep_res)
tab_shapiro_AvailImplem_Rep <- data.frame(matrix(unlist(shapiro_AvailImplem_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_AvailImplem_Rep) <- c_shap
# create data frame  
Test <- c("AvailImplem_Rep 12")
tab_aov_AvailImplem_Rep <- cbind(Test, tab_aov_AvailImplem_Rep, tab_levene_AvailImplem_Rep, tab_shapiro_AvailImplem_Rep)



#### gather anova 12 results #### 
# combine data frames of anovas 
shortInterviews_anova_12 <- bind_rows(tab_aov_LevelObserved_Rep, tab_aov_LevelObserved_Instr, tab_aov_BothAllow_Observed, tab_aov_EmailsAllow_Observed, 
                                      tab_aov_HandsOffAllow_Observed, tab_aov_TransProblems, tab_aov_AvailImplem_Rep)



#### anova LevelObserved_Rep 23 ####
# anova
anova_LevelObserved_Rep <- aov(LevelObserved_Rep_score ~ Exp * HMI, data = data_23)
sum_anova_LevelObserved_Rep <- Anova(anova_LevelObserved_Rep, type="II")
tab_aov_LevelObserved_Rep <- data.frame(matrix(unlist(EtaSq(anova_LevelObserved_Rep, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_LevelObserved_Rep) <- c_aov_t2
# levene (homogeneity)
levene_LevelObserved_Rep <- leveneTest(LevelObserved_Rep_score ~ Exp * HMI, data = data_23)
tab_levene_LevelObserved_Rep <- data.frame(matrix(unlist(levene_LevelObserved_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_LevelObserved_Rep) <- c_lev
# shapiro (normality)
anova_LevelObserved_Rep_res <- residuals(object = anova_LevelObserved_Rep)
shapiro_LevelObserved_Rep <- shapiro.test(x = anova_LevelObserved_Rep_res)
tab_shapiro_LevelObserved_Rep <- data.frame(matrix(unlist(shapiro_LevelObserved_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_LevelObserved_Rep) <- c_shap
# create data frame  
Test <- c("LevelObserved_Rep 23")
tab_aov_LevelObserved_Rep <- cbind(Test, tab_aov_LevelObserved_Rep, tab_levene_LevelObserved_Rep, tab_shapiro_LevelObserved_Rep)

#### anova LevelObserved_Instr 23 ####
# anova
anova_LevelObserved_Instr <- aov(LevelObserved_Instr_score ~ Exp * HMI, data = data_23)
sum_anova_LevelObserved_Instr <- Anova(anova_LevelObserved_Instr, type="II")
tab_aov_LevelObserved_Instr <- data.frame(matrix(unlist(EtaSq(anova_LevelObserved_Instr, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_LevelObserved_Instr) <- c_aov_t2
# levene (homogeneity)
levene_LevelObserved_Instr <- leveneTest(LevelObserved_Instr_score ~ Exp * HMI, data = data_23)
tab_levene_LevelObserved_Instr <- data.frame(matrix(unlist(levene_LevelObserved_Instr), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_LevelObserved_Instr) <- c_lev
# shapiro (normality)
anova_LevelObserved_Instr_res <- residuals(object = anova_LevelObserved_Instr)
shapiro_LevelObserved_Instr <- shapiro.test(x = anova_LevelObserved_Instr_res)
tab_shapiro_LevelObserved_Instr <- data.frame(matrix(unlist(shapiro_LevelObserved_Instr), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_LevelObserved_Instr) <- c_shap
# create data frame  
Test <- c("LevelObserved_Instr 23")
tab_aov_LevelObserved_Instr <- cbind(Test, tab_aov_LevelObserved_Instr, tab_levene_LevelObserved_Instr, tab_shapiro_LevelObserved_Instr)

#### anova BothAllow_Observed 23 ####
# anova
anova_BothAllow_Observed <- aov(BothAllow_Observed_score ~ Exp * HMI, data = data_23)
sum_anova_BothAllow_Observed <- Anova(anova_BothAllow_Observed, type="II")
tab_aov_BothAllow_Observed <- data.frame(matrix(unlist(EtaSq(anova_BothAllow_Observed, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_BothAllow_Observed) <- c_aov_t2
# levene (homogeneity)
levene_BothAllow_Observed <- leveneTest(BothAllow_Observed_score ~ Exp * HMI, data = data_23)
tab_levene_BothAllow_Observed <- data.frame(matrix(unlist(levene_BothAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_BothAllow_Observed) <- c_lev
# shapiro (normality)
anova_BothAllow_Observed_res <- residuals(object = anova_BothAllow_Observed)
shapiro_BothAllow_Observed <- shapiro.test(x = anova_BothAllow_Observed_res)
tab_shapiro_BothAllow_Observed <- data.frame(matrix(unlist(shapiro_BothAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_BothAllow_Observed) <- c_shap
# create data frame  
Test <- c("BothAllow_Observed 23")
tab_aov_BothAllow_Observed <- cbind(Test, tab_aov_BothAllow_Observed, tab_levene_BothAllow_Observed, tab_shapiro_BothAllow_Observed)

#### anova EmailsAllow_Observed 23 ####
# anova
anova_EmailsAllow_Observed <- aov(EmailsAllow_Observed_score ~ Exp * HMI, data = data_23)
sum_anova_EmailsAllow_Observed <- Anova(anova_EmailsAllow_Observed, type="II")
tab_aov_EmailsAllow_Observed <- data.frame(matrix(unlist(EtaSq(anova_EmailsAllow_Observed, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_EmailsAllow_Observed) <- c_aov_t2
# levene (homogeneity)
levene_EmailsAllow_Observed <- leveneTest(EmailsAllow_Observed_score ~ Exp * HMI, data = data_23)
tab_levene_EmailsAllow_Observed <- data.frame(matrix(unlist(levene_EmailsAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_EmailsAllow_Observed) <- c_lev
# shapiro (normality)
anova_EmailsAllow_Observed_res <- residuals(object = anova_EmailsAllow_Observed)
shapiro_EmailsAllow_Observed <- shapiro.test(x = anova_EmailsAllow_Observed_res)
tab_shapiro_EmailsAllow_Observed <- data.frame(matrix(unlist(shapiro_EmailsAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_EmailsAllow_Observed) <- c_shap
# create data frame  
Test <- c("EmailsAllow_Observed 23")
tab_aov_EmailsAllow_Observed <- cbind(Test, tab_aov_EmailsAllow_Observed, tab_levene_EmailsAllow_Observed, tab_shapiro_EmailsAllow_Observed)

#### anova HandsOffAllow_Observed 23 ####
# anova
anova_HandsOffAllow_Observed <- aov(HandsOffAllow_Observed_score ~ Exp * HMI, data = data_23)
sum_anova_HandsOffAllow_Observed <- Anova(anova_HandsOffAllow_Observed, type="II")
tab_aov_HandsOffAllow_Observed <- data.frame(matrix(unlist(EtaSq(anova_HandsOffAllow_Observed, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_HandsOffAllow_Observed) <- c_aov_t2
# levene (homogeneity)
levene_HandsOffAllow_Observed <- leveneTest(HandsOffAllow_Observed_score ~ Exp * HMI, data = data_23)
tab_levene_HandsOffAllow_Observed <- data.frame(matrix(unlist(levene_HandsOffAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_HandsOffAllow_Observed) <- c_lev
# shapiro (normality)
anova_HandsOffAllow_Observed_res <- residuals(object = anova_HandsOffAllow_Observed)
shapiro_HandsOffAllow_Observed <- shapiro.test(x = anova_HandsOffAllow_Observed_res)
tab_shapiro_HandsOffAllow_Observed <- data.frame(matrix(unlist(shapiro_HandsOffAllow_Observed), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_HandsOffAllow_Observed) <- c_shap
# create data frame  
Test <- c("HandsOffAllow_Observed 23")
tab_aov_HandsOffAllow_Observed <- cbind(Test, tab_aov_HandsOffAllow_Observed, tab_levene_HandsOffAllow_Observed, tab_shapiro_HandsOffAllow_Observed)

#### anova TransProblems 23 ####
# anova
anova_TransProblems <- aov(TransProblems_score ~ Exp * HMI, data = data_23)
sum_anova_TransProblems <- Anova(anova_TransProblems, type="II")
tab_aov_TransProblems <- data.frame(matrix(unlist(EtaSq(anova_TransProblems, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_TransProblems) <- c_aov_t2
# levene (homogeneity)
levene_TransProblems <- leveneTest(TransProblems_score ~ Exp * HMI, data = data_23)
tab_levene_TransProblems <- data.frame(matrix(unlist(levene_TransProblems), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_TransProblems) <- c_lev
# shapiro (normality)
anova_TransProblems_res <- residuals(object = anova_TransProblems)
shapiro_TransProblems <- shapiro.test(x = anova_TransProblems_res)
tab_shapiro_TransProblems <- data.frame(matrix(unlist(shapiro_TransProblems), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_TransProblems) <- c_shap
# create data frame  
Test <- c("TransProblems 23")
tab_aov_TransProblems <- cbind(Test, tab_aov_TransProblems, tab_levene_TransProblems, tab_shapiro_TransProblems)

#### anova AvailImplem_Rep 23 ####
# anova
anova_AvailImplem_Rep <- aov(AvailImplem_Rep_score ~ Exp * HMI, data = data_23)
sum_anova_AvailImplem_Rep <- Anova(anova_AvailImplem_Rep, type="II")
tab_aov_AvailImplem_Rep <- data.frame(matrix(unlist(EtaSq(anova_AvailImplem_Rep, type = 2, anova = TRUE)), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_aov_AvailImplem_Rep) <- c_aov_t2
# levene (homogeneity)
levene_AvailImplem_Rep <- leveneTest(AvailImplem_Rep_score ~ Exp * HMI, data = data_23)
tab_levene_AvailImplem_Rep <- data.frame(matrix(unlist(levene_AvailImplem_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_levene_AvailImplem_Rep) <- c_lev
# shapiro (normality)
anova_AvailImplem_Rep_res <- residuals(object = anova_AvailImplem_Rep)
shapiro_AvailImplem_Rep <- shapiro.test(x = anova_AvailImplem_Rep_res)
tab_shapiro_AvailImplem_Rep <- data.frame(matrix(unlist(shapiro_AvailImplem_Rep), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_shapiro_AvailImplem_Rep) <- c_shap
# create data frame  
Test <- c("AvailImplem_Rep 23")
tab_aov_AvailImplem_Rep <- cbind(Test, tab_aov_AvailImplem_Rep, tab_levene_AvailImplem_Rep, tab_shapiro_AvailImplem_Rep)



#### gather anova 23 results #### 
# combine data frames of anovas 
shortInterviews_anova_23 <- bind_rows(tab_aov_LevelObserved_Rep, tab_aov_LevelObserved_Instr, tab_aov_BothAllow_Observed, tab_aov_EmailsAllow_Observed, 
                                      tab_aov_HandsOffAllow_Observed, tab_aov_TransProblems, tab_aov_AvailImplem_Rep)


#### combine anova 12 and anova 23 results ####
shortInterviews_anova <- bind_rows(shortInterviews_anova_12, shortInterviews_anova_23)


#### save data ####
write_excel_csv(shortInterviews_anova, "data/processed/anova_shortInterviews.csv")
rm(list=setdiff(ls(), c("shortInterviews_anova", "data_scores", "data_12", "data_23")))