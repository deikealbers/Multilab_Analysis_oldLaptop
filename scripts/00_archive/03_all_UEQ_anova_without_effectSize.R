#### script for anova testing of UEQ scores ####
# builds on script 02_all_UsabQ_scores.R
# https://statsandr.com/blog/anova-in-r/#introduction
    # UEQ_Attractiveness
    # UEQ_Perspicuity
    # UEQ_Efficiency
    # UEQ_Dependability
    # UEQ_Novelty
    # UEQ_Stimulation

#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
library(compute.es); 
library(ggplot2); 
library(multcomp);
library(pastecs); 
library(reshape); 
library(WRS);
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


#### functions ####
fun_mean <- function(x){ return(data.frame(y=mean(x),label=round(mean(x,na.rm=T), 2)))}

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
c_aov_t2 <- c("SumSq_Exp", "SumSq_HMI", "SumSq_Interaction", "SumSq_Residuals", "Df_Exp", "Df_HMI", "Df_Interaction", 
              "Df_Residuals", "F_Exp", "F_HMI", "F_Interaction", "F_Residuals", "p_Exp", "p_HMI", "p_Interaction", 
              "p_Residuals") # used for type II (-> aov_t2) anovas; here default, since main effects are focus of research

#### anova UEQ_Attractiveness 12 ####
# anova
anova_UEQ_Attractiveness <- aov(UEQ_Attractiveness ~ Exp * HMI, data = data_12)
sum_anova_UEQ_Attractiveness <- Anova(anova_UEQ_Attractiveness, type="II")
tab_aov_UEQ_Attractiveness <- data.frame(matrix(unlist(sum_anova_UEQ_Attractiveness), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Perspicuity <- aov(UEQ_Perspicuity ~ Exp * HMI, data = data_12)
sum_anova_UEQ_Perspicuity <- Anova(anova_UEQ_Perspicuity, type="II")
tab_aov_UEQ_Perspicuity <- data.frame(matrix(unlist(sum_anova_UEQ_Perspicuity), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Efficiency <- aov(UEQ_Efficiency ~ Exp * HMI, data = data_12)
sum_anova_UEQ_Efficiency <- Anova(anova_UEQ_Efficiency, type="II")
tab_aov_UEQ_Efficiency <- data.frame(matrix(unlist(sum_anova_UEQ_Efficiency), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Dependability <- aov(UEQ_Dependability ~ Exp * HMI, data = data_12)
sum_anova_UEQ_Dependability <- Anova(anova_UEQ_Dependability, type="II")
tab_aov_UEQ_Dependability <- data.frame(matrix(unlist(sum_anova_UEQ_Dependability), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Novelty <- aov(UEQ_Novelty ~ Exp * HMI, data = data_12)
sum_anova_UEQ_Novelty <- Anova(anova_UEQ_Novelty, type="II")
tab_aov_UEQ_Novelty <- data.frame(matrix(unlist(sum_anova_UEQ_Novelty), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Stimulation <- aov(UEQ_Stimulation ~ Exp * HMI, data = data_12)
sum_anova_UEQ_Stimulation <- Anova(anova_UEQ_Stimulation, type="II")
tab_aov_UEQ_Stimulation <- data.frame(matrix(unlist(sum_anova_UEQ_Stimulation), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
UEQ_anova_12 <- bind_rows(tab_aov_UEQ_Attractiveness, tab_aov_UEQ_Perspicuity, tab_aov_UEQ_Efficiency, tab_aov_UEQ_Dependability, tab_aov_UEQ_Novelty, tab_aov_UEQ_Stimulation)

#### anova UEQ_Attractiveness 23 ####
# anova
anova_UEQ_Attractiveness <- aov(UEQ_Attractiveness ~ Exp * HMI, data = data_23)
sum_anova_UEQ_Attractiveness <- Anova(anova_UEQ_Attractiveness, type="II")
tab_aov_UEQ_Attractiveness <- data.frame(matrix(unlist(sum_anova_UEQ_Attractiveness), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Perspicuity <- aov(UEQ_Perspicuity ~ Exp * HMI, data = data_23)
sum_anova_UEQ_Perspicuity <- Anova(anova_UEQ_Perspicuity, type="II")
tab_aov_UEQ_Perspicuity <- data.frame(matrix(unlist(sum_anova_UEQ_Perspicuity), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Efficiency <- aov(UEQ_Efficiency ~ Exp * HMI, data = data_23)
sum_anova_UEQ_Efficiency <- Anova(anova_UEQ_Efficiency, type="II")
tab_aov_UEQ_Efficiency <- data.frame(matrix(unlist(sum_anova_UEQ_Efficiency), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Dependability <- aov(UEQ_Dependability ~ Exp * HMI, data = data_23)
sum_anova_UEQ_Dependability <- Anova(anova_UEQ_Dependability, type="II")
tab_aov_UEQ_Dependability <- data.frame(matrix(unlist(sum_anova_UEQ_Dependability), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Novelty <- aov(UEQ_Novelty ~ Exp * HMI, data = data_23)
sum_anova_UEQ_Novelty <- Anova(anova_UEQ_Novelty, type="II")
tab_aov_UEQ_Novelty <- data.frame(matrix(unlist(sum_anova_UEQ_Novelty), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
anova_UEQ_Stimulation <- aov(UEQ_Stimulation ~ Exp * HMI, data = data_23)
sum_anova_UEQ_Stimulation <- Anova(anova_UEQ_Stimulation, type="II")
tab_aov_UEQ_Stimulation <- data.frame(matrix(unlist(sum_anova_UEQ_Stimulation), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
UEQ_anova_23 <- bind_rows(tab_aov_UEQ_Attractiveness, tab_aov_UEQ_Perspicuity, tab_aov_UEQ_Efficiency, tab_aov_UEQ_Dependability, tab_aov_UEQ_Novelty, tab_aov_UEQ_Stimulation)

#### combine anova 12 and anova 23 results ####
UEQ_anova <- bind_rows(UEQ_anova_12, UEQ_anova_23)

#### save data ####
write_excel_csv(UEQ_anova, "data/processed/anova_UEQ.csv")
rm(list=setdiff(ls(), c("UEQ_anova", "data_Q", "data_12", "data_23", "fun_mean")))