#### script for anova testing of usability questionnaires ####
# builds on script 02_all_UsabQ_scores.R
# https://statsandr.com/blog/anova-in-r/#introduction

#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
setwd("~/R/Multilab")

#### load dataset ####
load("data/processed/R_data_all_Q.RData")


#### data sets ####
# data_12 is for comparison sim (GER) vs test track (GER)
# data_23 is for comparison test track (GER) vs test track (USA)

data_12 <- data_Q %>%
  filter(Exp == '1' | Exp == '2')

data_23 <- data_Q %>%
  filter(Exp == '2' | Exp == '3')

#### descriptive results ####
by(data_12$SUS_score, list(data_12$HMI, data_12$Exp), stat.desc)


#### anova ####
# steps
# 2) establish procedure (requirements testing + anova; export all in one row)
# 3) build dataframe with title and data with according columns

# data frame column names
c_aov_nI <- c("Df_Exp", "Df_HMI", "Df_Residuals", "SumSq_Exp", "SumSq_HMI", "SumSq_Residuals", "MeanSq_Exp", "MeanSq_HMI", "MeanSq_Residuals", "F_Exp", "F_HMI", "F_Residuals", "p_Exp", "p_HMI", "p_Residuals")
c_aov_I <- c("Df_Exp", "Df_HMI", "Df_Interaction", "Df_Residuals", "SumSq_Exp", "SumSq_HMI", "SumSq_Interaction", "SumSq_Residuals", "MeanSq_Exp", "MeanSq_HMI", "MeanSq_Interaction", "MeanSq_Residuals", "F_Exp", "F_HMI", "F_Interaction", "F_Residuals", "p_Exp", "p_HMI", "p_Interaction", "p_Residuals")
c_lev <- c("lev_Df_group", "lev_Df", "lev_F", "lev_x1", "lev_p", "lev_x2")
c_shap <- c("shap_W", "shap_p" , "method", "data")

# used for type II (-> aov2) anovas; here default, since main effects are focus of research
c_aov2_nI <- c("SumSq_Exp", "SumSq_HMI", "SumSq_Residuals", "Df_Exp", "Df_HMI", "Df_Residuals", "F_Exp", "F_HMI", "F_Residuals", "p_Exp", "p_HMI", "p_Residuals")
c_aov2_I <- c("SumSq_Exp", "SumSq_HMI", "SumSq_Interaction", "SumSq_Residuals", "Df_Exp", "Df_HMI", "Df_Interaction", "Df_Residuals", "F_Exp", "F_HMI", "F_Interaction", "F_Residuals", "p_Exp", "p_HMI", "p_Interaction", "p_Residuals")


data <- data_12 %>%
  add_column(var_1 = .$SUS_score) %>%
  add_column(var_2 = .$UMUX_score) 

var_1 <- data$SUS_score
var_2 <- data$UMUX_score

b <- data$Alter
summary(b)

for (i in 1:2)
  assign(paste0("var_", i), b)
  print(summary(b))
  
# anova wihout interaction effects
anova_1_tmp <- aov(var_1 ~ Exp + HMI, data = data)
sum_anova_1_tmp <- Anova(anova_1_tmp, type="II")
# levene (homogeneity)
# plot(anova_2_tmp, 1)
levene_tmp <- leveneTest(var_1 ~ Exp * HMI, data = data)
sum_levene_tmp <- data.frame(matrix(unlist(levene_tmp), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_levene_tmp) <- c_lev
# shapiro (normality)
# plot(anova_1_tmp, 2)
anova_1_tmp_residuals <- residuals(object = anova_1_tmp)
shapiro_tmp_1 <- shapiro.test(x = anova_1_tmp_residuals)
sum_shapiro_tmp_1 <- data.frame(matrix(unlist(shapiro_tmp_1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_shapiro_tmp_1) <- c_shap
# create data frame  
sum_aov_1_tmp <- data.frame(matrix(unlist(sum_anova_1_tmp), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_aov_1_tmp) <- c_aov2_nI
Test <- c("var_1")
sum_aov_1_tmp <- cbind(Test, sum_aov_1_tmp, sum_levene_tmp, sum_shapiro_tmp_1)


# anova interaction effects
# anova
anova_2_tmp <- aov(var_1 ~ Exp * HMI, data = data)
sum_anova_2_tmp <- Anova(anova_2_tmp, type="II")
# levene (homogeneity) - same as without interaction
# shapiro (normality)
# plot(anova_2_tmp, 2)
anova_2_tmp_residuals <- residuals(object = anova_2_tmp)
shapiro_tmp_2 <- shapiro.test(x = anova_2_tmp_residuals)
sum_shapiro_tmp_2 <- data.frame(matrix(unlist(shapiro_tmp_2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_shapiro_tmp_2) <- c_shap

# create data frame
sum_aov_2_tmp <- data.frame(matrix(unlist(sum_anova_2_tmp), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_aov_2_tmp) <- c_aov2_I
Test <- c("var_1")
sum_aov_2_tmp <- cbind(Test, sum_aov_2_tmp, sum_levene_tmp, sum_shapiro_tmp_2)

# combine data frames
results <- bind_rows(sum_aov_2_tmp, sum_aov_1_tmp)




























# anova wihout interaction effects
anova_1_tmp <- aov(var_2 ~ Exp + HMI, data = data)
sum_anova_1_tmp <- Anova(anova_1_tmp, type="II")
# levene (homogeneity)
# plot(anova_2_tmp, 1)
levene_tmp <- leveneTest(var_2 ~ Exp * HMI, data = data)
sum_levene_tmp <- data.frame(matrix(unlist(levene_tmp), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_levene_tmp) <- c_lev
# shapiro (normality)
# plot(anova_1_tmp, 2)
anova_1_tmp_residuals <- residuals(object = anova_1_tmp)
shapiro_tmp_1 <- shapiro.test(x = anova_1_tmp_residuals)
sum_shapiro_tmp_1 <- data.frame(matrix(unlist(shapiro_tmp_1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_shapiro_tmp_1) <- c_shap
# create data frame  
sum_aov_1_tmp <- data.frame(matrix(unlist(sum_anova_1_tmp), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_aov_1_tmp) <- c_aov2_nI
Test <- c("var_2")
sum_aov_1_tmp <- cbind(Test, sum_aov_1_tmp, sum_levene_tmp, sum_shapiro_tmp_1)


# anova interaction effects
# anova
anova_2_tmp <- aov(var_2 ~ Exp * HMI, data = data)
sum_anova_2_tmp <- Anova(anova_2_tmp, type="II")
# levene (homogeneity) - same as without interaction
# shapiro (normality)
# plot(anova_2_tmp, 2)
anova_2_tmp_residuals <- residuals(object = anova_2_tmp)
shapiro_tmp_2 <- shapiro.test(x = anova_2_tmp_residuals)
sum_shapiro_tmp_2 <- data.frame(matrix(unlist(shapiro_tmp_2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_shapiro_tmp_2) <- c_shap

# create data frame
sum_aov_2_tmp <- data.frame(matrix(unlist(sum_anova_2_tmp), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_aov_2_tmp) <- c_aov2_I
Test <- c("var_2")
sum_aov_2_tmp <- cbind(Test, sum_aov_2_tmp, sum_levene_tmp, sum_shapiro_tmp_2)

# combine data frames
results <- bind_rows(results, sum_aov_2_tmp, sum_aov_1_tmp)

