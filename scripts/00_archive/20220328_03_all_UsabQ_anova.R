#### script for anova testing of usability questionnaires ####
# builds on script 02_all_UsabQ_scores.R
# https://statsandr.com/blog/anova-in-r/#introduction

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
# library(AICcmodavg)
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

# anova main effects

# levene (homogeneity)
# plot(anova_2_SUS, 1)
levene_SUS <- leveneTest(SUS_score ~ Exp * HMI, data = data_12)
sum_levene_SUS <- data.frame(matrix(unlist(levene_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_levene_SUS) <- c_lev
# shapiro (normality)
# plot(anova_1_SUS, 2)
anova_1_SUS_residuals <- residuals(object = anova_1_SUS)
shapiro_SUS_1 <- shapiro.test(x = anova_1_SUS_residuals)
sum_shapiro_SUS_1 <- data.frame(matrix(unlist(shapiro_SUS_1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_shapiro_SUS_1) <- c_shap
# anova
anova_1_SUS <- aov(SUS_score ~ Exp + HMI, data = data_12)
sum_anova_1_SUS <- Anova(anova_1_SUS, type="II")
# create data frame  
sum_aov_1_SUS <- data.frame(matrix(unlist(sum_anova_1_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_aov_1_SUS) <- c_aov2_nI
Test <- c("1_SUS")
sum_aov_1_SUS <- cbind(Test, sum_aov_1_SUS, sum_levene_SUS, sum_shapiro_SUS_1)


# anova interaction effects
# levene (homogeneity) - same as without interaction
# shapiro (normality)
# plot(anova_2_SUS, 2)
anova_2_SUS_residuals <- residuals(object = anova_2_SUS)
shapiro_SUS_2 <- shapiro.test(x = anova_2_SUS_residuals)
sum_shapiro_SUS_2 <- data.frame(matrix(unlist(shapiro_SUS_2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_shapiro_SUS_2) <- c_shap
# anova
anova_2_SUS <- aov(SUS_score ~ Exp * HMI, data = data_12)
sum_anova_2_SUS <- Anova(anova_2_SUS, type="II")
# create data frame
sum_aov_2_SUS <- data.frame(matrix(unlist(sum_anova_2_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(sum_aov_2_SUS) <- c_aov2_I
Test <- c("2_SUS")
sum_aov_2_SUS <- cbind(Test, sum_aov_2_SUS, sum_levene_SUS, sum_shapiro_SUS_2)

# combine data frames
results <- bind_rows(sum_aov_2_SUS, sum_aov_1_SUS)

# other tests?
# for loop for the variables?









# 
# p <- ggplot(data_Q, aes(x=HMI, y=SUS_score, fill=HMI)) + 
#   geom_boxplot() +
#   theme_bw() +
#   ylim(1, 100) +
#   theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#   facet_wrap(~Exp) + 
#   scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
#   labs(y="Score", x="",
#        title = "SUS Score") +
#   stat_summary(fun = mean, geom="point",colour="black", size=2) +
#   stat_summary(fun.data = fun_mean, geom="text", vjust=1.4)
# p



#### new anova section ####
# define  column names
c_lev <- c("lev_Df_group", "lev_Df", "lev_F", "lev_x1", "lev_p", "lev_x2")
c_shap <- c("shap_W", "shap_p" , "method", "data")
c_aov_t2 <- c("SumSq_Exp", "SumSq_HMI", "SumSq_Interaction", "SumSq_Residuals", "Df_Exp", "Df_HMI", "Df_Interaction", 
              "Df_Residuals", "F_Exp", "F_HMI", "F_Interaction", "F_Residuals", "p_Exp", "p_HMI", "p_Interaction", 
              "p_Residuals") # used for type II (-> aov_t2) anovas; here default, since main effects are focus of research

#### anova SUS ####
# anova
anova_SUS <- aov(SUS_score ~ Exp * HMI, data = data_12)
sum_anova_SUS <- Anova(anova_SUS, type="II")
tab_aov_SUS <- data.frame(matrix(unlist(sum_anova_SUS), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
Test <- c("SUS")
tab_aov_SUS <- cbind(Test, tab_aov_SUS, tab_levene_SUS, tab_shapiro_SUS)

#### anova UMUX ####
# anova
anova_UMUX <- aov(UMUX_score ~ Exp * HMI, data = data_12)
sum_anova_UMUX <- Anova(anova_UMUX, type="II")
tab_aov_UMUX <- data.frame(matrix(unlist(sum_anova_UMUX), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
Test <- c("UMUX")
tab_aov_UMUX <- cbind(Test, tab_aov_UMUX, tab_levene_UMUX, tab_shapiro_UMUX)

#### anova Trust ####
# anova
anova_Trust <- aov(Trust ~ Exp * HMI, data = data_12)
sum_anova_Trust <- Anova(anova_Trust, type="II")
tab_aov_Trust <- data.frame(matrix(unlist(sum_anova_Trust), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
Test <- c("Trust")
tab_aov_Trust <- cbind(Test, tab_aov_Trust, tab_levene_Trust, tab_shapiro_Trust)

#### anova Acceptance ####
# anova
anova_Acceptance <- aov(Acceptance ~ Exp * HMI, data = data_12)
sum_anova_Acceptance <- Anova(anova_Acceptance, type="II")
tab_aov_Acceptance <- data.frame(matrix(unlist(sum_anova_Acceptance), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
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
Test <- c("Acceptance")
tab_aov_Acceptance <- cbind(Test, tab_aov_Acceptance, tab_levene_Acceptance, tab_shapiro_Acceptance)




#### gather anova results #### 
# combine data frames of anovas 
results <- bind_rows(tab_aov_SUS, tab_aov_UMUX, tab_aov_Trust, tab_aov_Acceptance)
