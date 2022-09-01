#### script for anova testing of usability questionnaires ####

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

# https://statsandr.com/blog/anova-in-r/#introduction
# SUS_score
# UMUX_score
# Trust
# Acceptance

#### readme ####
# script is based on article "Cumulative Link Models for Ordinal Regression with the R Package ordinal" by Christensen
# and https://rcompanion.org/handbook/G_11.html

#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
library(ordinal)
library(DescTools)
library(tibble)
# library(compute.es) 
# library(ggplot2)
# library(multcomp)
# library(pastecs)
# library(reshape)
# library(WRS)
# library(DescTools)
# library(afex)
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
# manipulate data sets
data_Q <- data_all %>%
  mutate(Exp = factor(Exp, ordered = TRUE, levels = c("1", "2", "3"))) %>%
  mutate(HMI = factor(HMI, ordered = TRUE, levels = c("LC", "HC"))) %>%
  select(c(Exp, HMI, 
           SUS_score, UMUX_score,
           starts_with("UEQ_"),
           Trust, Acceptance)) %>%
  mutate(SUS_score = factor(SUS_score, ordered = TRUE)) %>%
  mutate(UMUX_score = factor(UMUX_score, ordered = TRUE)) %>%
  mutate(UEQ_Attractiveness = factor(UEQ_Attractiveness, ordered = TRUE)) %>%
  mutate(UEQ_Perspicuity = factor(UEQ_Perspicuity, ordered = TRUE)) %>%
  mutate(UEQ_Efficiency = factor(UEQ_Efficiency, ordered = TRUE)) %>%
  mutate(UEQ_Dependability = factor(UEQ_Dependability, ordered = TRUE)) %>%
  mutate(UEQ_Novelty = factor(UEQ_Novelty, ordered = TRUE)) %>%
  mutate(UEQ_Stimulation = factor(UEQ_Stimulation, ordered = TRUE)) %>%
  mutate(Trust = factor(Trust, ordered = TRUE)) %>%
  mutate(Acceptance = factor(Acceptance, ordered = TRUE))
# build subsets
data_12 <- data_Q %>%
  filter(Exp == '1' | Exp == '2')
data_23 <- data_Q %>%
  filter(Exp == '2' | Exp == '3')

#### clm table names ####
c_coef <- c("factor_1", "factor_2", "factor_3", 
            "est_1", "est_2", "est_3", 
            "sterr_1", "sterr_2", "sterr_3", 
            "z_1", "z_2", "z_3", 
            "p_1", "p_2", "p_3")
c_aov <- c("aov_df_1", "aov_df_2", "aov_df_3",
           "aov_ChiSq_1", "aov_ChiSq_2", "aov_ChiSq_3",
           "aov_p_1", "aov_p_2", "aov_p_3")

################################## ________ CLM 12 ________ ########################################
#### clm 12 SUS_score ####
variable <- "SUS_score"
data <- "12"

# model
clm_12 = clm(SUS_score ~ Exp + HMI + Exp:HMI,
            data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- tab_clm_12


#### clm 12 UMUX_score ####
variable <- "UMUX_score"
data <- "12"

# model
clm_12 = clm(UMUX_score ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 Trust ####
variable <- "Trust"
data <- "12"

# model
clm_12 = clm(Trust ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 Acceptance ####
variable <- "Acceptance"
data <- "12"

# model
clm_12 = clm(Acceptance ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 UEQ_Attractiveness ####
variable <- "UEQ_Attractiveness"
data <- "12"

# model
clm_12 = clm(UEQ_Attractiveness ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 UEQ_Perspicuity ####
variable <- "UEQ_Perspicuity"
data <- "12"

# model
clm_12 = clm(UEQ_Perspicuity ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 UEQ_Efficiency ####
variable <- "UEQ_Efficiency"
data <- "12"

# model
clm_12 = clm(UEQ_Efficiency ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 UEQ_Dependability ####
variable <- "UEQ_Dependability"
data <- "12"

# model
clm_12 = clm(UEQ_Dependability ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 UEQ_Novelty ####
variable <- "UEQ_Novelty"
data <- "12"

# model
clm_12 = clm(UEQ_Novelty ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

#### clm 12 UEQ_Stimulation ####
variable <- "UEQ_Stimulation"
data <- "12"

# model
clm_12 = clm(UEQ_Stimulation ~ Exp + HMI + Exp:HMI,
             data = data_12)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_12_sum_coef <- data.frame(summary(clm_12)[["coefficients"]])
clm_12_sum_coef_df <- tibble::rownames_to_column(clm_12_sum_coef, "factor")
clm_12_coef <- clm_12_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_12_coef <- data.frame(matrix(unlist(clm_12_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_12_aov <- anova(clm_12, type = 3)
tab_clm_12_aov <- data.frame(matrix(unlist(clm_12_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_12_aov) <- c_aov

# build table clm+aov
tab_clm_12 <- cbind(variable, data, tab_clm_12_coef, tab_clm_12_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_12)

################################## ________ CLM 23 ________ ########################################
#### clm 23 SUS_score ####
variable <- "SUS_score"
data <- "23"

# model
clm_23 = clm(SUS_score ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UMUX_score ####
variable <- "UMUX_score"
data <- "23"

# model
clm_23 = clm(UMUX_score ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 Trust ####
variable <- "Trust"
data <- "23"

# model
clm_23 = clm(Trust ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 Acceptance ####
variable <- "Acceptance"
data <- "23"

# model
clm_23 = clm(Acceptance ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UEQ_Attractiveness ####
variable <- "UEQ_Attractiveness"
data <- "23"

# model
clm_23 = clm(UEQ_Attractiveness ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UEQ_Perspicuity ####
variable <- "UEQ_Perspicuity"
data <- "23"

# model
clm_23 = clm(UEQ_Perspicuity ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UEQ_Efficiency ####
variable <- "UEQ_Efficiency"
data <- "23"

# model
clm_23 = clm(UEQ_Efficiency ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UEQ_Dependability ####
variable <- "UEQ_Dependability"
data <- "23"

# model
clm_23 = clm(UEQ_Dependability ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UEQ_Novelty ####
variable <- "UEQ_Novelty"
data <- "23"

# model
clm_23 = clm(UEQ_Novelty ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

#### clm 23 UEQ_Stimulation ####
variable <- "UEQ_Stimulation"
data <- "23"

# model
clm_23 = clm(UEQ_Stimulation ~ Exp + HMI + Exp:HMI,
             data = data_23)

# extract coefficient table with parameter estimates, standard errors and 
# Wald based p values for tests of the parameters being zero.
clm_23_sum_coef <- data.frame(summary(clm_23)[["coefficients"]])
clm_23_sum_coef_df <- tibble::rownames_to_column(clm_23_sum_coef, "factor")
clm_23_coef <- clm_23_sum_coef_df %>%
  filter(factor == "Exp.L" | factor == "HMI.L" | factor == "Exp.L:HMI.L")
tab_clm_23_coef <- data.frame(matrix(unlist(clm_23_coef), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_coef) <- c_coef

# anova: analysis of deviance (ANODE) tables also based on Wald Chi-Sq tests 
# and provides tables with type I, II and III hypothesis tests using the SAS definition
clm_23_aov <- anova(clm_23, type = 3)
tab_clm_23_aov <- data.frame(matrix(unlist(clm_23_aov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_clm_23_aov) <- c_aov

# build table clm+aov
tab_clm_23 <- cbind(variable, data, tab_clm_23_coef, tab_clm_23_aov)

## build results table (first test different)
CLM_results_table <- bind_rows(CLM_results_table, tab_clm_23)

################################## ________ save ________ ########################################
write_excel_csv(CLM_results_table, "data/results/CLM_UsabQ.csv")
rm(list=setdiff(ls(), c("CLM_results_table", "data_Q", "data_12")))