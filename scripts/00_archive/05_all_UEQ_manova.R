#### script for anova testing of UEQ scores ####

# builds on script 04_Lime_AutLvl_UsabQ_add_shortI+ER-scores.R

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
library(pastecs) 
library(reshape) 
library(WSR2)
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
data_Q <- data_all %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI))

#### data sets ####
# data_12 is for comparison sim (GER) vs test track (GER)
# data_23 is for comparison test track (GER) vs test track (USA)

data_12 <- data_all %>%
  filter(Exp == '1' | Exp == '2')

data_23 <- data_all %>%
  filter(Exp == '2' | Exp == '3')

#### define  column names of results table
# c_manova_t2

#### manova 12 ####
UEQ_scales_12<-cbind(data_12$UEQ_Attractiveness, data_12$UEQ_Perspicuity, data_12$UEQ_Efficiency, 
                  data_12$UEQ_Dependability, data_12$UEQ_Novelty, data_12$UEQ_Stimulation)

manova_UEQ_12<-manova(UEQ_scales_12 ~ Exp*HMI, data = data_12)
manova_UEQ_12
# summary(manova_UEQ_12, intercept = TRUE)
sum_manova_UEQ_12 <- Anova(manova_UEQ_12, type = "III")
sum_manova_UEQ_12

# tab_manova_UEQ_12 <- data.frame(matrix(unlist(sum_manova_UEQ_12), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# names(tab_manova_UEQ_12) <- c_manova_t2
a <- capture.output(sum_manova_UEQ_12)
