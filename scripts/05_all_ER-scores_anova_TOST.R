#### script for descriptive analysis, anova and TOST testing of experimenter rating
# https://statsandr.com/blog/anova-in-r/#introduction


#### preparations ####
rm(list = ls())
library(tidyverse)
library(ggplot2); 
setwd("~/R/Multilab")

#### load dataset ####
load("data/processed/R_data_all_Q.RData")

data <- data_Q %>%
  select(c(Exp, HMI, VPNr, ends_with("_ER"))) %>%
  add_column(ER_overall = (.$TC01_ER + .$TC02_ER + .$TC03_ER + .$TC04_ER +
               .$TC05_ER + .$TC06_ER + .$TC07_ER + .$TC08_ER +
               .$TC09_ER + .$TC10_ER + .$TC11_ER + .$TC12_ER)/12)


ER_summary <- data %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(TC01_ER, TC02_ER, TC03_ER, TC04_ER, TC05_ER, TC06_ER, TC07_ER, TC08_ER, TC09_ER, TC10_ER, TC11_ER, TC12_ER), 
               .funs = c(mean = "mean", sd ="sd", min = "min", max = "max", median = "median"))
print(as.data.frame(ER_summary))

test <- data %>%
  filter(Exp == 2, HMI == "LC")
summary(test$TC03_ER)

test1 <- data %>%
  filter(Exp == 2)
summary(test$TC03_ER)

test3 <- data %>%
  filter(Exp == 2, HMI == "HC")
summary(test$TC03_ER)
