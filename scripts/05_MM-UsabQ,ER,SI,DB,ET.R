#### preparations ####
rm(list = ls())
library(tidyverse)
library(afex)
# library(ggplot2); 
library(reshape)
# library(lme4)
# library(nlme)
setwd("~/R/Multilab_Analysis")

#### readme - script ####
# script is based on paper of Singmann
# packages used: afex, function mixed
#   --> calls either lme4 or glmer, depending on the family and reports p values (lmertest()))
#   --> per default: Type 3 sums of squares (recommended by Singmann)
# 
# general model format:
#   fixed effects: Exp, HMI & Interaction Exp:HMI
#   random effects: TC (random intercept & slopes for HMI) & VP (random intercept)
#   --> afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12_VP_l)
#   
# depending on the format of the dependent data, the famiy and the method (for obtaining p values) is chosen:
#   --> for family = "gaussian" --> default method: "S"
#       for continuous decimal data with normal distribution, like weight, length, et al
#       model <- afex::mixed(AV ~ Exp*HMI + (HMI|TC) + (1|VP), data) --- calls lme4
#   --> for family = "poisson" --> method: "PB" | alternative "LRT" is not recommended, because TC has <40 factor levels
#       for positive integer or small natural number like count, individual number, frequency
#       model <- afex::mixed(AV ~ Exp*HMI + (HMI|TC) + (1|VP), data, method = "PB", family = "poisson") --- calls glmer
#   --> for family = "binomial" --> method: "PB" | alternative "LRT" is not recommended, because TC has <40 factor levels
#       binary data like 0 and 1, or proportion like survival number vs death number, positive frequency vs negative frequency, 
#       winning times vs the number of failtures, et al…
#       model <- afex::mixed(AV ~ Exp*HMI + (HMI|TC) + (1|VP), data, method = "PB", family = "binomial") --- calls glmer
#   

#### readme - Comments to model structure ####
#     factors of interest: exp + HMI
#     other pot. influencing factors: TC (different situations, different variances!) & VP (grouping variable)
#                                     potentially Age & gender, or even other sd variables, 
#                                     but not of interest, too complicated and (partially) incl. in VP
# other model structures:
#   tested with AV ER_score
# produce errors/not possible with data 
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|VP:TC), data_12_VP_l) # error
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC:VP), data_12_VP_l) # error
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|VP/TC), data_12_VP_l) # error
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC/VP), data_12_VP_l) # error
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC) + (1|VP:TC), data_12_VP_l) # error
# afex::mixed(ER_score ~ Exp*HMI + VP + (Exp*HMI|TC), data_12_VP_l) # error
# 
# possible models
# afex::mixed(ER_score ~ Exp*HMI + (HMI|TC), data_12_VP_l) # VP not included
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC), data_12_VP_l) # VP not included, no reason, why Exp should be included in r.e.
# afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC) + (1|VP), data_12_VP_l) # VP included, r.intercept & slope for Exp*HMI among TC
#                                                                       # no reason, why Exp should be included in r.e.
#                                                                       # receives singular fit warning
# chosen model
# afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12_VP_l) # VP included, r.intercept & slope for HMI among TC

#### readme - metrics ####
#   gaussian
#     ET: attention ratio street
#     ET: gaze allocation time (TC10, TC12)
#     ET: total glance time (TC10, TC12)
#     ET: gaze allocation time (TC2, TC6, TC8)
#     ET: total glance time (TC2, TC6, TC8)
#     DB: take-over time (TC10, TC12)
#   poisson
#     DB: nr hands off detections (TC6, TC7, TC8)
#     ER: ER_overall (all TC)
#   binomial
#     SI/DB: obs. Level vs rep. Level
#     SI/DB: obs. Level vs instr. Level
#     SI: 2(/1)x allocation of driving task (Emails, HandsOff, Both)
#     SI: transition problems
#     SI: rep, avail. change vs. implemented avail. change

# test with function: performance::check_normality(model) doesn't work, 
# use the graphical inspection of the AV instead:
  # ggplot(data_12, aes(ER_score)) +
  #   geom_histogram(bins = 100)
    

#### import data & build subsets ####
data_all <- read.csv("data/preprocessed/multilab_subj_complete.csv", encoding = "UTF-8")

# build data sets
data_wide <- data_all %>%
  mutate(Exp = case_when(
    Exp == 1 ~ "e1",
    Exp == 2 ~ "e2",
    Exp == 3 ~ "e3",
    TRUE ~ as.character(Exp))) %>%
  mutate(Exp_name = Exp) %>%
  mutate(VPNr = as.character(VPNr)) %>%
  unite("VP", Exp_name,VPNr) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI)) %>%
  mutate(VP = factor(VP)) %>%
  dplyr::select(c(Exp, HMI, VP, 
                  ends_with("_ER"))) # adjustment needed for the other metrics

names(data_wide) <- c("Exp", "HMI", "VP", "TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12")

# restructure for long format
data_long<-melt(data_wide, id = c("Exp", "HMI", "VP"), 
                   measured = c("TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12"))

names(data_long) <-c("Exp", "HMI", "VP", "TC","ER_score")

# create subsets
data_12 <- data_long %>%
  filter(Exp != "e3")

data_23 <- data_long %>%
  filter(Exp !="e1")
################################## ________ MM 12 ________ ########################################
model <- afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12) # VP as grouping variable, TC as grouping variable and affecting the variability among HMIs
model # output = anova(model, type = 3)
summary(model) # output != output: model

a <- afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12, family = gaussian(link=log), method = "LRT")
a
afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12, method = "BP", family = "quapoisson")
################################## ________ save ________ ########################################