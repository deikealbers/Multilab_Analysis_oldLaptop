#### preparations ####
rm(list = ls())
library(tidyverse)
library(ggplot2); 
library(reshape)
library(lme4)
library(nlme)
setwd("~/R/Multilab_Analysis")

#### import data ####
data_all <- read.csv("data/preprocessed/multilab_subj_complete.csv", encoding = "UTF-8")

#### with VP ####
# ## adjust data set ##
# 
# data_12_VP <- data_all %>%
#   filter(Exp != 3) %>%  
#   mutate(Exp = case_when(
#     Exp == 1 ~ "e1",
#     Exp == 2 ~ "e2",
#     Exp == 3 ~ "e3",
#     TRUE ~ as.character(Exp))) %>%
#   mutate(Exp_name = Exp) %>%
#   mutate(VPNr = as.character(VPNr)) %>%
#   unite("VP", Exp_name,VPNr) %>%
#   mutate(Exp = factor(Exp)) %>%
#   mutate(HMI = factor(HMI)) %>%
#   dplyr::select(c(Exp, HMI, VP, ends_with("_ER")))
# 
# names(data_12_VP) <- c("Exp", "HMI", "VP", "TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12")
# 
# ## restructure for long format
# data_12_VP_l<-melt(data_12_VP, id = c("Exp", "HMI", "VP"), measured = c("TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12"))
# names(data_12_VP_l) <-c("Exp", "HMI", "VP", "TC", "ER_score")
# 
# ## models 
# model =  lmer(ER_score ~ Exp +  HMI + (1|VP) + (1|TC),
#               data=data_12_VP_l,
#               REML=FALSE)
# summary(model)
# 
# 
# model = lmer(ER_score ~ Exp +
#                HMI + (1+HMI|VP) +
#                (1+HMI|TC),
#              data=data_12_VP_l,
#              REML=FALSE)
# summary(model)
# 
# # politeness.model2 = lmer(frequency ~ attitude + gender + 
# #                            (1|subject) + (1|scenario), data=politeness)
# # summary(politeness.model2)
# ###
# model_interceptOnly <- gls(ER_score ~ 1, data = data_12_VP_l, method = "ML")
# summary(model_interceptOnly)
# 
# ###
# model_randominterceptOnly <-lme(ER_score ~ 1, data = data_12_VP_l, random = ~1|TC, method = "ML")
# summary(model_randominterceptOnly)
# 
# anova(model_interceptOnly, model_randominterceptOnly)
# 
# ###
# model_randominterceptHMI <-lme(ER_score ~ HMI, data = data_12_VP_l, random = ~1|TC, method = "ML")
# summary(model_randominterceptHMI)
# 
# anova(model_randominterceptOnly, model_randominterceptHMI)
# 
# ###
# model_randominterceptHMIExp <-lme(ER_score ~ HMI + Exp, data = data_12_VP_l, random = ~1|TC, method = "ML")
# summary(model_randominterceptHMIExp)
# 
# anova(model_randominterceptHMI, model_randominterceptHMIExp)

#### sociodemographics gender + age included ####
# adjust data set ##


data_12 <- data_all %>%
  filter(Exp != 3) %>%  
  mutate(Exp = case_when(
    Exp == 1 ~ "e1",
    Exp == 2 ~ "e2",
    Exp == 3 ~ "e3",
    TRUE ~ as.character(Exp))) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI)) %>%
  dplyr::select(c(Exp, HMI, Alter, Geschlecht, ends_with("_ER")))
names(data_12) <- c("Exp", "HMI", "Alter", "Geschlecht", "TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12")

## restructure for long format
data_12_l<-melt(data_12, id = c("Exp", "HMI", "Alter", "Geschlecht"), 
                     measured = c("TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12"))

names(data_12_l) <-c("Exp", "HMI", "Alter", "Geschlecht", "TC","ER_score")

### max model
model_max = lmerTest::lmer(ER_score ~ Exp * HMI + 
                   (1|TC),
                 data=data_12_l,
                 REML=FALSE)
summary(model_max)
anova(model_max)



## models
model =  lmer(ER_score ~ Exp +  HMI + (1|Alter) + (1|TC),
              data=data_12_l,
              REML=FALSE)
summary(model)


model = lmer(ER_score ~ Exp +
               HMI + (1+HMI|Alter) +
               (1+HMI|TC),
             data=data_12_l,
             REML=FALSE)
summary(model)

# politeness.model2 = lmer(frequency ~ attitude + gender + 
#                            (1|subject) + (1|scenario), data=politeness)
# summary(politeness.model2)

###
model_interceptOnly <- gls(ER_score ~ 1, data = data_12_l, method = "ML")
summary(model_interceptOnly)

###
model_randominterceptOnly <-lme(ER_score ~ 1, data = data_12_l, random = ~1|TC, method = "ML")
summary(model_randominterceptOnly)

anova(model_interceptOnly, model_randominterceptOnly)

###
model_randominterceptHMI <-lme(ER_score ~ HMI, data = data_12_l, random = ~1|TC, method = "ML")
summary(model_randominterceptHMI)

anova(model_randominterceptOnly, model_randominterceptHMI)

###
model_randominterceptHMIExp <-lme(ER_score ~ HMI + Exp, data = data_12_l, random = ~1|TC, method = "ML")
summary(model_randominterceptHMIExp)

anova(model_randominterceptHMI, model_randominterceptHMIExp)

### random slopes and/or interactions? --- check with Hausarbeit Svenja?




anova <- aov(data_12$TC01~data_12$Exp*data_12$HMI)

plot(anova, 2)

#### 27.08.2022 ####
data_12 <- data_all %>%
  filter(Exp != 3) %>%  
  mutate(Exp = case_when(
    Exp == 1 ~ "e1",
    Exp == 2 ~ "e2",
    Exp == 3 ~ "e3",
    TRUE ~ as.character(Exp))) %>%
  mutate(Exp = factor(Exp)) %>%
  mutate(HMI = factor(HMI)) %>%
  dplyr::select(c(Exp, HMI, VPNr, ends_with("_ER")))
names(data_12) <- c("Exp", "HMI", "VPNr", "TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12")

## restructure for long format
data_12_l<-melt(data_12, id = c("Exp", "HMI", "VPNr"), 
                measured = c("TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12"))

names(data_12_l) <-c("Exp", "HMI", "VP", "TC","ER_score")

data_12_VP <- data_all %>%
  filter(Exp != 3) %>%
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
  dplyr::select(c(Exp, HMI, VP, ends_with("_ER")))

names(data_12_VP) <- c("Exp", "HMI", "VP", "TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12")

## restructure for long format
data_12_VP_l<-melt(data_12_VP, id = c("Exp", "HMI", "VP"), 
                measured = c("TC01", "TC02", "TC03", "TC04", "TC05", "TC06" , "TC07", "TC08", "TC09", "TC10", "TC11", "TC12"))

names(data_12_VP_l) <-c("Exp", "HMI", "VP", "TC","ER_score")

#### model ####
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|VP:TC), data_12_VP_l) # error
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC:VP), data_12_VP_l) # error
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|VP/TC), data_12_VP_l) # error
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC/VP), data_12_VP_l) # error
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC), data_12_VP_l)
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC) + (1|VP), data_12_VP_l) # most accurate? random intercepts & slopes depending on TC & intercepts for VPs
afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12_VP_l) # most accurate? random intercepts & slopes depending on TC & intercepts for VPs
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC) + (1|VP:TC), data_12_VP_l) # error


### check influence of VP effect in different models ###
afex::mixed(ER_score ~ Exp*HMI + VP + (Exp*HMI|TC), data_12_VP_l) # error
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC), data_12_VP_l)
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC) + (1|VP), data_12_VP_l)

afex::mixed(ER_score ~ Exp*HMI + (HMI|TC), data_12_VP_l)

#### ausgewählte Modelle ####
afex::mixed(ER_score ~ Exp*HMI + (Exp*HMI|TC) + (1|VP), data_12_VP_l)  # singular fit warning
model <- afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12_VP_l) # VP as grouping variable, TC as grouping variable and affecting the variability among HMIs
performance::check_normality(model)

afex::mixed(ER_score ~ Exp*HMI + (HMI|TC) + (1|VP), data_12_VP_l, method = "LRT", family = "poisson")

ggplot(data_12_VP_l, aes(ER_score)) +
  geom_histogram(bins = 100)

data_12_VP_log <- data_12_VP_l %>%
  add_column(ER_score_log = sqrt(.$ER_score))

ggplot(data_12_VP_log, aes(ER_score_log)) +
  geom_histogram(bins = 100)
