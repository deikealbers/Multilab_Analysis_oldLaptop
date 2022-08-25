#### preparations ####
rm(list = ls())
library(tidyverse)
library(ggplot2); 
library(reshape)
library(lme4)
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")
data_12 <- data_all %>%
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
  dplyr::select(c(Exp, HMI, VP, ends_with("_ER")))


names(data_12) <- c("Exp", "HMI", "VP", "01", "02", "03", "04", "05", "06" , "07", "08", "09", "10", "11", "12")

# restructure for long format
data_12_l<-melt(data_12, id = c("Exp", "HMI", "VP"), measured = c("01", "02", "03", "04", "05", "06" , "07", "08", "09", "10", "11", "12"))
names(data_12_l) <-c("Exp", "HMI", "VP", "TC", "ER_score")

model =  lmer(ER_score ~ Exp +  HMI + (1|VP) + (1|TC),
             data=data_12_l,
             REML=FALSE)
summary(model)


model = lmer(ER_score ~ Exp +
               HMI + (1+HMI|VP) +
               (1+HMI|TC),
             data=data_12_l,
             REML=FALSE)

summary(model)



politeness.model2 = lmer(frequency ~ attitude + gender + 
                           (1|subject) + (1|scenario), data=politeness)
summary(politeness.model2)



library(nlme)
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
