#### script for combining all 3 datasets of Multilab exp
# not incl.: Hofstede VSM Q (separate analysis with online surveys)
# not incl.: Kano questions on Speech Outputs

#### notes ####
# different variables in experiments
# only in exp1 & exp2 (+1 variable):
# Sehhilfe
# only in exp2 & exp3 (+2 variables):
# Wetter
# Licht
# only in exp3 (+70 variables):
# Nation
# NationGeb
# NationGeb2
# SehhilfeAuto
# SehhilfeAutoJetzt
# SehhilfeLesen
# SehhilfeLesenJetzt
# TPComments_PreQ
# VSM... (24 items/variables)
# UsabFactor... (24 items/variables)
# Speech... (14 items/variables)
# different formats for exp3 (integer) compared to exp01 & exp02 (numeric); 
# seem irrelevant since format automatically is numeric after binding
# Geschlecht
# Erfahrung
# KilometerJahr
# Vorwissen
# Oft...
# Wetter (not exp01)
# Licht (not exp01)
# ER 
# Be aware of:
# ChangeAvailReasonSel: TC10: code [0] means "Systemgrenze"; TC6 & TC12: code [0] means "Sensorfehler"
# *LevelSel: Sonstiges has code [99]
# *ChangeAvailSel: Sonstiges has code [99]
# *ChangeAvailReasonSel: Sonstiges has code [99]
# Further differences in the following variables
# Geschlecht: Option "other" [code 3] only in exp03
# OftCC/OftACC/OftLKA: Option "seldom" [code 1] only in exp03; exp01 & exp02 skip this code (-> [0,2,3,4,5])


### preparations ###
rm(list = ls())
library(tidyverse)
setwd("~/R/Multilab_Analysis")

#### import data ####
# Read in files
data_exp01 <- read.csv("data/preprocessed/preprocessed_Lime_exp01.csv", encoding = "UTF-8")
data_exp02 <- read.csv("data/preprocessed/preprocessed_Lime_exp02.csv", encoding = "UTF-8")
data_exp03 <- read.csv("data/preprocessed/preprocessed_Lime_exp03.csv", encoding = "UTF-8")

#### combine exp01 and exp02 ####
merge <- bind_rows(data_exp01, data_exp02, data_exp03)

#reorder certain columns
all_merged <- merge %>%
  rename(startdate = X.U.FEFF.startdate) %>%
  mutate(Exp = as.factor(Exp)) %>%
  mutate(HMI = as.factor(HMI)) %>%
  relocate(VPNr, .after = VPCode) %>%
  relocate(SehhilfeLesenJetzt, .after = Sehhilfe) %>%
  relocate(SehhilfeLesen, .after = Sehhilfe) %>%
  relocate(SehhilfeAutoJetzt, .after = Sehhilfe) %>%
  relocate(SehhilfeAuto, .after = Sehhilfe) %>%
  relocate(Licht, .after = AutomarkeLKA) %>%
  relocate(Wetter, .after = AutomarkeLKA) %>%
  relocate(TPComments_PreQ, .after = AutomarkeLKA) %>%
  relocate(NationGeb2, .after = Geschlecht) %>%
  relocate(NationGeb, .after = Geschlecht) %>%
  relocate(Nation, .after = Geschlecht)

data_all <- all_merged

#### save data ####
write_excel_csv(data_all, "data/preprocessed/merged_Lime_all.csv")
rm(list=setdiff(ls(), c("data_all")))

# when importing, mutate Exp and HMI
#  mutate(Exp = as.factor(Exp)) %>%
#  mutate(HMI = as.factor(HMI)) %>%
