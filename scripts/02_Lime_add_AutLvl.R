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
data_all <- read.csv("data/preprocessed/merged_Lime_all.csv", encoding = "UTF-8")

# factorize columns Exp & HMI
data_all <- data_all %>%
  mutate(Exp = as.factor(Exp)) %>%
  mutate(HMI = as.factor(HMI))

#### transform 99 replies in short interviews and compare ####
# 1) read in csv with the transformed codes
transform_99 <- read.csv("data/other/all_shortInterviews_transform99replies.csv", encoding = "UTF-8")
transform_99 <- transform_99 %>%
  mutate(Exp = factor(Exp))
# 2) join data
all_merged_t99 <- left_join(data_all, transform_99, by = c("VPNr", "Exp"))
# 3) create new column with replacements of 99 (2 steps: new column=original Level; change column if value=99)
all_merged_t99_newcolumns <- all_merged_t99 %>%
  add_column(TC01_LevelRep = .$TC01_LevelSel, .after = "TC01_LevelSel") %>%
  add_column(TC02_LevelRep = .$TC02_LevelSel, .after = "TC02_LevelSel") %>%
  add_column(TC03_LevelRep = .$TC03_LevelSel, .after = "TC03_LevelSel") %>%
  add_column(TC04_LevelRep = .$TC04_LevelSel, .after = "TC04_LevelSel") %>%
  add_column(TC05_LevelRep = .$TC05_LevelSel, .after = "TC05_LevelSel") %>%
  add_column(TC06_LevelRep = .$TC06_LevelSel, .after = "TC06_LevelSel") %>%
  add_column(TC07_LevelRep = .$TC07_LevelSel, .after = "TC07_LevelSel") %>%
  add_column(TC08_LevelRep = .$TC08_LevelSel, .after = "TC08_LevelSel") %>%
  add_column(TC09_LevelRep = .$TC09_LevelSel, .after = "TC09_LevelSel") %>%
  add_column(TC10_LevelRep = .$TC10_LevelSel, .after = "TC10_LevelSel") %>%
  add_column(TC11_LevelRep = .$TC11_LevelSel, .after = "TC11_LevelSel") %>%
  add_column(TC12_LevelRep = .$TC12_LevelSel, .after = "TC12_LevelSel") %>%
  add_column(TC01_ChangeAvailRep = .$TC01_ChangeAvailSel, .after = "TC01_ChangeAvailSel") %>%
  add_column(TC02_ChangeAvailRep = .$TC02_ChangeAvailSel, .after = "TC02_ChangeAvailSel") %>%
  add_column(TC03_ChangeAvailRep = .$TC03_ChangeAvailSel, .after = "TC03_ChangeAvailSel") %>%
  add_column(TC04_ChangeAvailRep = .$TC04_ChangeAvailSel, .after = "TC04_ChangeAvailSel") %>%
  add_column(TC05_ChangeAvailRep = .$TC05_ChangeAvailSel, .after = "TC05_ChangeAvailSel") %>%
  add_column(TC06_ChangeAvailRep = .$TC06_ChangeAvailSel, .after = "TC06_ChangeAvailSel") %>%
  add_column(TC07_ChangeAvailRep = .$TC07_ChangeAvailSel, .after = "TC07_ChangeAvailSel") %>%
  add_column(TC08_ChangeAvailRep = .$TC08_ChangeAvailSel, .after = "TC08_ChangeAvailSel") %>%
  add_column(TC09_ChangeAvailRep = .$TC09_ChangeAvailSel, .after = "TC09_ChangeAvailSel") %>%
  add_column(TC10_ChangeAvailRep = .$TC10_ChangeAvailSel, .after = "TC10_ChangeAvailSel") %>%
  add_column(TC11_ChangeAvailRep = .$TC11_ChangeAvailSel, .after = "TC11_ChangeAvailSel") %>%
  add_column(TC12_ChangeAvailRep = .$TC12_ChangeAvailSel, .after = "TC12_ChangeAvailSel") %>%
  add_column(TC06_ChangeAvailReasonRep = .$TC06_ChangeAvailReasonSel, .after = "TC06_ChangeAvailReasonSel") %>%
  add_column(TC10_ChangeAvailReasonRep = .$TC10_ChangeAvailReasonSel, .after = "TC10_ChangeAvailReasonSel") %>%
  add_column(TC12_ChangeAvailReasonRep = .$TC12_ChangeAvailReasonSel, .after = "TC12_ChangeAvailReasonSel") 

all_merged_t99_replace99 <- all_merged_t99_newcolumns %>%
  mutate(TC01_LevelRep = ifelse(TC01_LevelRep == "99", TC01_LevelSel_n, TC01_LevelRep)) %>%
  mutate(TC02_LevelRep = ifelse(TC02_LevelRep == "99", TC02_LevelSel_n, TC02_LevelRep)) %>%  
  mutate(TC03_LevelRep = ifelse(TC03_LevelRep == "99", TC03_LevelSel_n, TC03_LevelRep)) %>%
  mutate(TC04_LevelRep = ifelse(TC04_LevelRep == "99", TC04_LevelSel_n, TC04_LevelRep)) %>%
  mutate(TC05_LevelRep = ifelse(TC05_LevelRep == "99", TC05_LevelSel_n, TC05_LevelRep)) %>%
  mutate(TC06_LevelRep = ifelse(TC06_LevelRep == "99", TC06_LevelSel_n, TC06_LevelRep)) %>%
  mutate(TC07_LevelRep = ifelse(TC07_LevelRep == "99", TC07_LevelSel_n, TC07_LevelRep)) %>%
  mutate(TC08_LevelRep = ifelse(TC08_LevelRep == "99", TC08_LevelSel_n, TC08_LevelRep)) %>%
  mutate(TC09_LevelRep = ifelse(TC09_LevelRep == "99", TC09_LevelSel_n, TC09_LevelRep)) %>%
  mutate(TC10_LevelRep = ifelse(TC10_LevelRep == "99", TC10_LevelSel_n, TC10_LevelRep)) %>%
  mutate(TC11_LevelRep = ifelse(TC11_LevelRep == "99", TC11_LevelSel_n, TC11_LevelRep)) %>%
  mutate(TC12_LevelRep = ifelse(TC12_LevelRep == "99", TC12_LevelSel_n, TC12_LevelRep)) %>%
  mutate(TC01_ChangeAvailRep = ifelse(TC01_ChangeAvailRep == "99", TC01_ChangeAvail_n, TC01_ChangeAvailRep)) %>%
  mutate(TC02_ChangeAvailRep = ifelse(TC02_ChangeAvailRep == "99", TC02_ChangeAvail_n, TC02_ChangeAvailRep)) %>%  
  mutate(TC03_ChangeAvailRep = ifelse(TC03_ChangeAvailRep == "99", TC03_ChangeAvail_n, TC03_ChangeAvailRep)) %>%
  mutate(TC04_ChangeAvailRep = ifelse(TC04_ChangeAvailRep == "99", TC04_ChangeAvail_n, TC04_ChangeAvailRep)) %>%
  mutate(TC05_ChangeAvailRep = ifelse(TC05_ChangeAvailRep == "99", TC05_ChangeAvail_n, TC05_ChangeAvailRep)) %>%
  mutate(TC06_ChangeAvailRep = ifelse(TC06_ChangeAvailRep == "99", TC06_ChangeAvail_n, TC06_ChangeAvailRep)) %>%
  mutate(TC07_ChangeAvailRep = ifelse(TC07_ChangeAvailRep == "99", TC07_ChangeAvail_n, TC07_ChangeAvailRep)) %>%
  mutate(TC08_ChangeAvailRep = ifelse(TC08_ChangeAvailRep == "99", TC08_ChangeAvail_n, TC08_ChangeAvailRep)) %>%
  mutate(TC09_ChangeAvailRep = ifelse(TC09_ChangeAvailRep == "99", TC09_ChangeAvail_n, TC09_ChangeAvailRep)) %>%
  mutate(TC10_ChangeAvailRep = ifelse(TC10_ChangeAvailRep == "99", TC10_ChangeAvail_n, TC10_ChangeAvailRep)) %>%
  mutate(TC11_ChangeAvailRep = ifelse(TC11_ChangeAvailRep == "99", TC11_ChangeAvail_n, TC11_ChangeAvailRep)) %>%
  mutate(TC12_ChangeAvailRep = ifelse(TC12_ChangeAvailRep == "99", TC12_ChangeAvail_n, TC12_ChangeAvailRep)) %>%
  mutate(TC06_ChangeAvailReasonRep = ifelse(TC06_ChangeAvailReasonRep == "99", TC06_ChangeAvailReason_n, TC06_ChangeAvailReasonRep)) %>%
  mutate(TC10_ChangeAvailReasonRep = ifelse(TC10_ChangeAvailReasonRep == "99", TC10_ChangeAvailReason_n, TC10_ChangeAvailReasonRep)) %>%
  mutate(TC12_ChangeAvailReasonRep = ifelse(TC12_ChangeAvailReasonRep == "99", TC12_ChangeAvailReason_n, TC12_ChangeAvailReasonRep))
# 4) transform values from factors to the actual LoA + transform replies of ChangeAvail
# levels of automation
all_merged_t99_replaced <- all_merged_t99_replace99 %>%
  mutate(across(ends_with("_LevelRep"), ~case_when(
    . == 0 ~0,   # 'Manuelles Fahren' ~0,
    . == 1 ~0,   # 'Keine Automationsstufe' ~1,
    . == 2 ~2,   # 'Assistiertes Fahren' ~2,
    . == 3 ~2,   #'Mittlere Stufe' ~3,
    . == 4 ~3,   # 'Automatisiertes Fahren' ~4,
    . == 5 ~3,   # 'Hoechste Stufe' ~5,
    . == 99 ~99, # other reported levels (not clearly assignable to one LoA are 99)
    TRUE ~ as.numeric(.)))) %>%
  mutate(across(ends_with("_ChangeAvailRep"), ~case_when(
    . == 0 ~23,  # 'Alle verfuegbar' ~0,
    . == 1 ~23,  # 'Assistiert und Automatisiertes Fahren verfuegbar' ~1,
    . == 2 ~23,  # 'Automatisiert verfuegbar' ~2,
    . == 3 ~2,   # 'Assistiert verfuegbar' ~3,
    . == 5 ~2,   # 'Automatisiert nicht mehr verfuegbar' ~5,
    . == 4 ~0,   # 'Keines verfuegbar' ~4,
    . == 6 ~0,   # 'Automatisiert und Assistiert nicht mehr verfuegbar' ~6,
    . == 7 ~0,   # 'Keines mehr verfuegbar' ~7,
    . == 8 ~0,   # 'Nur Manuell verfuegbar' ~8,
    . == 99 ~99, # 'Sonstiges' ~ 99, #### falsch/unsicher
    . == 88 ~88, #  beschreibt etwas anders als ChangeAvail --> als (in)korrekt werten
    . == 77 ~77, #  beschreibt HoW --> als (in)korrekt werten
    . == 66 ~66, #  assistiert nicht mehr verfuegbar
    TRUE ~ as.numeric(.)))) %>%
  mutate_at(vars(ends_with("_ChangeAvailRep")), ~replace_na(., 1000)) %>% # NAs cannot be compared, therefore it is replaced with 1000
  mutate_at(vars(ends_with("_ChangeAvailReasonRep")), ~replace_na(., 1000)) # NAs cannot be compared, therefore it is replaced with 1000

## Description ChangeAvail
# 23   correct for: TC02, TC08, TC11;  alle LoA/automatisiert verfuegbar (0->23: TC02 u TC11 & 2->23: TC08)
# 2    correct for: TC06;              L3 nicht mehr verfuegbar, nur L2 (23->2: TC06) 
# 0    correct for: TC10, TC12;        nur L0 verfuegbar (23->0: TC10 u TC12)
# 99   incorrect for all;              'Sonstiges' ~ 99, #### falsch/unsicher
# 88   incorrect for TC02, TC06, TC8, TC10-TC12; correct for all others; beschreibt etwas anders als ChangeAvail --> als (in)korrekt werten
# 77   incorrect for TC02, TC06, TC8, TC10-TC12; correct for all others; beschreibt HoW --> als (in)korrekt werten
# 66   half correct for TC10, TC12;    "assistiert nicht mehr verfuegbar" (nur bei TC10 u TC12 als halbkorrekt werten?! <- kommt bei den TC nicht vor)
# 1000 incorrect for TC02, TC06, TC8, TC10-TC12; correct for all others; keine Aenderung erkannt --> als (in)korrekt werten
## Description ChangeAvailReason
# 0    correct; 'Systemgrenze' or 'Sensorfehler' ~0,
# 1    incorrect; 'Weiss nicht mehr' ~1,
# 2    incorrect; 'Nichts erkannt' ~2,
# 99   incorrect; Sonstiges/falsch
# 77   incorrect; beschreibt HoW
# 1000 incorrect: erkennt keine Aenderung 

# 5) create new column with instructed and observed LoA and its comparisons
## load datasets
AutLvl_01 <- read.csv("data/drivingdata/Exp01_AutLvl.csv", encoding = "UTF-8")
AutLvl_02 <- read.csv("data/drivingdata/Exp02_AutLvl.csv", encoding = "UTF-8")
AutLvl_03 <- read.csv("data/drivingdata/Exp03_AutLvl.csv", encoding = "UTF-8")
## prepare datasets
AutLvl_01_a <- AutLvl_01 %>%
  add_column(Exp = 1, .before = "VP")
AutLvl_02_a <- AutLvl_02 %>%
  add_column(Exp = 2, .before = "VP")
AutLvl_03_a <- AutLvl_03 %>%
  add_column(Exp = 3, .before = "VP")

AutLvl_merge <- bind_rows(AutLvl_01_a, AutLvl_02_a, AutLvl_03_a)

AutLvl_all <- AutLvl_merge %>%
  rename(VPNr = VP) %>%
  mutate(Exp = factor(Exp)) %>%
  rename(TC01_observedLevel = TC01) %>%
  rename(TC02_observedLevel = TC02) %>%
  rename(TC03_observedLevel = TC03) %>%
  rename(TC04_observedLevel = TC04) %>%
  rename(TC05_observedLevel = TC05) %>%
  rename(TC06_observedLevel = TC06) %>%
  rename(TC07_observedLevel = TC07) %>%
  rename(TC08_observedLevel = TC08) %>%
  rename(TC09_observedLevel = TC09) %>%
  rename(TC10_observedLevel = TC10) %>%
  rename(TC11_observedLevel = TC11) %>%
  rename(TC12_observedLevel = TC12) %>%
  add_column(TC01_Instr = 0) %>%
  add_column(TC02_Instr = 0) %>%
  add_column(TC03_Instr = 3) %>%
  add_column(TC04_Instr = 3) %>%
  add_column(TC05_Instr = 2) %>%
  add_column(TC06_Instr = 2) %>%
  add_column(TC07_Instr = 2) %>%
  add_column(TC08_Instr = 2) %>%
  add_column(TC09_Instr = 3) %>%
  add_column(TC10_Instr = 0) %>%
  add_column(TC11_Instr = 3) %>%
  add_column(TC12_Instr = 0)
## join data questionnaires with AutLvl
all_merged_t99_replaced_aut <- left_join(all_merged_t99_replaced, AutLvl_all, by = c("VPNr", "Exp"))

## comparisons
## observed vs instructed: 1= correct; 0= incorrect
## observed vs reported: 1= correct; 0= incorrect (99 is automatically wrong)

all_merged_t99_replaced_aut_comp1 <- all_merged_t99_replaced_aut %>%
  add_column(TC01_LevelObserved_Instr = NA) %>%
  add_column(TC02_LevelObserved_Instr = NA) %>%
  add_column(TC03_LevelObserved_Instr = NA) %>%
  add_column(TC04_LevelObserved_Instr = NA) %>%
  add_column(TC05_LevelObserved_Instr = NA) %>%
  add_column(TC06_LevelObserved_Instr = NA) %>%
  add_column(TC07_LevelObserved_Instr = NA) %>%
  add_column(TC08_LevelObserved_Instr = NA) %>%
  add_column(TC09_LevelObserved_Instr = NA) %>%
  add_column(TC10_LevelObserved_Instr = NA) %>%
  add_column(TC11_LevelObserved_Instr = NA) %>%
  add_column(TC12_LevelObserved_Instr = NA) %>%
  mutate(TC01_LevelObserved_Instr = ifelse(TC01_observedLevel == TC01_Instr, 1, 0)) %>%
  mutate(TC02_LevelObserved_Instr = ifelse(TC02_observedLevel == TC02_Instr, 1, 0)) %>%
  mutate(TC03_LevelObserved_Instr = ifelse(TC03_observedLevel == TC03_Instr, 1, 0)) %>%
  mutate(TC04_LevelObserved_Instr = ifelse(TC04_observedLevel == TC04_Instr, 1, 0)) %>%
  mutate(TC05_LevelObserved_Instr = ifelse(TC05_observedLevel == TC05_Instr, 1, 0)) %>%
  mutate(TC06_LevelObserved_Instr = ifelse(TC06_observedLevel == TC06_Instr, 1, 0)) %>%
  mutate(TC07_LevelObserved_Instr = ifelse(TC07_observedLevel == TC07_Instr, 1, 0)) %>%
  mutate(TC08_LevelObserved_Instr = ifelse(TC08_observedLevel == TC08_Instr, 1, 0)) %>%
  mutate(TC09_LevelObserved_Instr = ifelse(TC09_observedLevel == TC09_Instr, 1, 0)) %>%
  mutate(TC10_LevelObserved_Instr = ifelse(TC10_observedLevel == TC10_Instr, 1, 0)) %>%
  mutate(TC11_LevelObserved_Instr = ifelse(TC11_observedLevel == TC11_Instr, 1, 0)) %>%
  mutate(TC12_LevelObserved_Instr = ifelse(TC12_observedLevel == TC12_Instr, 1, 0))

all_merged_t99_replaced_aut_comp2 <- all_merged_t99_replaced_aut_comp1%>%
  add_column(TC01_LevelObserved_Rep = NA) %>%
  add_column(TC02_LevelObserved_Rep = NA) %>%
  add_column(TC03_LevelObserved_Rep = NA) %>%
  add_column(TC04_LevelObserved_Rep = NA) %>%
  add_column(TC05_LevelObserved_Rep = NA) %>%
  add_column(TC06_LevelObserved_Rep = NA) %>%
  add_column(TC07_LevelObserved_Rep = NA) %>%
  add_column(TC08_LevelObserved_Rep = NA) %>%
  add_column(TC09_LevelObserved_Rep = NA) %>%
  add_column(TC10_LevelObserved_Rep = NA) %>%
  add_column(TC11_LevelObserved_Rep = NA) %>%
  add_column(TC12_LevelObserved_Rep = NA) %>%
  mutate(TC01_LevelObserved_Rep = ifelse(TC01_observedLevel == TC01_LevelRep, 1, 0)) %>%
  mutate(TC02_LevelObserved_Rep = ifelse(TC02_observedLevel == TC02_LevelRep, 1, 0)) %>%
  mutate(TC03_LevelObserved_Rep = ifelse(TC03_observedLevel == TC03_LevelRep, 1, 0)) %>%
  mutate(TC04_LevelObserved_Rep = ifelse(TC04_observedLevel == TC04_LevelRep, 1, 0)) %>%
  mutate(TC05_LevelObserved_Rep = ifelse(TC05_observedLevel == TC05_LevelRep, 1, 0)) %>%
  mutate(TC06_LevelObserved_Rep = ifelse(TC06_observedLevel == TC06_LevelRep, 1, 0)) %>%
  mutate(TC07_LevelObserved_Rep = ifelse(TC07_observedLevel == TC07_LevelRep, 1, 0)) %>%
  mutate(TC08_LevelObserved_Rep = ifelse(TC08_observedLevel == TC08_LevelRep, 1, 0)) %>%
  mutate(TC09_LevelObserved_Rep = ifelse(TC09_observedLevel == TC09_LevelRep, 1, 0)) %>%
  mutate(TC10_LevelObserved_Rep = ifelse(TC10_observedLevel == TC10_LevelRep, 1, 0)) %>%
  mutate(TC11_LevelObserved_Rep = ifelse(TC11_observedLevel == TC11_LevelRep, 1, 0)) %>%
  mutate(TC12_LevelObserved_Rep = ifelse(TC12_observedLevel == TC12_LevelRep, 1, 0))

# 6) create new column with correct avail_level and its comparison
## implemented vs reported: 1= correct; 0= incorrect (88 is treated as 1000 (no change))
# (0, 0.5 (partly correct) and 1 (correct))

all_merged_t99_replaced_aut_comp2_avail <- all_merged_t99_replaced_aut_comp2 %>%
  add_column(TC01_ImplemAvail = 1000, .after = "TC01_ChangeAvailRep") %>% # no change
  add_column(TC02_ImplemAvail = 23, .after = "TC02_ChangeAvailRep") %>% # 2+3 become available
  add_column(TC03_ImplemAvail = 1000, .after = "TC03_ChangeAvailRep") %>% # no change
  add_column(TC04_ImplemAvail = 1000, .after = "TC04_ChangeAvailRep") %>% # no change
  add_column(TC05_ImplemAvail = 1000, .after = "TC05_ChangeAvailRep") %>% # no change
  add_column(TC06_ImplemAvail = 2, .after = "TC06_ChangeAvailRep") %>%  # 3 becomes unavailable
  add_column(TC07_ImplemAvail = 1000, .after = "TC07_ChangeAvailRep") %>% # no change
  add_column(TC08_ImplemAvail = 23, .after = "TC08_ChangeAvailRep") %>% # 2+3 become available
  add_column(TC09_ImplemAvail = 1000, .after = "TC09_ChangeAvailRep") %>% # no change
  add_column(TC10_ImplemAvail = 0, .after = "TC10_ChangeAvailRep") %>%  # 2+3 become unavailable
  add_column(TC11_ImplemAvail = 23, .after = "TC11_ChangeAvailRep") %>% # 2+3 become available
  add_column(TC12_ImplemAvail = 0, .after = "TC12_ChangeAvailRep")      # 2+3 become unavailable

all_merged_t99_replaced_aut_comp2_avail_comp <- all_merged_t99_replaced_aut_comp2_avail %>%
  add_column(TC01_AvailImplem_Rep = NA, .after = "TC01_ImplemAvail") %>%
  add_column(TC02_AvailImplem_Rep = NA, .after = "TC02_ImplemAvail") %>%
  add_column(TC03_AvailImplem_Rep = NA, .after = "TC03_ImplemAvail") %>%
  add_column(TC04_AvailImplem_Rep = NA, .after = "TC04_ImplemAvail") %>%
  add_column(TC05_AvailImplem_Rep = NA, .after = "TC05_ImplemAvail") %>%
  add_column(TC06_AvailImplem_Rep = NA, .after = "TC06_ImplemAvail") %>%
  add_column(TC07_AvailImplem_Rep = NA, .after = "TC07_ImplemAvail") %>%
  add_column(TC08_AvailImplem_Rep = NA, .after = "TC08_ImplemAvail") %>%
  add_column(TC09_AvailImplem_Rep = NA, .after = "TC09_ImplemAvail") %>%
  add_column(TC10_AvailImplem_Rep = NA, .after = "TC10_ImplemAvail") %>%
  add_column(TC11_AvailImplem_Rep = NA, .after = "TC11_ImplemAvail") %>%
  add_column(TC12_AvailImplem_Rep = NA, .after = "TC12_ImplemAvail") %>%
  mutate(TC01_AvailImplem_Rep = ifelse(TC01_ChangeAvailRep == TC01_ImplemAvail, 1,           # 1000 for TC01, TC03, TC04, TC05, TC07, TC09
                                       ifelse(TC01_ChangeAvailRep == 77, 1,                  # 77 for ""
                                              ifelse(TC01_ChangeAvailRep == 88, 1, 0)))) %>% # 88 for ""
  mutate(TC03_AvailImplem_Rep = ifelse(TC03_ChangeAvailRep == TC03_ImplemAvail, 1, 
                                       ifelse(TC03_ChangeAvailRep == 77, 1, 
                                              ifelse(TC03_ChangeAvailRep == 88, 1, 0)))) %>%
  mutate(TC04_AvailImplem_Rep = ifelse(TC04_ChangeAvailRep == TC04_ImplemAvail, 1, 
                                       ifelse(TC04_ChangeAvailRep == 77, 1, 
                                              ifelse(TC04_ChangeAvailRep == 88, 1, 0)))) %>%
  mutate(TC05_AvailImplem_Rep = ifelse(TC05_ChangeAvailRep == TC05_ImplemAvail, 1, 
                                       ifelse(TC05_ChangeAvailRep == 77, 1, 
                                              ifelse(TC05_ChangeAvailRep == 88, 1, 0)))) %>%
  mutate(TC07_AvailImplem_Rep = ifelse(TC07_ChangeAvailRep == TC07_ImplemAvail, 1, 
                                       ifelse(TC07_ChangeAvailRep == 77, 1, 
                                              ifelse(TC07_ChangeAvailRep == 88, 1, 0)))) %>%
  mutate(TC09_AvailImplem_Rep = ifelse(TC09_ChangeAvailRep == TC09_ImplemAvail, 1, 
                                       ifelse(TC09_ChangeAvailRep == 77, 1, 
                                              ifelse(TC09_ChangeAvailRep == 88, 1, 0)))) %>%
  mutate(TC02_AvailImplem_Rep = ifelse(TC02_ChangeAvailRep == TC02_ImplemAvail, 1, 0)) %>%   # 23 for TC02, TC08, TC11
  mutate(TC08_AvailImplem_Rep = ifelse(TC08_ChangeAvailRep == TC08_ImplemAvail, 1, 0)) %>%
  mutate(TC11_AvailImplem_Rep = ifelse(TC11_ChangeAvailRep == TC11_ImplemAvail, 1, 0)) %>%
  mutate(TC06_AvailImplem_Rep = ifelse(TC06_ChangeAvailRep == TC06_ImplemAvail, 1, 0)) %>%   # 2 for TC06
  mutate(TC10_AvailImplem_Rep = ifelse(TC10_ChangeAvailRep == TC10_ImplemAvail, 1,           # 0 for TC10, TC12
                                       ifelse(TC10_ChangeAvailRep == 66, 0.5, 0))) %>%       # 66=0.5 for "", kommt nicht vor
  mutate(TC12_AvailImplem_Rep = ifelse(TC12_ChangeAvailRep == TC12_ImplemAvail, 1, 
                                       ifelse(TC12_ChangeAvailRep == 66, 0.5, 0)))

# 7) create new column withreason for availability change and its comparison
## implemented vs reported: 1= correct; 0= incorrect (only 0 is correct, all others (1, 2, 77, 99, 1000) are not)
all_merged_t99_replaced_aut_comp2_avail_comp_all <- all_merged_t99_replaced_aut_comp2_avail_comp %>%
  add_column(TC06_AvailReasonCorrect = NA, .after = "TC06_ChangeAvailReasonRep") %>%
  add_column(TC10_AvailReasonCorrect = NA, .after = "TC10_ChangeAvailReasonRep") %>%
  add_column(TC12_AvailReasonCorrect = NA, .after = "TC12_ChangeAvailReasonRep") %>%
  mutate(TC06_AvailReasonCorrect = ifelse(TC06_ChangeAvailReasonRep == 0, 1, 0)) %>%
  mutate(TC10_AvailReasonCorrect = ifelse(TC10_ChangeAvailReasonRep == 0, 1, 0)) %>%
  mutate(TC12_AvailReasonCorrect = ifelse(TC12_ChangeAvailReasonRep == 0, 1, 0))

# 8) create new columns with comparison of 
# observed level vs hands-off allow & e-mails allow
# reported level vs hands-off allow & e-mails allow <-- not calculated (only refer to observed)

all_merged_t99_replaced_aut_comp2_avail_comp_all_allow1 <- all_merged_t99_replaced_aut_comp2_avail_comp_all %>%
  add_column(TC01_HandsOffAllow_Observed = NA, .after = "TC01_HandsOffAllow") %>%
  add_column(TC02_HandsOffAllow_Observed = NA, .after = "TC02_HandsOffAllow") %>%
  add_column(TC03_HandsOffAllow_Observed = NA, .after = "TC03_HandsOffAllow") %>%
  add_column(TC04_HandsOffAllow_Observed = NA, .after = "TC04_HandsOffAllow") %>%
  add_column(TC05_HandsOffAllow_Observed = NA, .after = "TC05_HandsOffAllow") %>%
  add_column(TC06_HandsOffAllow_Observed = NA, .after = "TC06_HandsOffAllow") %>%
  add_column(TC07_HandsOffAllow_Observed = NA, .after = "TC07_HandsOffAllow") %>%
  add_column(TC08_HandsOffAllow_Observed = NA, .after = "TC08_HandsOffAllow") %>%
  add_column(TC09_HandsOffAllow_Observed = NA, .after = "TC09_HandsOffAllow") %>%
  add_column(TC10_HandsOffAllow_Observed = NA, .after = "TC10_HandsOffAllow") %>%
  add_column(TC11_HandsOffAllow_Observed = NA, .after = "TC11_HandsOffAllow") %>%
  add_column(TC12_HandsOffAllow_Observed = NA, .after = "TC12_HandsOffAllow") %>%
  mutate(TC01_HandsOffAllow_Observed = ifelse((TC01_observedLevel == 3 & TC01_HandsOffAllow == 1) |
                                                (TC01_observedLevel != 3 & TC01_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC02_HandsOffAllow_Observed = ifelse((TC02_observedLevel == 3 & TC02_HandsOffAllow == 1) |
                                                (TC02_observedLevel != 3 & TC02_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC03_HandsOffAllow_Observed = ifelse((TC03_observedLevel == 3 & TC03_HandsOffAllow == 1) |
                                                (TC03_observedLevel != 3 & TC03_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC04_HandsOffAllow_Observed = ifelse((TC04_observedLevel == 3 & TC04_HandsOffAllow == 1) |
                                                (TC04_observedLevel != 3 & TC04_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC05_HandsOffAllow_Observed = ifelse((TC05_observedLevel == 3 & TC05_HandsOffAllow == 1) |
                                                (TC05_observedLevel != 3 & TC05_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC06_HandsOffAllow_Observed = ifelse((TC06_observedLevel == 3 & TC06_HandsOffAllow == 1) |
                                                (TC06_observedLevel != 3 & TC06_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC07_HandsOffAllow_Observed = ifelse((TC07_observedLevel == 3 & TC07_HandsOffAllow == 1) |
                                                (TC07_observedLevel != 3 & TC07_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC08_HandsOffAllow_Observed = ifelse((TC08_observedLevel == 3 & TC08_HandsOffAllow == 1) |
                                                (TC08_observedLevel != 3 & TC08_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC09_HandsOffAllow_Observed = ifelse((TC09_observedLevel == 3 & TC09_HandsOffAllow == 1) |
                                                (TC09_observedLevel != 3 & TC09_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC10_HandsOffAllow_Observed = ifelse((TC10_observedLevel == 3 & TC10_HandsOffAllow == 1) |
                                                (TC10_observedLevel != 3 & TC10_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC11_HandsOffAllow_Observed = ifelse((TC11_observedLevel == 3 & TC11_HandsOffAllow == 1) |
                                                (TC11_observedLevel != 3 & TC11_HandsOffAllow == 0), 1, 0)) %>%
  mutate(TC12_HandsOffAllow_Observed = ifelse((TC12_observedLevel == 3 & TC12_HandsOffAllow == 1) |
                                                (TC12_observedLevel != 3 & TC12_HandsOffAllow == 0), 1, 0))

all_compared <- all_merged_t99_replaced_aut_comp2_avail_comp_all_allow1 %>%
  add_column(TC01_EmailsAllow_Observed = NA, .after = "TC01_EmailsAllow") %>%
  add_column(TC02_EmailsAllow_Observed = NA, .after = "TC02_EmailsAllow") %>%
  add_column(TC03_EmailsAllow_Observed = NA, .after = "TC03_EmailsAllow") %>%
  add_column(TC04_EmailsAllow_Observed = NA, .after = "TC04_EmailsAllow") %>%
  add_column(TC05_EmailsAllow_Observed = NA, .after = "TC05_EmailsAllow") %>%
  add_column(TC06_EmailsAllow_Observed = NA, .after = "TC06_EmailsAllow") %>%
  add_column(TC07_EmailsAllow_Observed = NA, .after = "TC07_EmailsAllow") %>%
  add_column(TC08_EmailsAllow_Observed = NA, .after = "TC08_EmailsAllow") %>%
  add_column(TC09_EmailsAllow_Observed = NA, .after = "TC09_EmailsAllow") %>%
  add_column(TC10_EmailsAllow_Observed = NA, .after = "TC10_EmailsAllow") %>%
  add_column(TC11_EmailsAllow_Observed = NA, .after = "TC11_EmailsAllow") %>%
  add_column(TC12_EmailsAllow_Observed = NA, .after = "TC12_EmailsAllow") %>%
  mutate(TC01_EmailsAllow_Observed = ifelse((TC01_observedLevel == 3 & TC01_EmailsAllow == 1) |
                                              (TC01_observedLevel != 3 & TC01_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC02_EmailsAllow_Observed = ifelse((TC02_observedLevel == 3 & TC02_EmailsAllow == 1) |
                                              (TC02_observedLevel != 3 & TC02_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC03_EmailsAllow_Observed = ifelse((TC03_observedLevel == 3 & TC03_EmailsAllow == 1) |
                                              (TC03_observedLevel != 3 & TC03_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC04_EmailsAllow_Observed = ifelse((TC04_observedLevel == 3 & TC04_EmailsAllow == 1) |
                                              (TC04_observedLevel != 3 & TC04_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC05_EmailsAllow_Observed = ifelse((TC05_observedLevel == 3 & TC05_EmailsAllow == 1) |
                                              (TC05_observedLevel != 3 & TC05_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC06_EmailsAllow_Observed = ifelse((TC06_observedLevel == 3 & TC06_EmailsAllow == 1) |
                                              (TC06_observedLevel != 3 & TC06_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC07_EmailsAllow_Observed = ifelse((TC07_observedLevel == 3 & TC07_EmailsAllow == 1) |
                                              (TC07_observedLevel != 3 & TC07_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC08_EmailsAllow_Observed = ifelse((TC08_observedLevel == 3 & TC08_EmailsAllow == 1) |
                                              (TC08_observedLevel != 3 & TC08_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC09_EmailsAllow_Observed = ifelse((TC09_observedLevel == 3 & TC09_EmailsAllow == 1) |
                                              (TC09_observedLevel != 3 & TC09_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC10_EmailsAllow_Observed = ifelse((TC10_observedLevel == 3 & TC10_EmailsAllow == 1) |
                                              (TC10_observedLevel != 3 & TC10_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC11_EmailsAllow_Observed = ifelse((TC11_observedLevel == 3 & TC11_EmailsAllow == 1) |
                                              (TC11_observedLevel != 3 & TC11_EmailsAllow == 0), 1, 0)) %>%
  mutate(TC12_EmailsAllow_Observed = ifelse((TC12_observedLevel == 3 & TC12_EmailsAllow == 1) |
                                              (TC12_observedLevel != 3 & TC12_EmailsAllow == 0), 1, 0))

data_all <- all_compared

#### save data ####
write_excel_csv(data_all, "data/preprocessed/Lime+AutLvl_all.csv")
rm(list=setdiff(ls(), c("data_all")))

# when importing, mutate Exp and HMI
#  mutate(Exp = as.factor(Exp)) %>%
#  mutate(HMI = as.factor(HMI)) %>%
