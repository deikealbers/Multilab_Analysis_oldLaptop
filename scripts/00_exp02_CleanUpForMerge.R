#### script for processing data of exp01 ####
# copied from file Exp01_Exp02_Combine_20201209.R
# adjusted to have codes/numbers rather than string values - for easy comparison among the 3 exp.
# results in file "data/R_data_exp02.RData"

# check if changes in script affect section "# caution: depends on column position"

### preparations ###
rm(list = ls())
library(tidyverse)
setwd("~/R/Multilab")


#### import data ####
# Read in files
pre_tab <- read.csv("data/raw/Exp02_Pre_CompleteReplies.csv", encoding = "UTF-8")
mid_tab <- read.csv("data/raw/Exp02_Mid_CompleteReplies.csv", encoding = "UTF-8")
post_tab <- read.csv("data/raw/Exp02_Post_CompleteReplies.csv", encoding = "UTF-8")


#### clean up pre_tab ####
# delete and combine columns ! caution: columns selected by position
# assign correct data format incl. levels and rename unclear column names

# delete
pre_tab_n <- pre_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Zwischenfolie))  %>%
  unite("VPCode", VPCode.SQ001.: VPCode.SQ003.) 

# - VP7 is renamed to 57 (wrong assignment)
# - VP7 (now 57) is assigned to LC (wrong assignment to HC)
# - VP81 is assigned to LC (wrong assignment to HC)
pre_tab_n1 <- pre_tab_n %>%
  mutate(VPNr = ifelse(startdate == "2020-10-02 15:37:31" & VPNr == 7, 57, VPNr)) %>%
  mutate(HMI = ifelse(startdate == "2020-10-02 15:37:31" & HMI == "HC_HMI", "LC_HMI", HMI)) %>%
  mutate(HMI = ifelse(VPNr == "81" & HMI == "HC_HMI", "LC_HMI", HMI))

# assign correct data format incl. levels and rename unclear column names
# part of pre-questionnaire
# Sehhilfe: different data are collected for exp1+2 vs. exp3
pre_tab_n2 <- pre_tab_n1 %>%
  mutate(HMI = ifelse(HMI == 'HC_HMI', 'HC', 'LC')) %>%
  mutate(HMI = factor(HMI)) %>%
  mutate(Geschlecht = case_when( #in exp3 the option "other"=3 is introduced
    Geschlecht == 'Männlich' ~ 0,
    Geschlecht == 'Weiblich' ~ 1,
    Geschlecht == 'Divers' ~ 2,
    TRUE ~ as.numeric(Geschlecht))) %>% 
  mutate(Erfahrung = case_when(
    Erfahrung == 'Nie' ~ 0,
    Erfahrung == 'Weniger als einmal pro Monat' ~ 1,
    Erfahrung == 'Mehrmals pro Monat' ~ 2,
    Erfahrung == 'Mehrmals pro Woche' ~ 3,
    Erfahrung == 'Täglich' ~ 4,
    TRUE ~ as.numeric(Erfahrung))) %>%
  mutate(KilometerJahr = case_when(
    KilometerJahr == '< als 5.000 km' ~ 0,
    KilometerJahr == '5.001 - 10.000 km' ~ 1,
    KilometerJahr == '10.001 - 20.000 km' ~ 2,
    KilometerJahr == '> 20.000 km' ~ 3,
    TRUE ~ as.numeric(KilometerJahr))) %>%
  mutate(Sehhilfe = case_when(
    Sehhilfe == 'Nein' ~ 0,
    Sehhilfe == 'Ja, ich nutze sie auch jetzt' ~ 1,
    Sehhilfe == 'Ja, ich nutze sie jetzt nicht' ~ 2,
    TRUE ~ as.numeric(Sehhilfe))) %>%
  mutate("Farbfehlsichtigkeit" = ifelse(Farbfehlsichtigkeit == 'Ja', 1, 0)) %>%
  rename(Vorwissen = Vorwissen.SQ001.) %>%
  mutate(Vorwissen = case_when(
    Vorwissen == 'Keine Kenntnisse - 0' ~ 0,
    Vorwissen == '1' ~ 1,
    Vorwissen == '2' ~ 2,
    Vorwissen == '3' ~ 3,
    Vorwissen == 'Experte - 4' ~ 4,
    TRUE ~ as.numeric(Vorwissen))) %>%
  mutate(across(starts_with("FAS.SQ"), ~ifelse(. == 'Ja', 1, 0))) %>%  
  rename(FAS.CC = FAS.SQ001.)  %>%
  rename(FAS.ACC = FAS.SQ002.)  %>%
  rename(FAS.LKA = FAS.SQ003.)  %>%
  rename(FAS.NoFAS = FAS.SQ004.)  %>%
  mutate(across(starts_with("Oft"), ~case_when( #exp01 & exp02 miss "selten"=1"; introduced only in exp03 
    .== 'Nie' ~0,
    .== 'Monatlich' ~2,
    .== 'Wöchentlich' ~3,
    .== 'Täglich' ~4,
    .== 'Mehrmals täglich' ~5,
    TRUE ~ as.numeric(.))))
  

#### clean up mid_tab ####
# delete columns ! caution: columns selected by position
# assign correct data format incl. levels and rename unclear column names
mid_tab_n <- mid_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Datum))  %>%
  rename(VPNr = 'VP', mid_HMI = 'HMI', mid_startdate = 'startdate') %>%
  mutate(mid_HMI = ifelse(mid_HMI == 'HC_HMI', 'HC', 'LC')) %>%
  mutate(mid_HMI = factor(mid_HMI)) %>%
  mutate(Wetter = case_when( #exp01 misses Wetter & Licht 
    Wetter== 'Sonnig, blauer Himmel' ~0,
    Wetter== 'Leicht bewölkt' ~1,
    Wetter== 'Stark bewölkt' ~2,
    Wetter== 'Leichter Regen' ~3,
    TRUE ~ as.numeric(Wetter))) %>%
  mutate(Licht = case_when( #exp01 misses Wetter & Licht 
    Licht== 'Sehr hell, blendend' ~0,
    Licht== 'Hell' ~1,
    Licht== 'Düster, dämmrig' ~2,
    TRUE ~ as.numeric(Licht)))

# rename TC01-TC12; array from excel-File
newnames = c("mid_startdate", "VPNr", "mid_HMI", "Wetter", "Licht", "TC01_LevelSel", "TC01_LevelOthers", "TC01_ChangeAvail", "TC01_ChangeAvailSel", "TC01_ChangeAvailOthers", "TC01_HandsOffAllow", "TC01_EmailsAllow", "TC01_RemarksTP", "TC01_RemarksTL", "TC01_ER", "TC02_LevelSel", "TC02_LevelOthers", "TC02_ChangeAvail", "TC02_ChangeAvailSel", "TC02_ChangeAvailOthers", "TC02_HandsOffAllow", "TC02_EmailsAllow", "TC02_RemarksTP", "TC02_RemarksTL", "TC02_ER", "TC03_LevelSel", "TC03_LevelOthers", "TC03_ChangeAvail", "TC03_ChangeAvailSel", "TC03_ChangeAvailOthers", "TC03_HandsOffAllow", "TC03_EmailsAllow", "TC03_TransProblems", "TC03_TransProblemsOthers", "TC03_RemarksTP", "TC03_RemarksTL", "TC03_ER", "TC04_LevelSel", "TC04_LevelOthers", "TC04_ChangeAvail", "TC04_ChangeAvailSel", "TC04_ChangeAvailOthers", "TC04_HandsOffAllow", "TC04_EmailsAllow", "TC04_RemarksTP", "TC04_RemarksTL", "TC04_ER", "TC05_LevelSel", "TC05_LevelOthers", "TC05_ChangeAvail", "TC05_ChangeAvailSel", "TC05_ChangeAvailOthers", "TC05_HandsOffAllow", "TC05_EmailsAllow", "TC05_TransProblems", "TC05_TransProblemsOthers", "TC05_RemarksTP", "TC05_RemarksTL", "TC05_ER", "TC06_LevelSel", "TC06_LevelOthers", "TC06_ChangeAvail", "TC06_ChangeAvailSel", "TC06_ChangeAvailOthers", "TC06_ChangeAvailReasonSel", "TC06_ChangeAvailReasonOthers", "TC06_HandsOffAllow", "TC06_EmailsAllow", "TC06_RemarksTP", "TC06_RemarksTL", "TC06_ER", "TC07_LevelSel", "TC07_LevelOthers", "TC07_ChangeAvail", "TC07_ChangeAvailSel", "TC07_ChangeAvailOthers", "TC07_HandsOffAllow", "TC07_EmailsAllow", "TC07_RemarksTP", "TC07_RemarksTL", "TC07_ER", "TC08_LevelSel", "TC08_LevelOthers", "TC08_ChangeAvail", "TC08_ChangeAvailSel", "TC08_ChangeAvailOthers", "TC08_HandsOffAllow", "TC08_EmailsAllow", "TC08_RemarksTP", "TC08_RemarksTL", "TC08_ER", "TC09_LevelSel", "TC09_LevelOthers", "TC09_ChangeAvail", "TC09_ChangeAvailSel", "TC09_ChangeAvailOthers", "TC09_HandsOffAllow", "TC09_EmailsAllow", "TC09_TransProblems", "TC09_TransProblemsOthers", "TC09_RemarksTP", "TC09_RemarksTL", "TC09_ER", "TC10_LevelSel", "TC10_LevelOthers", "TC10_ChangeAvail", "TC10_ChangeAvailSel", "TC10_ChangeAvailOthers", "TC10_ChangeAvailReasonSel", "TC10_ChangeAvailReasonOthers", "TC10_HandsOffAllow", "TC10_EmailsAllow", "TC10_TransProblems", "TC10_TransProblemsOthers", "TC10_RemarksTP", "TC10_RemarksTL", "TC10_ER", "TC11_LevelSel", "TC11_LevelOthers", "TC11_ChangeAvail", "TC11_ChangeAvailSel", "TC11_ChangeAvailOthers", "TC11_HandsOffAllow", "TC11_EmailsAllow", "TC11_TransProblems", "TC11_TransProblemsOthers", "TC11_RemarksTP", "TC11_RemarksTL", "TC11_ER", "TC12_LevelSel", "TC12_LevelOthers", "TC12_ChangeAvail", "TC12_ChangeAvailSel", "TC12_ChangeAvailOthers", "TC12_ChangeAvailReasonSel", "TC12_ChangeAvailReasonOthers", "TC12_HandsOffAllow", "TC12_EmailsAllow", "TC12_TransProblems", "TC12_TransProblemsOthers", "TC12_RemarksTP", "TC12_RemarksTL", "TC12_ER")
mid_tab_n1 <- mid_tab_n
colnames(mid_tab_n1) <- newnames

# assign correct data format for mid questionnaire
mid_tab_n2 <- mid_tab_n1 %>%
  mutate(across(ends_with("LevelSel"), ~case_when( #"sonstiges" = 99 
    .== 'Manuelles Fahren' ~0,
    .== 'Keine Automationsstufe' ~1,
    .== 'Assistiertes Fahren' ~2,
    .== 'Mittlere Stufe' ~3,
    .== 'Automatisiertes Fahren' ~4,
    .== 'Höchste Stufe' ~5,
    .== 'Sonstiges' ~ 99,
    TRUE ~ as.numeric(.))))%>%
  mutate(across(ends_with("ChangeAvail"), ~ifelse(. == 'Ja', 1, 0))) %>% 
  mutate(across(ends_with("ChangeAvailSel"), ~case_when( #"sonstiges" = 99 
    .== 'Alle verfügbar' ~0,
    .== 'Assistiert und Automatisiertes Fahren verfügbar' ~1,
    .== 'Assistiert und Automatisiert verfügbar' ~1, # different wording in TC01
    .== 'Automatisiert verfügbar' ~2,
    .== 'Assistiert verfügbar' ~3,
    .== 'Keines verfügbar' ~4,
    .== 'Automatisiert nicht mehr verfügbar' ~5,
    .== 'Automatisiert und Assistiert nicht mehr verfügbar' ~6,
    .== 'Keines mehr verfügbar' ~7,
    .== 'Nur Manuell verfügbar' ~8,
    .== 'Sonstiges' ~ 99,
    TRUE ~ as.numeric(.)))) %>%
  mutate(across(ends_with("ChangeAvailReasonSel"), ~case_when( #"sonstiges" = 99 , bei TC10 wird Systemgrenze als 0 codiert; bei TC6 und TC12 ist es Sensorfehler
    .== 'Systemgrenze' ~0,
    .== 'Sensorfehler' ~0,    
    .== 'Weiß nicht mehr' ~1,
    .== 'Nichts erkannt' ~2,
    .== 'Sonstiges' ~ 99,
    TRUE ~ as.numeric(.)))) %>%
  mutate(across(ends_with("HandsOffAllow"), ~ifelse(. == 'Ja', 1, 0))) %>% 
  mutate(across(ends_with("EmailsAllow"), ~ifelse(. == 'Ja', 1, 0))) %>%  
  mutate(across(ends_with("TransProblems"), ~ifelse(. == 'Ja', 1, 0))) %>%  
  mutate(across(ends_with("_ER"), ~case_when(
    . == '1 - Keine Probleme; Schnelles Verarbeiten' ~1,
    . == '2 - Zögern; Unabhängige Lösung ohne Fehler Aber: Zögern, sehr bewusstes Handeln und volle Konzentration' ~2,
    . == '3 - Kleinere Fehler; Unabhängige Lösung ohne oder mit kleineren Fehlern, die selbstbewusst korrigiert Aber: Längere Denkpausen Bewertung möglicher Arbeitsschritte' ~3,
    . == '4 - Massive Fehler; Ein oder mehrere Fehler Deutlich beeinträchtigter Operationsablauf Übermäßige Korrektur von Fehlern Keine Hilfe des Experimentators erforderlich' ~4,
    . == '5 - Hilfe vom Experimenter; Mehrere Fehler Massive Fehler erfordern einen Neustart der Aufgabe, Hilfe des Experimentators erforderlich' ~5,
    TRUE ~ as.numeric(.))))

#### clean up post_tab ####

# delete columns ! caution: columns selected by position
post_tab_n <- post_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Datum))  %>%
  rename(post_HMI = 'HMI', post_startdate = 'startdate') 

# - VP34 is renamed to 35 (wrong assignment)
# - VP81 is assigned to LC (wrong assignment to LC)
post_tab_n1 <- post_tab_n %>%
  mutate(VPNr = ifelse(post_startdate == "2020-10-27 14:32:35" & VPNr == 34, 35, VPNr)) %>%
  mutate(post_HMI = ifelse(VPNr == "13" & post_HMI == 'LC_HMI', 'HC_HMI', post_HMI))

# assign correct data format incl. levels and rename unclear column names
post_tab_n2 <- post_tab_n1 %>%
  mutate(post_HMI = ifelse(post_HMI == 'HC_HMI', 'HC', 'LC')) %>%
  mutate(post_HMI = factor(post_HMI)) %>%
  mutate(across(starts_with("UsabilityMetric"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(starts_with("UsabilityMetric"), ~ifelse(. == 'Stimme voll zu - 7', 7, .))) %>%
  mutate(across(starts_with("SUS"), ~ifelse(. == 'Stimme voll zu - 5', 5, .))) %>%
  mutate(across(starts_with("SUS"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(starts_with("TuA"), ~ifelse(. == 'Stimme voll zu - 5', 5, .))) %>%
  mutate(across(starts_with("TuA"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(UsabilityMetric.SQ001.:UEQ.SQ026., ~as.numeric(.))) %>%
  mutate(across(AnstrengendWenden.SQ001.:Uebel.SQ001., ~as.numeric(.)))

# change variable names
UMUX_names = c("UMUX01", "UMUX02", "UMUX03", "UMUX04")
SUS_names = c("SUS01", "SUS02", "SUS03", "SUS04", "SUS05", "SUS06", "SUS07", "SUS08", "SUS09", "SUS10")
UEQ_names = c("UEQ01", "UEQ02", "UEQ03", "UEQ04", "UEQ05", "UEQ06", "UEQ07", "UEQ08", "UEQ09", "UEQ10", "UEQ11", "UEQ12", "UEQ13", "UEQ14", "UEQ15", "UEQ16", "UEQ17", "UEQ18", "UEQ19", "UEQ20", "UEQ21", "UEQ22", "UEQ23", "UEQ24", "UEQ25", "UEQ26") 

post_tab_n3 <- post_tab_n2 %>%
  rename(Trust = TuA.SQ001.) %>%
  rename(Acceptance = TuA.SQ002.) %>%
  rename(AnstrengendWenden = AnstrengendWenden.SQ001.) %>%
  rename(Uebel = Uebel.SQ001.)

# caution: depends on column position
colnames(post_tab_n3) [4:7] <- UMUX_names
colnames(post_tab_n3) [8:17] <- SUS_names
colnames(post_tab_n3) [20:45] <- UEQ_names

rm(list=setdiff(ls(), c("pre_tab_n2", "mid_tab_n2", "post_tab_n3")))

#### join data ####
# join data
lime_tab <- left_join(pre_tab_n2, mid_tab_n2, by = "VPNr")
lime_tab_n <- left_join(lime_tab, post_tab_n3, by = "VPNr")
lime_tab_n1 <- lime_tab_n %>%
  relocate(mid_startdate, .after = startdate) %>%
  relocate(post_startdate, .after = mid_startdate) %>%
  relocate(mid_HMI, .after = HMI) %>%
  relocate(post_HMI, .after = mid_HMI)


# delete pilot tests (1, 1, 2, 3, 50-52) and excluded data sets (aborted test runs; 12, 55, 68)
# delete doubled columns and add column for experiment information
data_exp02 <- lime_tab_n1[!(lime_tab_n1$VPNr == 1 | lime_tab_n1$VPNr == 2 | lime_tab_n1$VPNr == 3 | lime_tab_n1$VPNr == 50 | lime_tab_n1$VPNr == 51 | lime_tab_n1$VPNr == 52 | lime_tab_n1$VPNr == 12 | lime_tab_n1$VPNr == 55 | lime_tab_n1$VPNr == 68),]
data_exp02 <- data_exp02 %>%
  select(-c(mid_startdate, post_startdate, mid_HMI, post_HMI))

# modify for merge
data_exp02 <- data_exp02 %>%
  select(-c(Datum)) %>%
  add_column(Exp = 2, .before ="HMI")

#### save data #### 
write_excel_csv(data_exp02, "data/R_data_exp02.csv")
save(data_exp02, file = "data/R_data_exp02.RData")
rm(list=setdiff(ls(), c("data_exp02")))