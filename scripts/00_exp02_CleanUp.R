#### script for processing data of exp02 ####
# copied from file Exp01_Exp02_Combine_20201209.R
# results in file "data/R_data_exp02.RData"

### preparations ###
library(tidyverse)
setwd("~/R/Multilab")

#### import data ####
# Read in files
pre_tab <- read.csv("data/raw/Exp02_Pre_CompleteReplies.csv", encoding = "UTF-8")
mid_tab <- read.csv("data/raw/Exp02_Mid_CompleteReplies.csv", encoding = "UTF-8")
post_tab <- read.csv("data/raw/Exp02_Post_CompleteReplies.csv", encoding = "UTF-8")


#### pre_tab ####
# delete and combine columns ! caution: columns selected by position
# assign correct data format incl. levels and rename unclear column names
# find factor levels with function unique -> unique(pre_tab$HMI)+
pre_tab_n <- pre_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Zwischenfolie))  %>%
  unite("VPCode", VPCode.SQ001.: VPCode.SQ003.) %>%
  mutate(Geschlecht = factor(Geschlecht)) %>%
  mutate(Erfahrung = factor(Erfahrung, levels = c("Nie", "Weniger als einmal pro Monat", "Mehrmals pro Monat", "Mehrmals pro Woche", "Täglich"), ordered = TRUE)) %>%
  mutate(KilometerJahr = factor(KilometerJahr, levels = c("< als 5.000 km", "5.001 - 10.000 km", "10.001 - 20.000 km", "> 20.000 km"), ordered = TRUE)) %>%
  mutate(Sehhilfe = factor(Sehhilfe)) %>%
  mutate(Farbfehlsichtigkeit = factor(Farbfehlsichtigkeit)) %>%
  mutate(Vorwissen.SQ001. = ifelse(Vorwissen.SQ001. == 'Keine Kenntnisse - 0', 0, Vorwissen.SQ001.)) %>%
  mutate(Vorwissen.SQ001. = ifelse(Vorwissen.SQ001. == 'Experte - 4', 4, Vorwissen.SQ001.)) %>%  
  mutate(Vorwissen.SQ001. = as.numeric(Vorwissen.SQ001.)) %>%
  mutate(across(starts_with("FAS.SQ"), ~factor(.))) %>%
  rename(FAS.CC = FAS.SQ001.)  %>%
  rename(FAS.ACC = FAS.SQ002.)  %>%
  rename(FAS.LKA = FAS.SQ003.)  %>%
  rename(FAS.NoFAS = FAS.SQ004.)  %>%
  mutate(across(starts_with("Oft"), ~factor(., levels = c("", "Nie", "Monatlich", "Wöchentlich", "Täglich", "Mehrmals täglich"), ordered = TRUE)))

# wrong TP and group assignment of TP57, wrong group assignment of TP81
pre_tab_nc <- pre_tab_n %>%
  mutate(VPNr = ifelse(startdate == "2020-10-02 15:37:31" & VPNr == 7, 57, VPNr)) %>%
  mutate(HMI = ifelse(startdate == "2020-10-02 15:37:31" & HMI == 'HC_HMI', 'LC_HMI', HMI)) %>%
  mutate(HMI = ifelse(VPNr == "81" & HMI == 'HC_HMI', 'LC_HMI', HMI)) %>%
  mutate(HMI = factor(HMI))


#### mid_tab ####
# delete columns ! caution: columns selected by position
# assign correct data format incl. levels and rename unclear column names
mid_tab_n <- mid_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Datum))  %>%
  rename(VPNr = 'VP', mid_HMI = 'HMI', mid_startdate = 'startdate') %>%  
  mutate(Wetter = factor(Wetter, levels = c("Sonnig, blauer Himmel", "Leicht bewölkt", "Stark bewölkt",  "Leichter Regen"), ordered = TRUE)) %>%  
  mutate(Licht = factor(Licht, levels = c("Sehr hell, blendend", "Hell", "Düster, dämmrig")))

# rename TC01-TC12; array from excel-File
newnames = c("mid_startdate", "VPNr", "mid_HMI", "Wetter", "Licht", "TC01_LevelSel", "TC01_LevelOthers", "TC01_ChangeAvail", "TC01_ChangeAvailSel", "TC01_ChangeAvailOthers", "TC01_HandsOffAllow", "TC01_EmailsAllow", "TC01_RemarksTP", "TC01_RemarksTL", "TC01_ER", "TC02_LevelSel", "TC02_LevelOthers", "TC02_ChangeAvail", "TC02_ChangeAvailSel", "TC02_ChangeAvailOthers", "TC02_HandsOffAllow", "TC02_EmailsAllow", "TC02_RemarksTP", "TC02_RemarksTL", "TC02_ER", "TC03_LevelSel", "TC03_LevelOthers", "TC03_ChangeAvail", "TC03_ChangeAvailSel", "TC03_ChangeAvailOthers", "TC03_HandsOffAllow", "TC03_EmailsAllow", "TC03_TransProblems", "TC03_TransProblemsOthers", "TC03_RemarksTP", "TC03_RemarksTL", "TC03_ER", "TC04_LevelSel", "TC04_LevelOthers", "TC04_ChangeAvail", "TC04_ChangeAvailSel", "TC04_ChangeAvailOthers", "TC04_HandsOffAllow", "TC04_EmailsAllow", "TC04_RemarksTP", "TC04_RemarksTL", "TC04_ER", "TC05_LevelSel", "TC05_LevelOthers", "TC05_ChangeAvail", "TC05_ChangeAvailSel", "TC05_ChangeAvailOthers", "TC05_HandsOffAllow", "TC05_EmailsAllow", "TC05_TransProblems", "TC05_TransProblemsOthers", "TC05_RemarksTP", "TC05_RemarksTL", "TC05_ER", "TC06_LevelSel", "TC06_LevelOthers", "TC06_ChangeAvail", "TC06_ChangeAvailSel", "TC06_ChangeAvailOthers", "TC06_ChangeAvailReasonSel", "TC06_ChangeAvailReasonOthers", "TC06_HandsOffAllow", "TC06_EmailsAllow", "TC06_RemarksTP", "TC06_RemarksTL", "TC06_ER", "TC07_LevelSel", "TC07_LevelOthers", "TC07_ChangeAvail", "TC07_ChangeAvailSel", "TC07_ChangeAvailOthers", "TC07_HandsOffAllow", "TC07_EmailsAllow", "TC07_RemarksTP", "TC07_RemarksTL", "TC07_ER", "TC08_LevelSel", "TC08_LevelOthers", "TC08_ChangeAvail", "TC08_ChangeAvailSel", "TC08_ChangeAvailOthers", "TC08_HandsOffAllow", "TC08_EmailsAllow", "TC08_RemarksTP", "TC08_RemarksTL", "TC08_ER", "TC09_LevelSel", "TC09_LevelOthers", "TC09_ChangeAvail", "TC09_ChangeAvailSel", "TC09_ChangeAvailOthers", "TC09_HandsOffAllow", "TC09_EmailsAllow", "TC09_TransProblems", "TC09_TransProblemsOthers", "TC09_RemarksTP", "TC09_RemarksTL", "TC09_ER", "TC10_LevelSel", "TC10_LevelOthers", "TC10_ChangeAvail", "TC10_ChangeAvailSel", "TC10_ChangeAvailOthers", "TC10_ChangeAvailReasonSel", "TC10_ChangeAvailReasonOthers", "TC10_HandsOffAllow", "TC10_EmailsAllow", "TC10_TransProblems", "TC10_TransProblemsOthers", "TC10_RemarksTP", "TC10_RemarksTL", "TC10_ER", "TC11_LevelSel", "TC11_LevelOthers", "TC11_ChangeAvail", "TC11_ChangeAvailSel", "TC11_ChangeAvailOthers", "TC11_HandsOffAllow", "TC11_EmailsAllow", "TC11_TransProblems", "TC11_TransProblemsOthers", "TC11_RemarksTP", "TC11_RemarksTL", "TC11_ER", "TC12_LevelSel", "TC12_LevelOthers", "TC12_ChangeAvail", "TC12_ChangeAvailSel", "TC12_ChangeAvailOthers", "TC12_ChangeAvailReasonSel", "TC12_ChangeAvailReasonOthers", "TC12_HandsOffAllow", "TC12_EmailsAllow", "TC12_TransProblems", "TC12_TransProblemsOthers", "TC12_RemarksTP", "TC12_RemarksTL", "TC12_ER")
colnames(mid_tab_n) <- newnames

# assign correct data format
mid_tab_n <- mid_tab_n %>%
  mutate(across(ends_with("LevelSel"), ~factor(.))) %>%
  mutate(across(ends_with("ChangeAvail"), ~factor(.))) %>%
  mutate(across(ends_with("ChangeAvailSel"), ~factor(.))) %>%
  mutate(across(ends_with("ChangeAvailReasonSel"), ~factor(.))) %>%
  mutate(across(ends_with("HandsOffAllow"), ~factor(.))) %>%
  mutate(across(ends_with("EmailsAllow"), ~factor(.))) %>%
  mutate(across(ends_with("TransProblems"), ~factor(.)))

mid_tab_n <- mid_tab_n %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == '1 - Keine Probleme; Schnelles Verarbeiten', 1, .))) %>%    
  mutate(across(ends_with("_ER"), ~ifelse(. == '2 - Zögern; Unabhängige Lösung ohne Fehler Aber: Zögern, sehr bewusstes Handeln und volle Konzentration', 2, .))) %>%    
  mutate(across(ends_with("_ER"), ~ifelse(. == '3 - Kleinere Fehler; Unabhängige Lösung ohne oder mit kleineren Fehlern, die selbstbewusst korrigiert Aber: Längere Denkpausen Bewertung möglicher Arbeitsschritte', 3, .))) %>%    
  mutate(across(ends_with("_ER"), ~ifelse(. == '4 - Massive Fehler; Ein oder mehrere Fehler Deutlich beeinträchtigter Operationsablauf Übermäßige Korrektur von Fehlern Keine Hilfe des Experimentators erforderlich', 4, .))) %>%    
  mutate(across(ends_with("_ER"), ~ifelse(. == '5 - Hilfe vom Experimenter; Mehrere Fehler Massive Fehler erfordern einen Neustart der Aufgabe, Hilfe des Experimentators erforderlich', 5, .))) %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == '5 - Hilfe vom Experimenter; Mehrere Fehler Massive Fehler erfordern einen Neustart der Aufgabe, Hilfe des Experimentators erforderlich;', 5, .))) %>%
  mutate(across(ends_with("_ER"), ~as.numeric(.)))  



#### post_tab ####
# delete columns ! caution: columns selected by position
# assign correct data format incl. levels and rename unclear column names
post_tab_n <- post_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Datum))  %>%
  rename(post_HMI = 'HMI', post_startdate = 'startdate') %>%
  mutate(across(starts_with("UsabilityMetric"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(starts_with("UsabilityMetric"), ~ifelse(. == 'Stimme voll zu - 7', 7, .))) %>%
  mutate(across(starts_with("SUS"), ~ifelse(. == 'Stimme voll zu - 5', 5, .))) %>%
  mutate(across(starts_with("SUS"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(starts_with("TuA"), ~ifelse(. == 'Stimme voll zu - 5', 5, .))) %>%
  mutate(across(starts_with("TuA"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(UsabilityMetric.SQ001.:UEQ.SQ026., ~as.numeric(.))) %>%
  mutate(across(AnstrengendWenden.SQ001.:Uebel.SQ001., ~as.numeric(.)))

# wrong assignment of test person 35; wrong group assignment of 13
post_tab_nc <- post_tab_n %>%
  mutate(VPNr = ifelse(post_startdate == "2020-10-27 14:32:35" & VPNr == 34, 35, VPNr)) %>%
  mutate(post_HMI = ifelse(VPNr == "13" & post_HMI == 'LC_HMI', 'HC_HMI', post_HMI))

#### join data ####
# join data
lime_tab <- left_join(pre_tab_nc, mid_tab_n, by = "VPNr")
lime_tab <- left_join(lime_tab, post_tab_nc, by = "VPNr")
lime_tab_n <- lime_tab %>%
  relocate(mid_startdate, .after = startdate) %>%
  relocate(post_startdate, .after = mid_startdate) %>%
  relocate(mid_HMI, .after = HMI) %>%
  relocate(post_HMI, .after = mid_HMI)


# delete pilot tests (1, 1, 2, 3, 50-52) and excluded data sets (aborted test runs; 12, 55, 68)
# delete doubled columns and add column for experiment information
data_exp02 <- lime_tab_n[!(lime_tab_n$VPNr == 1 | lime_tab_n$VPNr == 2 | lime_tab_n$VPNr == 3 | lime_tab_n$VPNr == 50 | lime_tab_n$VPNr == 51 | lime_tab_n$VPNr == 52 | lime_tab_n$VPNr == 12 | lime_tab_n$VPNr == 55 | lime_tab_n$VPNr == 68),]
data_exp02 <- data_exp02 %>%
  select(-c(mid_startdate, post_startdate, mid_HMI, post_HMI)) %>%
  add_column(Exp = "Exp02: Test Track", .after ="HMI")


#### save data ####
write_excel_csv(data_exp02, "data/R_data_exp02.csv")
# save(data_exp02, file = "data/R_data_exp02.RData")
rm(list=setdiff(ls(), c("data_exp02")))
