#### script for processing data of exp01 ####
# copied from file Exp01_Exp02_Combine_20201209.R
# results in file "data/R_data_exp01.RData"

### preparations ###
library(tidyverse)
setwd("~/R/Multilab")


##### import data ####
# Read in files
lc_tab <- read.csv("data/raw/Exp01_LC_HMI_CompleteReplies_rm64,add58.csv", encoding = "UTF-8")
hc_tab <- read.csv("data/raw/Exp01_HC_HMI_CompleteReplies.csv", encoding = "UTF-8")


#### adjust colnames ####
newnames = c("X.U.FEFF.id","submitdate","lastpage","startlanguage","seed","startdate","datestamp","VP","Datum","PKZ.SQ001.","PKZ.SQ002.","PKZ.SQ003.","Alter","Geschlecht","Erfahrung","KilometerJahr","Sehhilfe","Farbfehlsichtigkeit","Farbfehlsichtigkeit2","Vorwissen.SQ001.","FAS.SQ001.","FAS.SQ002.","FAS.SQ003.","FAS.SQ004.","OftCC","AutomarkeCC","OftACC","AutomarkeACC","OftLKA","AutomarkeLKA","ZWF01","ZWF02","TC01_LevelSel","TC01_LevelOthers","TC01_ChangeAvail","TC01_ChangeAvailSel","TC01_ChangeAvailOthers","TC01_HandsOffAllow","TC01_EmailsAllow","TC01_RemarksTP","TC01_RemarksTL","TC01_ER","ZWF03","TC02_LevelSel","TC02_LevelOthers","TC02_ChangeAvail","TC02_ChangeAvailSel","TC02_ChangeAvailOthers","TC02_HandsOffAllow","TC02_EmailsAllow","TC02_RemarksTP","TC02_RemarksTL","TC02_ER","ZWF04","TC03_LevelSel","TC03_LevelOthers","TC03_ChangeAvail","TC03_ChangeAvailSel","TC03_ChangeAvailOthers","TC03_HandsOffAllow","TC03_EmailsAllow","TC03_TransProblems","TC03_TransProblemsOthers","TC03_RemarksTP","TC03_RemarksTL","TC03_ER","ZWF05","TC04_LevelSel","TC04_LevelOthers","TC04_ChangeAvail","TC04_ChangeAvailSel","TC04_ChangeAvailOthers","TC04_HandsOffAllow","TC04_EmailsAllow","TC04_RemarksTP","TC04_RemarksTL","TC04_ER","ZWF06","TC05_LevelSel","TC05_LevelOthers","TC05_ChangeAvail","TC05_ChangeAvailSel","TC05_ChangeAvailOthers","TC05_HandsOffAllow","TC05_EmailsAllow","TC05_TransProblems","TC05_TransProblemsOthers","TC05_RemarksTP","TC05_RemarksTL","TC05_ER","ZWF7","TC06_LevelSel","TC06_LevelOthers","TC06_ChangeAvail","TC06_ChangeAvailSel","TC06_ChangeAvailOthers","TC06_ChangeAvailReasonSel","TC06_ChangeAvailReasonOthers","TC06_HandsOffAllow","TC06_EmailsAllow","TC06_RemarksTP","TC06_RemarksTL","TC06_ER","ZWF08","TC07_LevelSel","TC07_LevelOthers","TC07_ChangeAvail","TC07_ChangeAvailSel","TC07_ChangeAvailOthers","TC07_HandsOffAllow","TC07_EmailsAllow","TC07_RemarksTP","TC07_RemarksTL","TC07_ER","ZWF09","TC08_LevelSel","TC08_LevelOthers","TC08_ChangeAvail","TC08_ChangeAvailSel","TC08_ChangeAvailOthers","TC08_HandsOffAllow","TC08_EmailsAllow","TC08_RemarksTP","TC08_RemarksTL","TC08_ER","ZWF10","TC09_LevelSel","TC09_LevelOthers","TC09_ChangeAvail","TC09_ChangeAvailSel","TC09_ChangeAvailOthers","TC09_HandsOffAllow","TC09_EmailsAllow","TC09_TransProblems","TC09_TransProblemsOthers","TC09_RemarksTP","TC09_RemarksTL","TC09_ER","ZWF11","TC10_LevelSel","TC10_LevelOthers","TC10_ChangeAvail","TC10_ChangeAvailSel","TC10_ChangeAvailOthers","TC10_ChangeAvailReasonSel","TC10_ChangeAvailReasonOthers","TC10_HandsOffAllow","TC10_EmailsAllow","TC10_TransProblems","TC10_TransProblemsOthers","TC10_RemarksTP","TC10_RemarksTL","TC10_ER","ZWF12","TC11_LevelSel","TC11_LevelOthers","TC11_ChangeAvail","TC11_ChangeAvailSel","TC11_ChangeAvailOthers","TC11_HandsOffAllow","TC11_EmailsAllow","TC11_TransProblems","TC11_TransProblemsOthers","TC11_RemarksTP","TC11_RemarksTL","TC11_ER","ZWF13","TC12_LevelSel","TC12_LevelOthers","TC12_ChangeAvail","TC12_ChangeAvailSel","TC12_ChangeAvailOthers","TC12_ChangeAvailReasonSel","TC12_ChangeAvailReasonOthers","TC12_HandsOffAllow","TC12_EmailsAllow","TC12_TransProblems","TC12_TransProblemsOthers","TC12_RemarksTP","TC12_RemarksTL","TC12_ER","ZWF14","UsabilityMetric.SQ001.","UsabilityMetric.SQ002.","UsabilityMetric.SQ003.","UsabilityMetric.SQ004.","SUS1.SQ001.","SUS1.SQ002.","SUS1.SQ003.","SUS1.SQ004.","SUS1.SQ005.","SUS1.SQ006.","SUS1.SQ007.","SUS1.SQ008.","SUS1.SQ009.","SUS1.SQ010.","TuA.SQ001.","TuA.SQ002.","UEQ.SQ001.","UEQ.SQ002.","UEQ.SQ003.","UEQ.SQ004.","UEQ.SQ005.","UEQ.SQ006.","UEQ.SQ007.","UEQ.SQ008.","UEQ.SQ009.","UEQ.SQ010.","UEQ.SQ011.","UEQ.SQ012.","UEQ.SQ013.","UEQ.SQ014.","UEQ.SQ015.","UEQ.SQ016.","UEQ.SQ017.","UEQ.SQ018.","UEQ.SQ019.","UEQ.SQ020.","UEQ.SQ021.","UEQ.SQ022.","UEQ.SQ023.","UEQ.SQ024.","UEQ.SQ025.","UEQ.SQ026.","GutGefallen","NichtGefallen","Verbessern","Anmerkungen","AnstrengendWenden.SQ001.","Uebel.SQ001.","AllgAnregungen")
colnames(lc_tab) <- newnames
colnames(hc_tab) <- newnames

#### combine tables #####
# add column indicating HMI
lc_tab_n <- add_column(lc_tab, HMI = "LC_HMI", .after = "VP")
hc_tab_n <- add_column(hc_tab, HMI = "HC_HMI", .after = "VP")

# change TP number VV52 to 76 and VV53 to 77 and treat as numbers (combining otherwise not possible)
lc_tab_nc <- lc_tab_n %>%
  mutate(VP = ifelse(VP == "VV52", 76, VP)) %>%
  mutate(VP = ifelse(VP == "VV53", 77, VP)) %>%
  mutate(VP = as.integer(VP))

# treat code as characters (combining otherwise not possible)
hc_tab_nc <- hc_tab_n %>%
  mutate(PKZ.SQ003. = as.character(PKZ.SQ003.))

# combine
lime_tab <- bind_rows(lc_tab_nc, hc_tab_nc)

#### clean up ####
# delete and combine column
# assign correct data format incl. levels and rename unclear column names
# find factor levels with function unique -> unique(pre_tab$HMI)+

# delete 
lime_tab_n <- lime_tab %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp)) %>%
  select(-starts_with("ZWF")) %>%
  unite("VPCode", PKZ.SQ001.: PKZ.SQ003.) 

# assign correct data format incl. levels and rename unclear column names
lime_tab_n1 <- lime_tab_n %>%
  rename(VPNr = VP) %>%
  mutate(HMI = factor(HMI)) %>%
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

# assign correct data format for mid questionnaire
lime_tab_n2 <- lime_tab_n1 %>%
  mutate(across(ends_with("LevelSel"), ~factor(.))) %>%
  mutate(across(ends_with("ChangeAvail"), ~factor(.))) %>%
  mutate(across(ends_with("ChangeAvailSel"), ~factor(.))) %>%
  mutate(across(ends_with("ChangeAvailReasonSel"), ~factor(.))) %>%
  mutate(across(ends_with("HandsOffAllow"), ~factor(.))) %>%
  mutate(across(ends_with("EmailsAllow"), ~factor(.))) %>%
  mutate(across(ends_with("TransProblems"), ~factor(.))) %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == 'Keine Probleme; Schnelles Verarbeiten;1', 1, .))) %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == 'Zögern; Unabhängige Lösung ohne Fehler Aber: Zögern, sehr bewusstes Handeln und volle Konzentration; 2', 2, .))) %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == 'Kleinere Fehler; Unabhängige Lösung ohne oder mit kleineren Fehlern, die selbstbewusst korrigiert Aber: Längere Denkpausen Bewertung möglicher Arbeitsschritte; 3', 3, .))) %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == 'Massive Fehler; Ein oder mehrere Fehler Deutlich beeinträchtigter Operationsablauf Übermäßige Korrektur von Fehlern Keine Hilfe des Experimentators erforderlich; 4', 4, .))) %>%
  mutate(across(ends_with("_ER"), ~ifelse(. == 'Hilfe vom Experimenter; Mehrere Fehler Massive Fehler erfordern einen Neustart der Aufgabe, Hilfe des Experimentators erforderlich; 5', 5, .))) %>%
  mutate(across(ends_with("_ER"), ~as.numeric(.)))

# assign correct data format for post questionnaire
lime_tab_n3 <- lime_tab_n2 %>%
  mutate(across(starts_with("UsabilityMetric"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(starts_with("UsabilityMetric"), ~ifelse(. == 'Stimme voll zu - 7', 7, .))) %>%
  mutate(across(starts_with("SUS"), ~ifelse(. == 'Stimme voll zu - 5', 5, .))) %>%
  mutate(across(starts_with("SUS"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(starts_with("TuA"), ~ifelse(. == 'Stimme voll zu - 5', 5, .))) %>%
  mutate(across(starts_with("TuA"), ~ifelse(. == 'Stimme gar nicht zu - 1', 1, .))) %>%
  mutate(across(UsabilityMetric.SQ001.:UEQ.SQ026., ~as.numeric(.))) %>%
  mutate(across(AnstrengendWenden.SQ001.:Uebel.SQ001., ~as.numeric(.)))

#### adjust dataframe to prepare for combination with exp02 ####
data_exp01 <- lime_tab_n3 %>%
  add_column(Wetter = "N/A", .after = "AutomarkeLKA") %>%
  add_column(Licht = "N/A", .after = "Wetter") %>%
  add_column(Exp = "Exp01: Driving Simulator", .after ="HMI")

#### save data #### 
# write_excel_csv(data_exp01, "data/R_data_exp01.csv")
# save(data_exp01, file = "data/R_data_exp01.RData")
rm(list=setdiff(ls(), c("data_exp01")))
