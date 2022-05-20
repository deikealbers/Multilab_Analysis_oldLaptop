#### script for processing data of exp03 ####
# copied from file Exp03_CleanUpData_20211127.R
# results in file "data/R_data_exp03.RData"

# check if changes in script affect section "# caution: depends on column position"

# note in mid_tab: ER is before TPComments in exp3 (in exp01 and exp02 it is behind TLComments which is after TPComments)

### preparations ###
rm(list = ls())
library(tidyverse)
setwd("~/R/Multilab")

#### import data ####
# Read in files
pre_tab <- read.csv("data/raw/Exp03_Pre_Codes.csv", encoding = "UTF-8")
mid_tab <- read.csv("data/raw/Exp03_Mid_Codes.csv", encoding = "UTF-8")
post_tab <- read.csv("data/raw/Exp03_Post_Codes.csv", encoding = "UTF-8")

##### clean up pre_tab #### 
# delete and combine columns
# assign correct data format incl. levels and rename unclear column names

# delete unnecessary rows, mutate wrong VPCode sections
# for this see excel 20211127_Versuchsplanung_VerteilungGruppen.xlsx & VPCodes.xlsx
pre_tab_n <- pre_tab %>%
  filter(startdate!="2021-07-05 08:17:01") %>% #DOGI02, supposedly Garret
  filter(startdate!="2021-07-07 21:18:05") %>% #LOBI06, doubled
  filter(startdate!="2021-07-13 18:13:45") %>% #BRST01, cancelled
  filter(startdate!="2021-07-15 13:41:29") %>% #FEBE11, cancelled
  filter(startdate!="2021-07-29 11:58:49") %>% #MAJO09, cancelled  
  filter(startdate!="2021-08-02 20:45:52") %>% #JUMI01, cancelled
  mutate(VPCode.VPCode3. = ifelse(startdate == "2021-07-02 18:15:30" & VPCode.VPCode3. == 21091999, 09, VPCode.VPCode3.)) %>%
  mutate(VPCode.VPCode3. = ifelse(startdate == "2021-07-07 16:04:45" & VPCode.VPCode3. == 14, 02, VPCode.VPCode3.)) %>%
  mutate(VPCode.VPCode1. = ifelse(startdate == "2021-07-31 16:46:21", "NO", VPCode.VPCode1.)) %>% #"NA" is not recognized as string
  mutate(VPCode.VPCode1. = toupper(VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode2. = toupper(VPCode.VPCode2.)) 

# delete
pre_tab_n1 <- pre_tab_n %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Instruktion))  %>%
  unite("VPCode", VPCode.VPCode1.: VPCode.VPCode3.) 

# assign correct data format incl. levels and rename unclear column names

# part of pre-questionnaire that is the same in exp1+2
pre_tab_n2 <- pre_tab_n1 %>%
  mutate("Farbfehlsichtigkeit" = ifelse(Farbfehlsichtigkeit == 'Y', 1, 0)) %>%
  rename(KilometerJahr = KmJahr) %>%
  rename(Vorwissen = Vorwissen.VorwissenAut.) %>%
  mutate(across(starts_with("FAS."), ~ifelse(. == 'Y', 1, 0))) %>%  
  rename(FAS.CC = FAS.CC.)  %>%
  rename(FAS.ACC = FAS.ACC.)  %>%
  rename(FAS.LKA = FAS.LKA.)  %>%
  rename(FAS.NoFAS = FAS.None.) %>%
  rename(TPComments_PreQ = TPComments)

# part of pre-questionnaire that is NOT the same in exp1+2
pre_tab_n3 <- pre_tab_n2 %>%
  mutate("NationGeb" = ifelse(NationGeb == 'Y', 1, 0))

# change variable names
VSM_names = c("VSM01", "VSM02", "VSM03", "VSM04", "VSM05", "VSM06", "VSM07", "VSM08", "VSM09", "VSM10", "VSM11", "VSM12", "VSM13", "VSM14", "VSM15", "VSM16", "VSM17", "VSM18", "VSM19", "VSM20", "VSM21", "VSM22", "VSM23", "VSM24") 
UsabFact_names = c("UsabFact01", "UsabFact02", "UsabFact03", "UsabFact04", "UsabFact05", "UsabFact06", "UsabFact07", "UsabFact08", "UsabFact09", "UsabFact10", "UsabFact11", "UsabFact12", "UsabFact13", "UsabFact14", "UsabFact15", "UsabFact16", "UsabFact17", "UsabFact18", "UsabFact19", "UsabFact20", "UsabFact21", "UsabFact22", "UsabFact23", "UsabFact24") 
colnames(pre_tab_n3) [23:46] <- VSM_names
colnames(pre_tab_n3) [47:70] <- UsabFact_names


#### clean up mid_tab ####
# delete and combine columns ! caution: columns selected by position
# assign correct data format incl. levels and rename unclear column names

# mutate wrong VPCode sections
# for this see excel 20211127_Versuchsplanung_VerteilungGruppen.xlsx & VPCodes.xlsx
mid_tab_n <- mid_tab %>%
  mutate(VPCode.VPCode1. = ifelse(startdate == "2021-08-03 07:19:17" & VPCode.VPCode1. == "DO", "DA", VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode1. = ifelse(startdate == "2021-08-03 09:32:57", "NO", VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode1. = toupper(VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode2. = toupper(VPCode.VPCode2.))

# delete
mid_tab_n1 <- mid_tab_n %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Datum, StopPage, StopPage2))  %>%
  rename(mid_VPNr = "VP", mid_HMI = 'HMI', mid_startdate = 'startdate') %>%  
  unite("VPCode", VPCode.VPCode1.: VPCode.VPCode3.)

# rename TC01-TC12; array copied from exp01 but changed position of ER (see notes below)
newnames = c("mid_startdate", "mid_VPNr", "mid_HMI", "Wetter", "Licht","VPCode", "SehhilfeAuto", "SehhilfeLesen", "SehhilfeAutoJetzt", "SehhilfeLesenJetzt", 
"TC01_LevelSel", "TC01_LevelOthers", "TC01_ChangeAvail", "TC01_ChangeAvailSel", "TC01_ChangeAvailOthers", "TC01_HandsOffAllow", "TC01_EmailsAllow", "TC01_ER", "TC01_RemarksTP", "TC01_RemarksTL", 
"TC02_LevelSel", "TC02_LevelOthers", "TC02_ChangeAvail", "TC02_ChangeAvailSel", "TC02_ChangeAvailOthers", "TC02_HandsOffAllow", "TC02_EmailsAllow", "TC02_ER", "TC02_RemarksTP", "TC02_RemarksTL", 
"TC03_LevelSel", "TC03_LevelOthers", "TC03_ChangeAvail", "TC03_ChangeAvailSel", "TC03_ChangeAvailOthers", "TC03_HandsOffAllow", "TC03_EmailsAllow", "TC03_TransProblems", "TC03_TransProblemsOthers", "TC03_ER", "TC03_RemarksTP", "TC03_RemarksTL", 
"TC04_LevelSel", "TC04_LevelOthers", "TC04_ChangeAvail", "TC04_ChangeAvailSel", "TC04_ChangeAvailOthers", "TC04_HandsOffAllow", "TC04_EmailsAllow", "TC04_ER", "TC04_RemarksTP", "TC04_RemarksTL", 
"TC05_LevelSel", "TC05_LevelOthers", "TC05_ChangeAvail", "TC05_ChangeAvailSel", "TC05_ChangeAvailOthers", "TC05_HandsOffAllow", "TC05_EmailsAllow", "TC05_TransProblems", "TC05_TransProblemsOthers", "TC05_ER", "TC05_RemarksTP", "TC05_RemarksTL",  
"TC06_LevelSel", "TC06_LevelOthers", "TC06_ChangeAvail", "TC06_ChangeAvailSel", "TC06_ChangeAvailOthers", "TC06_ChangeAvailReasonSel", "TC06_ChangeAvailReasonOthers", "TC06_HandsOffAllow", "TC06_EmailsAllow", "TC06_ER", "TC06_RemarksTP", "TC06_RemarksTL", 
"TC07_LevelSel", "TC07_LevelOthers", "TC07_ChangeAvail", "TC07_ChangeAvailSel", "TC07_ChangeAvailOthers", "TC07_HandsOffAllow", "TC07_EmailsAllow", "TC07_ER", "TC07_RemarksTP", "TC07_RemarksTL", 
"TC08_LevelSel", "TC08_LevelOthers", "TC08_ChangeAvail", "TC08_ChangeAvailSel", "TC08_ChangeAvailOthers", "TC08_HandsOffAllow", "TC08_EmailsAllow", "TC08_ER", "TC08_RemarksTP", "TC08_RemarksTL", 
"TC09_LevelSel", "TC09_LevelOthers", "TC09_ChangeAvail", "TC09_ChangeAvailSel", "TC09_ChangeAvailOthers", "TC09_HandsOffAllow", "TC09_EmailsAllow", "TC09_TransProblems", "TC09_TransProblemsOthers", "TC09_ER", "TC09_RemarksTP", "TC09_RemarksTL", 
"TC10_LevelSel", "TC10_LevelOthers", "TC10_ChangeAvail", "TC10_ChangeAvailSel", "TC10_ChangeAvailOthers", "TC10_ChangeAvailReasonSel", "TC10_ChangeAvailReasonOthers", "TC10_HandsOffAllow", "TC10_EmailsAllow", "TC10_TransProblems", "TC10_TransProblemsOthers", "TC10_ER", "TC10_RemarksTP", "TC10_RemarksTL", 
"TC11_LevelSel", "TC11_LevelOthers", "TC11_ChangeAvail", "TC11_ChangeAvailSel", "TC11_ChangeAvailOthers", "TC11_HandsOffAllow", "TC11_EmailsAllow", "TC11_TransProblems", "TC11_TransProblemsOthers", "TC11_ER", "TC11_RemarksTP", "TC11_RemarksTL",  
"TC12_LevelSel", "TC12_LevelOthers", "TC12_ChangeAvail", "TC12_ChangeAvailSel", "TC12_ChangeAvailOthers", "TC12_ChangeAvailReasonSel", "TC12_ChangeAvailReasonOthers", "TC12_HandsOffAllow", "TC12_EmailsAllow", "TC12_TransProblems", "TC12_TransProblemsOthers", "TC12_ER", "TC12_RemarksTP", "TC12_RemarksTL")
colnames(mid_tab_n1) <- newnames

# assign correct data format incl. levels and rename unclear column names
# part of pre-questionnaire that is the same in exp1+2
# assign correct data format
mid_tab_n2 <- mid_tab_n1 %>%
  mutate(mid_HMI = factor(mid_HMI)) %>%
  mutate(across(ends_with("LevelSel"), ~case_when( #"sonstiges" = 99 
    .== '-oth-' ~ 99,
    TRUE ~ as.numeric(.))))%>%
  mutate(across(ends_with("ChangeAvail"), ~ifelse(. == 'Y', 1, 0))) %>% 
  mutate(across(ends_with("ChangeAvailSel"), ~case_when( #"sonstiges" = 99 
    .== '-oth-' ~ 99,
    TRUE ~ as.numeric(.)))) %>%
  mutate(across(ends_with("ChangeAvailReasonSel"), ~case_when( #"sonstiges" = 99 , bei TC10 wird Systemgrenze als 0 codiert; bei TC6 und TC12 ist es Sensorfehler
    .== 'Sonstiges' ~ 99,
    TRUE ~ as.numeric(.)))) %>%
  mutate(across(ends_with("HandsOffAllow"), ~ifelse(. == 'Y', 1, 0))) %>% 
  mutate(across(ends_with("EmailsAllow"), ~ifelse(. == 'Y', 1, 0))) %>%  
  mutate(across(ends_with("TransProblems"), ~ifelse(. == 'Y', 1, 0))) 
  

#### clean up post_tab ####
# delete and combine columns
# assign correct data format incl. levels and rename unclear column names

# delete unnecessary rows, mutate wrong VPCode sections
# for this see excel 20211127_Versuchsplanung_VerteilungGruppen.xlsx & VPCodes.xlsx
post_tab_n <- post_tab %>%
  mutate(VPCode.VPCode1. = ifelse(startdate == "2021-08-02 15:11:24" & VPCode.VPCode1. == "AN", "AZ", VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode1. = ifelse(startdate == "2021-08-03 12:33:36", "NO", VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode1. = toupper(VPCode.VPCode1.)) %>%
  mutate(VPCode.VPCode2. = toupper(VPCode.VPCode2.))

# delete 
post_tab_n1 <- post_tab_n %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Datum, SpeechInfo, SpeechBreak))  %>%
  rename(post_VPNr = 'VP', post_HMI = 'HMI', post_startdate = 'startdate') %>%
  unite("VPCode", VPCode.VPCode1.: VPCode.VPCode3.)

# change variable names
UMUX_names = c("UMUX01", "UMUX02", "UMUX03", "UMUX04")
SUS_names = c("SUS01", "SUS02", "SUS03", "SUS04", "SUS05", "SUS06", "SUS07", "SUS08", "SUS09", "SUS10")
UEQ_names = c("UEQ01", "UEQ02", "UEQ03", "UEQ04", "UEQ05", "UEQ06", "UEQ07", "UEQ08", "UEQ09", "UEQ10", "UEQ11", "UEQ12", "UEQ13", "UEQ14", "UEQ15", "UEQ16", "UEQ17", "UEQ18", "UEQ19", "UEQ20", "UEQ21", "UEQ22", "UEQ23", "UEQ24", "UEQ25", "UEQ26") 

post_tab_n2 <- post_tab_n1 %>%
  rename(Trust = TuA.Trust.) %>%
  rename(Acceptance = TuA.Acceptance.) %>%
  rename(AnstrengendWenden = AnstrengendWenden.AnstrengendWenden.) %>%
  rename(Uebel = Uebel.Uebel.)

# caution: depends on column position
colnames(post_tab_n2) [5:8] <- UMUX_names
colnames(post_tab_n2) [9:18] <- SUS_names
colnames(post_tab_n2) [21:46] <- UEQ_names


#### join data ####
# join data
join_tab <- left_join(pre_tab_n3, mid_tab_n2, by = "VPCode")
join_tab_n <- left_join(join_tab, post_tab_n2, by = "VPCode")
join_tab_n1 <- join_tab_n %>%
  relocate(mid_startdate, .after = startdate) %>%
  relocate(post_startdate, .after = mid_startdate) %>%
  relocate(mid_HMI, .after = VPCode) %>%
  relocate(post_HMI, .after = mid_HMI) %>%
  relocate(mid_VPNr, .after = VPCode) %>%
  relocate(post_VPNr, .after = mid_VPNr)

# delete doubled columns and add column for experiment information
data_exp03 <- join_tab_n1 %>%
  select(-c(startdate, post_startdate, post_HMI, post_VPNr)) %>%
  rename(HMI = 'mid_HMI') %>%
  rename(startdate = 'mid_startdate') %>%  
  rename(VPNr = 'mid_VPNr') 

# modify for merge
data_exp03 <- data_exp03 %>%
  add_column(Exp = 3, .before ="HMI")

#### save data ####
write_excel_csv(data_exp03, "data/R_data_exp03.csv")
save(data_exp03, file = "data/R_data_exp03.RData")
rm(list=setdiff(ls(), c("data_exp03")))