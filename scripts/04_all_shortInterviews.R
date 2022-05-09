#### script for the short interviews ####
# builds on script 02_all_UsabQ_scores.R
# includes levels of automation & comparisons of 
    # observed level vs instructed        --> TC01_LevelObserved_Instr          ### actually belongs to driving behavior
    # observed level vs reported          --> TC01_LevelObserved_Rep
    # correct notice availability change  --> TC01_ImplemAvail
    # correct reason for availability change (only TC06, 10, 12) --> TC06_AvailReasonCorrect
        # 1 = correct, 0 = incorrect; 0.5 for half-correct (not existent)


#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
library(compute.es); 
library(ggplot2); 
library(multcomp);
library(pastecs); 
# library(reshape) # doesn't go with dplyr
library(rstatix)
library(coin)
library(vtable)

setwd("~/R/Multilab")

#### load dataset, delete not needed columns ####
load("data/R_data_all.RData")

data <- data_all %>%
  select(c(startdate, Exp, HMI, VPNr, 
           ends_with("_LevelObserved_Rep"), ends_with("_LevelObserved_Instr"), 
           ends_with("_AvailImplem_Rep"), ends_with("_AvailReasonCorrect"), 
           ends_with("Allow_Observed"), # HandsOff + Emails
           ends_with("TransProblems")))

#### calculate scores ####
  ## LevelObserved_Rep_score      (all TC --> max n*12)
  ## LevelObserved_Instr_score    (all TC --> max n*12)
  ## HandsOffAllow_Observed_score (all TC --> max n*12)
  ## EmailsAllow_Observed_score   (all TC --> max n*12)
  ## BothAllow_Observed_score     (all TC --> max 2n*12)
  ## TransProblems_score          (TC03, TC05, TC09, TC10, TC11, TC12 --> max n*6)
  ## AvailImplem_Rep_score        (all TC --> max n*12)
  ## AvailReasonCorrect_score     (TC06, TC10, TC12 --> max n*3)

data_scores <- data %>%
  add_column(LevelObserved_Rep_score = .$TC01_LevelObserved_Rep + .$TC02_LevelObserved_Rep + .$TC03_LevelObserved_Rep +
               .$TC04_LevelObserved_Rep + .$TC05_LevelObserved_Rep + .$TC06_LevelObserved_Rep +
               .$TC07_LevelObserved_Rep + .$TC08_LevelObserved_Rep + .$TC09_LevelObserved_Rep +
               .$TC10_LevelObserved_Rep + .$TC11_LevelObserved_Rep + .$TC12_LevelObserved_Rep, .after = "VPNr") %>%
  add_column(LevelObserved_Instr_score = .$TC01_LevelObserved_Instr + .$TC02_LevelObserved_Instr + .$TC03_LevelObserved_Instr +
               .$TC04_LevelObserved_Instr + .$TC05_LevelObserved_Instr + .$TC06_LevelObserved_Instr +
               .$TC07_LevelObserved_Instr + .$TC08_LevelObserved_Instr + .$TC09_LevelObserved_Instr +
               .$TC10_LevelObserved_Instr + .$TC11_LevelObserved_Instr + .$TC12_LevelObserved_Instr, .after = "VPNr") %>%
  add_column(HandsOffAllow_Observed_score = .$TC01_HandsOffAllow_Observed + .$TC02_HandsOffAllow_Observed + .$TC03_HandsOffAllow_Observed +
               .$TC04_HandsOffAllow_Observed + .$TC05_HandsOffAllow_Observed + .$TC06_HandsOffAllow_Observed +
               .$TC07_HandsOffAllow_Observed + .$TC08_HandsOffAllow_Observed + .$TC09_HandsOffAllow_Observed +
               .$TC10_HandsOffAllow_Observed + .$TC11_HandsOffAllow_Observed + .$TC12_HandsOffAllow_Observed, .after = "VPNr") %>%
  add_column(EmailsAllow_Observed_score = .$TC01_EmailsAllow_Observed + .$TC02_EmailsAllow_Observed + .$TC03_EmailsAllow_Observed +
               .$TC04_EmailsAllow_Observed + .$TC05_EmailsAllow_Observed + .$TC06_EmailsAllow_Observed +
               .$TC07_EmailsAllow_Observed + .$TC08_EmailsAllow_Observed + .$TC09_EmailsAllow_Observed +
               .$TC10_EmailsAllow_Observed + .$TC11_EmailsAllow_Observed + .$TC12_EmailsAllow_Observed, .after = "VPNr") %>%
  add_column(BothAllow_Observed_score = .$EmailsAllow_Observed_score + .$HandsOffAllow_Observed_score, .after = "VPNr") %>%
  add_column(TransProblems_score = .$TC03_TransProblems + .$TC05_TransProblems + .$TC09_TransProblems +
               .$TC10_TransProblems + .$TC11_TransProblems + .$TC12_TransProblems, .after = "VPNr") %>%
  add_column(AvailImplem_Rep_score = .$TC01_AvailImplem_Rep + .$TC02_AvailImplem_Rep + .$TC03_AvailImplem_Rep +
               .$TC04_AvailImplem_Rep + .$TC05_AvailImplem_Rep + .$TC06_AvailImplem_Rep +
               .$TC07_AvailImplem_Rep + .$TC08_AvailImplem_Rep + .$TC09_AvailImplem_Rep +
               .$TC10_AvailImplem_Rep + .$TC11_AvailImplem_Rep + .$TC12_AvailImplem_Rep, .after = "VPNr") %>%
  add_column(AvailReasonCorrect_score = .$TC06_AvailReasonCorrect + .$TC10_AvailReasonCorrect + .$TC12_AvailReasonCorrect, .after = "VPNr")
  
#### descriptive analysis of the scores ####
n_groups <- data_scores %>% 
  count(Exp, HMI)

shortI_summary <- data_scores %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(LevelObserved_Rep_score, LevelObserved_Instr_score, HandsOffAllow_Observed_score, 
                            EmailsAllow_Observed_score, BothAllow_Observed_score, TransProblems_score, 
                            AvailImplem_Rep_score, AvailReasonCorrect_score), 
               .funs = c(mean = "mean", sd ="sd", min = "min", max = "max", median = "median"))
print(as.data.frame(shortI_summary))

#### descriptive proportions of correct answers or observations per TC ####
    ## LevelObserved_Rep       (all TC, n)
    ## LevelObserved_Instr     (all TC, n)
    ## HandsOffAllow_Observed  (all TC, n)
    ## EmailsAllow_Observed    (all TC, n)
            ## BothAllow_Observed      (all TC, 2n) <- left out
    ## TransProblems           (6x TC, n)
    ## AvailImplem_Rep         (all TC, n)
    ## AvailReasonCorrect      (3x TC TC, n)

## proportion of correct answers per TC for the comparison Observed_Rep
rel_Observed_Rep <- data_scores %>%
  select(c(Exp, HMI, ends_with("Observed_Rep"))) %>%
  group_by(Exp, HMI) %>%
    summarise_at(.vars = vars(ends_with("Observed_Rep")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$LevelObserved_Rep_score_mean / 12)*100, digits = 2), .after = "HMI") 

## proportion of correct answers per TC for the comparison Observed_Instr
rel_Observed_Instr <- data_scores %>%
  select(c(Exp, HMI, VPNr, ends_with("Observed_Instr"))) %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(ends_with("Observed_Instr")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$LevelObserved_Instr_score_mean / 12)*100, digits = 2), .after = "HMI")

## proportion of correct answers per TC for the comparison HandsOffAllow_Observed
rel_HandsOffAllow_Observed <- data_scores %>%
  select(c(Exp, HMI, VPNr, ends_with("HandsOffAllow_Observed"))) %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(ends_with("HandsOffAllow_Observed")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$HandsOffAllow_Observed_score_mean / 12)*100, digits = 2), .after = "HMI")

## proportion of correct answers per TC for the comparison EmailsAllow_Observed 
rel_EmailsAllow_Observed <- data_scores %>%
  select(c(Exp, HMI, VPNr, ends_with("EmailsAllow_Observed"))) %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(ends_with("EmailsAllow_Observed")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$EmailsAllow_Observed_score_mean / 12)*100, digits = 2), .after = "HMI")

## proportion of mentions TransProblems 
rel_TransProblems <- data_scores %>%
  select(c(Exp, HMI, VPNr, ends_with("TransProblems"))) %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(ends_with("TransProblems")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$TransProblems_score_mean / 6)*100, digits = 2), .after = "HMI")
          
## proportion of correct answers per TC for the comparison AvailImplem_Rep
rel_AvailImplem_Rep_Observed <- data_scores %>%
  select(c(Exp, HMI, VPNr, ends_with("AvailImplem_Rep"))) %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(ends_with("AvailImplem_Rep")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$AvailImplem_Rep_score_mean / 12)*100, digits = 2), .after = "HMI")

## proportion of correct answers per TC for the comparison AvailReasonCorrect
rel_AvailReasonCorrect <- data_scores %>%
  select(c(Exp, HMI, VPNr, ends_with("AvailReasonCorrect"))) %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(ends_with("AvailReasonCorrect")), 
               .funs = c(rel = "sum")) %>%
  left_join(., n_groups, by = c("Exp", "HMI")) %>%
  mutate(across(ends_with("_rel"), ~round((.x / n)*100, digits =2))) %>%
  add_column(overall = round((shortI_summary$AvailReasonCorrect_score_mean / 3)*100, digits = 2), .after = "HMI")


#### qualitative analysis ####
data_qual <- data_all %>%
  select(c(Exp, HMI, VPNr, 
           ends_with("RemarksTP"), #12
           ends_with("RemarksTL"), #12
           ends_with("LevelOthers"), #12,
           ends_with("ChangeAvailOthers"), #12 
           ends_with("ChangeAvailReasonOthers"), #3
           ends_with("TransProblemsOthers"), #6
           ends_with("observedLevel"), #12
           ends_with("_Instr"), #12
           ends_with("ImplemAvail"), #12 %>%
           GutGefallen, NichtGefallen, Verbessern, Anmerkungen, AllgAnregungen
           )) %>%
  select(-c(ends_with("Observed_Instr"))) %>%
  select(order(colnames(.))) %>%
  relocate(HMI, .after = "Exp") %>%
  relocate(VPNr, .after = "HMI") %>%
  relocate(NichtGefallen, .after = "GutGefallen") %>%
  relocate(Verbessern, .after = "NichtGefallen") %>%
  relocate(Anmerkungen, .after = "Verbessern") %>%
  relocate(AllgAnregungen, .after = "Anmerkungen")

#### export results ####
write_excel_csv(shortI_summary, "data/processed/summary_shortInterviews.csv")

write_excel_csv(rel_AvailImplem_Rep_Observed, "data/processed/singleTC/rel_AvailImplem_Rep_Observed.csv")
write_excel_csv(rel_AvailReasonCorrect, "data/processed/singleTC/rel_AvailReasonCorrect.csv")
write_excel_csv(rel_EmailsAllow_Observed, "data/processed/singleTC/rel_EmailsAllow_Observed.csv")
write_excel_csv(rel_HandsOffAllow_Observed, "data/processed/singleTC/rel_HandsOffAllow_Observed.csv")
write_excel_csv(rel_Observed_Instr, "data/processed/singleTC/rel_Observed_Instr.csv")
write_excel_csv(rel_Observed_Rep, "data/processed/singleTC/rel_Observed_Rep.csv")
write_excel_csv(rel_TransProblems, "data/processed/singleTC/rel_TransProblems.csv")

write_excel_csv(data_qual, "data/processed/data_qualitative.csv")

save(data_scores, file = "data/processed/data_all_scores-shortInterviews.RData")