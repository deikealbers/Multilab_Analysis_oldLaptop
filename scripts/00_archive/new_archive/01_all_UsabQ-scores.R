#### script for processing data of usability questionnaires ####
# builds on script 01_all_Merge.R

#### preparations ####
rm(list = ls())
library(tidyverse)
setwd("~/R/Multilab")

#### load dataset ####
load("data/R_data_all.RData")

#### Usability questionnaires ####

#### UMUX ####
# Method after Finstad 2010, 4.2 Analysis
data_Q <- data_all %>%
  add_column(UMUX01s = .$UMUX01 -1, .after ="UMUX04") %>%
  add_column(UMUX02s = 7- .$UMUX02, .after ="UMUX01s") %>%
  add_column(UMUX03s = .$UMUX03 -1, .after ="UMUX02s") %>%
  add_column(UMUX04s = 7 - .$UMUX04, .after ="UMUX03s") %>%
  add_column(UMUX_score = ((.$UMUX01s + .$UMUX02s +.$UMUX03s +.$UMUX04s)/24)*100, .after ="UMUX04s")

#### SUS ####
# Method after Brooke 1996, Scoring SUS
data_Q <- data_Q %>%
  add_column(SUS01s = .$SUS01 -1, .after ="SUS10") %>%
  add_column(SUS02s = 5- .$SUS02, .after ="SUS01s") %>%
  add_column(SUS03s = .$SUS03 -1, .after ="SUS02s") %>%
  add_column(SUS04s = 5- .$SUS04, .after ="SUS03s") %>%
  add_column(SUS05s = .$SUS05 -1, .after ="SUS04s") %>%
  add_column(SUS06s = 5- .$SUS06, .after ="SUS05s") %>%
  add_column(SUS07s = .$SUS07 -1, .after ="SUS06s") %>%
  add_column(SUS08s = 5- .$SUS08, .after ="SUS07s") %>%
  add_column(SUS09s = .$SUS09 -1, .after ="SUS08s") %>%
  add_column(SUS10s = 5- .$SUS10, .after ="SUS09s") %>%
  add_column(SUS_score = 2.5 * (.$SUS01s + .$SUS02s + .$SUS03s + .$SUS04s + .$SUS05s + .$SUS06s + .$SUS07s + .$SUS08s + .$SUS09s + .$SUS10s), .after = "SUS10s")

#### UEQ ####
# Method after Laugwitz, Manual and Excel tool
data_Q <- data_Q %>%
  add_column(UEQ01s = .$UEQ01 -4, .after = "UEQ26") %>%
  add_column(UEQ02s = .$UEQ02 -4, .after = "UEQ01s") %>%
  add_column(UEQ03s = 4-  .$UEQ03, .after = "UEQ02s") %>%
  add_column(UEQ04s = 4-  .$UEQ04, .after = "UEQ03s") %>%
  add_column(UEQ05s = 4-  .$UEQ05, .after = "UEQ04s") %>%
  add_column(UEQ06s = .$UEQ06 -4, .after = "UEQ05s") %>%
  add_column(UEQ07s = .$UEQ07 -4, .after = "UEQ06s") %>%
  add_column(UEQ08s = .$UEQ08 -4, .after = "UEQ07s") %>%
  add_column(UEQ09s = 4- .$UEQ09, .after = "UEQ08s") %>%
  add_column(UEQ10s = 4- .$UEQ10, .after = "UEQ09s") %>%
  add_column(UEQ11s = .$UEQ11 -4, .after = "UEQ10s") %>%
  add_column(UEQ12s = 4- .$UEQ12, .after = "UEQ11s") %>%
  add_column(UEQ13s = .$UEQ13 -4, .after = "UEQ12s") %>%
  add_column(UEQ14s = .$UEQ14 -4, .after = "UEQ13s") %>%
  add_column(UEQ15s = .$UEQ15 -4, .after = "UEQ14s") %>%
  add_column(UEQ16s = .$UEQ16 -4, .after = "UEQ15s") %>%
  add_column(UEQ17s = 4- .$UEQ17, .after = "UEQ16s") %>%
  add_column(UEQ18s = 4- .$UEQ18, .after = "UEQ17s") %>%
  add_column(UEQ19s = 4- .$UEQ19, .after = "UEQ18s") %>%
  add_column(UEQ20s = .$UEQ20 -4, .after = "UEQ19s") %>%
  add_column(UEQ21s = 4- .$UEQ21, .after = "UEQ20s") %>%
  add_column(UEQ22s = .$UEQ22 -4, .after = "UEQ21s") %>%
  add_column(UEQ23s = 4- .$UEQ23, .after = "UEQ22s") %>%
  add_column(UEQ24s = 4- .$UEQ24, .after = "UEQ23s") %>%
  add_column(UEQ25s = 4- .$UEQ25, .after = "UEQ24s") %>%
  add_column(UEQ26s = .$UEQ26 -4, .after = "UEQ25s")

data_Q <- data_Q %>%
  add_column(UEQ_Attractiveness = (.$UEQ01s + .$UEQ12s + .$UEQ14s + .$UEQ16s + .$UEQ24s + .$UEQ25s)/6, .after = "UEQ26s") %>%
  add_column(UEQ_Perspicuity = (.$UEQ02s + .$UEQ04s + .$UEQ13s + .$UEQ21s)/4, .after = "UEQ_Attractiveness") %>%
  add_column(UEQ_Efficiency = (.$UEQ09s + .$UEQ20s + .$UEQ22s + .$UEQ23s)/4, .after = "UEQ_Perspicuity") %>%
  add_column(UEQ_Dependability = (.$UEQ08s + .$UEQ11s + .$UEQ17s + .$UEQ19s)/4, .after = "UEQ_Efficiency") %>%
  add_column(UEQ_Stimulation = (.$UEQ05s + .$UEQ06s + .$UEQ07s + .$UEQ18s)/4, .after = "UEQ_Dependability") %>%
  add_column(UEQ_Novelty = (.$UEQ03s + .$UEQ10s + .$UEQ15s + .$UEQ26s)/4, .after = "UEQ_Stimulation")

#### Usability Questionnaires summary ####
UsabQ_summary <- data_Q %>%
  group_by(Exp, HMI) %>%
  summarise_at(.vars = vars(SUS_score, UMUX_score, Trust, Acceptance, UEQ_Attractiveness, UEQ_Perspicuity, UEQ_Efficiency, UEQ_Dependability, UEQ_Stimulation, UEQ_Novelty), 
               .funs = c(mean = "mean", sd ="sd", min = "min", max = "max", median = "median"))
print(as.data.frame(UsabQ_summary))

#### Anstrengung + Uebelkeit ####
Uebel_Anstr_summary <- data_Q %>%
  group_by(Exp) %>%
  summarise_at(.vars = vars(AnstrengendWenden, Uebel), 
               .funs = c(mean = "mean", sd ="sd", min = "min", max = "max", median = "median"))
print(as.data.frame(Uebel_Anstr_summary))

#### save data ####
write_excel_csv(data_Q, "data/processed/R_data_all_Q.csv")
write_excel_csv(UsabQ_summary, "data/processed/summary_UsabQ.csv")
write_excel_csv(Uebel_Anstr_summary, "data/processed/summary_Uebel_Anstr.csv")
save(data_Q, file = "data/processed/R_data_all_Q.RData")
# rm(list=setdiff(ls(), c("data_Q", "UsabQ_summary", "Uebel_Anstr_summary")))