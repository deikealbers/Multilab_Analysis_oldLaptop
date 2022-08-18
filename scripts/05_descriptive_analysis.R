#### notes ####
## these two questionnaires are not analysed yet (in previous scripts. Summaries of single items is not purposeful
## therefore, they'll be excluded from summary. When they're included they shall be added here (vorbefragung))
#     VSM... (n = 24) --------- not included in summaries
#     UsabFact... (n = 24)  --------- not included in summaries

#### preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(data.table)
setwd("~/R/Multilab_Analysis")

#### write function for summary ####
# adjusted from skim() function
mean_ML = function(x) mean(x, na.rm = TRUE)
sd_ML = function(x) sd(x, na.rm = TRUE)
median_ML = function(x) median(x, na.rm = TRUE)
min_ML = function(x) min(x, na.rm = TRUE)
max_ML = function(x) max(x, na.rm = TRUE)

ML_skim <- skim_with(numeric = sfl(n = length, mean = mean_ML, sd = sd_ML, median = median_ML, min =  min_ML, max = max_ML), append = FALSE)

#### import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")

#### notes on variables ####
### meta variables, n = 5  ##
#     startdate
#     Exp
#     HMI
#     VPCode
#     VPNr

### variables of vorbefragung ###
# ## nominal data, n = 10 ##
#     Geschlecht
#     NationGeb
#     Sehhilfe
#     SehhilfeAuto
#     SehhilfeAutoJetzt
#     SehhilfeLesen
#     SehhilfeLesenJetzt
#     Farbfehlsichtigkeit
#     Wetter
#     Licht
# ## ordinal data, n = 58 ##
#     Erfahrung
#     KilometerJahr
#     Vorwissen 
#     FAS.CC ... (n = 4)
#     OftCC ... (n = 3)
#     VSM... (n = 24) --------- not included in summaries
#     UsabFact... (n = 24)  --------- not included in summaries
# ## interval data, n = 1 ##
#     Alter
# ## other (text, qualitative, ...), n = 7 ##
#     Nation
#     NationGeb2
#     Farbfehlsichtigkeit2
#     Automarke... (n = 3)
#     TCPComments_PreQ

### variables of nachbefragung ###
# ## nominal data, n = 0 ##
#
# ## ordinal data, n = 16 ##
#     TC...ER (n = 12)
#     Trust
#     Acceptance
#     AnstrengendWenden
#     Uebel
# ## interval data, n =  9 ##
#     UMUX_score
#     SUS_score
#     UEQ_...(subscales) (n = 6)
#     ER_overall
# ## count data, n = 8 ##
#     AvailReasonCorrect_score
#     AvailImplem_Rep_score
#     TransProblems_score
#     BothAllow_Observed_score
#     EmailsAllow_Observed_score
#     HandsOffAllow_Observed_score
#     LevelObserved_Instr_score
#     LevelObserved_Rep_score
# ## other (text, qualitative, ...), n = 50 ##
#     TC...ChangeAvailOthers (n = 12) (bereits mit scores verrechnet)
#     TC...RemarksTP (n = 12)
#     TC...RemarksTL (n = 12)
#     TC...TransProblemsOthers (n = 6)
#     TC...ChangeAvailReasonCorrectOthers (n = 3) (bereits mit scores verrechnet)
#     GutGefallen
#     NichtGefallen
#     Verbessern
#     Anmerkungen
#     AllgAnregungen
# ## not needed variables, n = 325 ##
#     TC...LevelSel (n = 12)
#     TC...LevelRep (n = 12)
#     TC...LevelOthers (n = 12)
#     TC...ChangeAvail (n = 12)
#     TC...ChangeAvailSel (n = 12)
#     TC...ChangeAvailRep (n =12)
#     TC...ImplemAvail (n = 12)
#     TC...AvailImplemRep (n = 12)
#     TC...HandsOffAllow (n = 12)
#     TC...HandsOffAllow_Observed (n = 12)
#     TC...EmailsAllow (n = 12)
#     TC...Emails_Observed (n = 12)
#     TC...TransProblems (n = 6)
#     TC...ChangeAvailReasonSel (n = 3)
#     TC...ChangeAvailReasonRep (n = 3)
#     TC...ChangeAvailReasonCorrect (n = 3)
#     UMUX** + UMUX**s (n = 8)
#     SUS** + SUS**s (n = 20)
#     UEQ** + UEQ**s (n = 52)
#     Speech... (n = 14) (ausgewertet in AHFE Paper)
#     TC...LevelSel_n (n = 12)
#     TC...ChangeAvail_n (n = 12)
#     TC...ChangeAvailReason_n (n = 3)
#     TC..._observedLevel (n = 12)
#     TC..._Instr (n = 12)
#     TC..._LevelObserved_Instr (n = 12)
#     TC..._LevelObserved_Rep (n = 12)


#### subsets vorbefragung ####
# nominal data
subset_vor_nominal <- data_all %>%
  select(Exp, HMI, 
         Geschlecht, NationGeb, 
         Sehhilfe, 
         SehhilfeAuto, SehhilfeAutoJetzt, SehhilfeLesen, SehhilfeLesenJetzt, 
         Farbfehlsichtigkeit, 
         Wetter, Licht) %>%
  mutate(Geschlecht = as.factor(Geschlecht)) %>%
  mutate(NationGeb = as.factor(NationGeb)) %>%
  mutate(across(starts_with("Sehhilfe"), as_factor)) %>%
  mutate(Farbfehlsichtigkeit = as.factor(Farbfehlsichtigkeit)) %>%
  mutate(Wetter = as.factor(Wetter)) %>%
  mutate(Licht = as.factor(Licht))
# ordinal data
vars_ordinal <- c("Erfahrung", "KilometerJahr", 
                  "FAS.CC", "FAS.ACC", "FAS.LKA", "FAS.NoFAS",
                  "OftCC", "OftACC", "OftLKA"
                  # VSM + UsabFactors
                  )

subset_vor_ordinal_factors <- data_all %>%
  select(Exp, HMI, 
         vars_ordinal) %>%
  mutate_at(vars_ordinal, factor)
subset_vor_ordinal_nofactors <- data_all %>%
  select(Exp, HMI, 
         vars_ordinal)

# interval data
subset_vor_interval <- data_all %>%
  select(Exp, HMI, 
         Alter)

#### descriptive summaries vorbefragung ####
# summary nominal data
summary_vor_nominal <- subset_vor_nominal %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique)) %>%
  add_column(numeric.n = ifelse(.$Exp == 1, 26, 
                        ifelse(.$Exp == 2 & .$HMI == "HC", 33,
                               ifelse(.$Exp == 2 & .$HMI == "LC", 28,
                                      ifelse(.$Exp == 3, 21, NA)))))

# summary ordinal data
summary_vor_ordinal_factors <- subset_vor_ordinal_factors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_vor_ordinal_nofactors <- subset_vor_ordinal_nofactors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_vor_ordinal <- left_join(summary_vor_ordinal_nofactors, summary_vor_ordinal_factors)

# summary interval data
summary_vor_interval <- subset_vor_interval %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))


# create summary for vorbefragung
summary_vorbefragung <- bind_rows(summary_vor_nominal, summary_vor_ordinal, summary_vor_interval)

summary_headers <- c("variable", "Exp", "HMI", "n_missing", "frequencies", "n", "mean", "sd", "median", "min", "max")
names(summary_vorbefragung) <- summary_headers

summ_vorbefragung <- summary_vorbefragung %>%
  mutate(n = n - n_missing) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 2)) %>%
  mutate(max = round(max, 2)) %>%
  relocate(frequencies, .after = "max")

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all", "summ_vorbefragung",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

#### subsets nachbefragung ####
# ordinal data
vars_ordinal <- c("Trust", "Acceptance",  
                  "TC01_ER", "TC02_ER", "TC03_ER", "TC04_ER", "TC05_ER", "TC06_ER", 
                  "TC07_ER", "TC08_ER", "TC09_ER", "TC10_ER", "TC11_ER", "TC12_ER")
subset_nach_ordinal_factors <- data_all %>%
  select(Exp, HMI, 
         vars_ordinal) %>%
  mutate_at(vars_ordinal, factor)
subset_nach_ordinal_nofactors <- data_all %>%
  select(Exp, HMI, 
         vars_ordinal)

# interval data
subset_nach_interval <- data_all %>%
  select(Exp, HMI, 
         SUS_score, UMUX_score, starts_with("UEQ_"), ER_overall,
         LevelObserved_Rep_score, LevelObserved_Instr_score, 
         HandsOffAllow_Observed_score, EmailsAllow_Observed_score, BothAllow_Observed_score,
         TransProblems_score, 
         AvailImplem_Rep_score, AvailReasonCorrect_score)

# ordinal data, but only distinguish between experiments, not HMIs
subset_nach_ordinal_exp_factors <- data_all %>%
  select(Exp, 
         AnstrengendWenden, Uebel) %>%
  mutate_at(c("AnstrengendWenden", "Uebel"), factor)
subset_nach_ordinal_exp_nofactors <- data_all %>%
  select(Exp, 
         AnstrengendWenden, Uebel)

#### descriptive summaries nachbefragung ####
# summary ordinal data
summary_nach_ordinal_factors <- subset_nach_ordinal_factors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_nach_ordinal_nofactors <- subset_nach_ordinal_nofactors %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_nach_ordinal <- left_join(summary_nach_ordinal_nofactors, summary_nach_ordinal_factors)

# summary interval data
summary_nach_interval <- subset_nach_interval %>%
  dplyr::group_by(Exp, HMI) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

# summary ordinal data, but only distinguish between experiments, not HMIs
summary_nach_ordinal_exp_factors <- subset_nach_ordinal_exp_factors %>%
  dplyr::group_by(Exp) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_nach_ordinal_exp_nofactors <- subset_nach_ordinal_exp_nofactors %>%
  dplyr::group_by(Exp) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_nach_ordinal_exp <- left_join(summary_nach_ordinal_exp_nofactors, summary_nach_ordinal_exp_factors)

# create summary for nachbefragung
summary_nachbefragung <- bind_rows(summary_nach_ordinal, summary_nach_interval, summary_nach_ordinal_exp)

summary_headers <- c("variable", "Exp", "HMI", "n_missing", "n", "mean", "sd", "median", "min", "max", "frequencies")
names(summary_nachbefragung) <- summary_headers

summ_nachbefragung <- summary_nachbefragung %>%
  mutate(n = n - n_missing) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 2)) %>%
  mutate(max = round(max, 2)) %>%
  relocate(frequencies, .after = "max")

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))
#### frequency tables of nominal and ordinal data: vor- and nachbefragung ####
# functions for frequency tables
notemptycount_ML <- function(x) {  (sum(x!="", na.rm = TRUE))}

count0_ML <- function(x) {  (sum(x==0, na.rm = TRUE))}
count1_ML <- function(x) {  (sum(x==1, na.rm = TRUE))}
count2_ML <- function(x) {  (sum(x==2, na.rm = TRUE))}
count3_ML <- function(x) {  (sum(x==3, na.rm = TRUE))}
count4_ML <- function(x) {  (sum(x==4, na.rm = TRUE))}
count5_ML <- function(x) {  (sum(x==5, na.rm = TRUE))}
count6_ML <- function(x) {  (sum(x==6, na.rm = TRUE))}

### vor - and nachbefragung ###
## preparation
ML_variables = c("Geschlecht", "NationGeb", 
                  "Sehhilfe", 
                  "SehhilfeAuto", "SehhilfeAutoJetzt", "SehhilfeLesen", "SehhilfeLesenJetzt", 
                  "Farbfehlsichtigkeit", 
                  "Wetter", "Licht",
                  "Erfahrung", "KilometerJahr", 
                  "FAS.CC", "FAS.ACC", "FAS.LKA", "FAS.NoFAS",
                  "OftCC", "OftACC", "OftLKA",
                  # VSM + UsabFactors
                  "TC01_ER", "TC02_ER", "TC03_ER", "TC04_ER", "TC05_ER", "TC06_ER",
                  "TC07_ER", "TC08_ER", "TC09_ER", "TC10_ER", "TC11_ER", "TC12_ER",
                  "Trust", "Acceptance"
                  )

# ## 1HC
# ML_1HC <- data_all %>%
#   filter(Exp == 1 & HMI == "HC") %>%
#   select(all_of(ML_variables))
# 
# ML_1HC0 <- as.data.frame(sapply(ML_1HC, count0_ML))
# ML_1HC1 <- as.data.frame(sapply(ML_1HC, count1_ML))
# ML_1HC2 <- as.data.frame(sapply(ML_1HC, count2_ML))
# ML_1HC3 <- as.data.frame(sapply(ML_1HC, count3_ML))
# ML_1HC4 <- as.data.frame(sapply(ML_1HC, count4_ML))
# ML_1HC5 <- as.data.frame(sapply(ML_1HC, count5_ML))
# ML_1HCn <- as.data.frame(sapply(ML_1HC, notemptycount_ML))
# 
# ML_freq_1HC <- data.frame(ML_1HCn, ML_1HC0, ML_1HC1, ML_1HC2, ML_1HC3, ML_1HC4, ML_1HC5) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "1HC", variable = ML_variables, .before = "n")
# 
# ## 1LC
# ML_1LC <- data_all %>%
#   filter(Exp == 1 & HMI == "LC") %>%
#   select(all_of(ML_variables))
# 
# ML_1LC0 <- as.data.frame(sapply(ML_1LC, count0_ML))
# ML_1LC1 <- as.data.frame(sapply(ML_1LC, count1_ML))
# ML_1LC2 <- as.data.frame(sapply(ML_1LC, count2_ML))
# ML_1LC3 <- as.data.frame(sapply(ML_1LC, count3_ML))
# ML_1LC4 <- as.data.frame(sapply(ML_1LC, count4_ML))
# ML_1LC5 <- as.data.frame(sapply(ML_1LC, count5_ML))
# ML_1LCn <- as.data.frame(sapply(ML_1LC, notemptycount_ML))
# 
# ML_freq_1LC <- data.frame(ML_1LCn, ML_1LC0, ML_1LC1, ML_1LC2, ML_1LC3, ML_1LC4, ML_1LC5) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "1LC", variable = ML_variables, .before = "n")
# 
# ## 2HC
# ML_2HC <- data_all %>%
#   filter(Exp == 2 & HMI == "HC") %>%
#   select(all_of(ML_variables))
# 
# ML_2HC0 <- as.data.frame(sapply(ML_2HC, count0_ML))
# ML_2HC1 <- as.data.frame(sapply(ML_2HC, count1_ML))
# ML_2HC2 <- as.data.frame(sapply(ML_2HC, count2_ML))
# ML_2HC3 <- as.data.frame(sapply(ML_2HC, count3_ML))
# ML_2HC4 <- as.data.frame(sapply(ML_2HC, count4_ML))
# ML_2HC5 <- as.data.frame(sapply(ML_2HC, count5_ML))
# ML_2HCn <- as.data.frame(sapply(ML_2HC, notemptycount_ML))
# 
# ML_freq_2HC <- data.frame(ML_2HCn, ML_2HC0, ML_2HC1, ML_2HC2, ML_2HC3, ML_2HC4, ML_2HC5) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "2HC", variable = ML_variables, .before = "n")
# 
# ## 2LC
# ML_2LC <- data_all %>%
#   filter(Exp == 2 & HMI == "LC") %>%
#   select(all_of(ML_variables))
# 
# ML_2LC0 <- as.data.frame(sapply(ML_2LC, count0_ML))
# ML_2LC1 <- as.data.frame(sapply(ML_2LC, count1_ML))
# ML_2LC2 <- as.data.frame(sapply(ML_2LC, count2_ML))
# ML_2LC3 <- as.data.frame(sapply(ML_2LC, count3_ML))
# ML_2LC4 <- as.data.frame(sapply(ML_2LC, count4_ML))
# ML_2LC5 <- as.data.frame(sapply(ML_2LC, count5_ML))
# ML_2LCn <- as.data.frame(sapply(ML_2LC, notemptycount_ML))
# 
# ML_freq_2LC <- data.frame(ML_2LCn, ML_2LC0, ML_2LC1, ML_2LC2, ML_2LC3, ML_2LC4, ML_2LC5) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "2LC", variable = ML_variables, .before = "n")
# 
# ## 3HC
# ML_3HC <- data_all %>%
#   filter(Exp == 3 & HMI == "HC") %>%
#   select(all_of(ML_variables))
# 
# ML_3HC0 <- as.data.frame(sapply(ML_3HC, count0_ML))
# ML_3HC1 <- as.data.frame(sapply(ML_3HC, count1_ML))
# ML_3HC2 <- as.data.frame(sapply(ML_3HC, count2_ML))
# ML_3HC3 <- as.data.frame(sapply(ML_3HC, count3_ML))
# ML_3HC4 <- as.data.frame(sapply(ML_3HC, count4_ML))
# ML_3HC5 <- as.data.frame(sapply(ML_3HC, count5_ML))
# ML_3HCn <- as.data.frame(sapply(ML_3HC, notemptycount_ML))
# 
# ML_freq_3HC <- data.frame(ML_3HCn, ML_3HC0, ML_3HC1, ML_3HC2, ML_3HC3, ML_3HC4, ML_3HC5) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "3HC", variable = ML_variables, .before = "n")
# 
# ## 3LC
# ML_3LC <- data_all %>%
#   filter(Exp == 3 & HMI == "LC") %>%
#   select(all_of(ML_variables))
# 
# ML_3LC0 <- as.data.frame(sapply(ML_3LC, count0_ML))
# ML_3LC1 <- as.data.frame(sapply(ML_3LC, count1_ML))
# ML_3LC2 <- as.data.frame(sapply(ML_3LC, count2_ML))
# ML_3LC3 <- as.data.frame(sapply(ML_3LC, count3_ML))
# ML_3LC4 <- as.data.frame(sapply(ML_3LC, count4_ML))
# ML_3LC5 <- as.data.frame(sapply(ML_3LC, count5_ML))
# ML_3LCn <- as.data.frame(sapply(ML_3LC, notemptycount_ML))
# 
# ML_freq_3LC <- data.frame(ML_3LCn, ML_3LC0, ML_3LC1, ML_3LC2, ML_3LC3, ML_3LC4, ML_3LC5) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "3LC", variable = ML_variables, .before = "n")
# 
# ## 1 --- UebelAnstrengend
# ML_1 <- data_all %>%
#   filter(Exp == 1) %>%
#   select(all_of(c("Uebel", "AnstrengendWenden")))
# 
# ML_10 <- as.data.frame(sapply(ML_1, count0_ML))
# ML_11 <- as.data.frame(sapply(ML_1, count1_ML))
# ML_12 <- as.data.frame(sapply(ML_1, count2_ML))
# ML_13 <- as.data.frame(sapply(ML_1, count3_ML))
# ML_14 <- as.data.frame(sapply(ML_1, count4_ML))
# ML_15 <- as.data.frame(sapply(ML_1, count5_ML))
# ML_1n <- as.data.frame(sapply(ML_1, notemptycount_ML))
# 
# ML_freq_1 <- data.frame(ML_1n, ML_10, ML_11, ML_12, ML_13, ML_14, ML_15) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "1", variable = c("Uebel", "AnstrengendWenden"), .before = "n")
# 
# ## 2 --- UebelAnstrengend
# ML_2 <- data_all %>%
#   filter(Exp == 2) %>%
#   select(all_of(c("Uebel", "AnstrengendWenden")))
# 
# ML_20 <- as.data.frame(sapply(ML_2, count0_ML))
# ML_21 <- as.data.frame(sapply(ML_2, count1_ML))
# ML_22 <- as.data.frame(sapply(ML_2, count2_ML))
# ML_23 <- as.data.frame(sapply(ML_2, count3_ML))
# ML_24 <- as.data.frame(sapply(ML_2, count4_ML))
# ML_25 <- as.data.frame(sapply(ML_2, count5_ML))
# ML_2n <- as.data.frame(sapply(ML_2, notemptycount_ML))
# 
# ML_freq_2 <- data.frame(ML_2n, ML_20, ML_21, ML_22, ML_23, ML_24, ML_25) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "2", variable = c("Uebel", "AnstrengendWenden"), .before = "n")
# 
# ## 3 --- UebelAnstrengend
# ML_3 <- data_all %>%
#   filter(Exp == 3) %>%
#   select(all_of(c("Uebel", "AnstrengendWenden")))
# 
# ML_30 <- as.data.frame(sapply(ML_3, count0_ML))
# ML_31 <- as.data.frame(sapply(ML_3, count1_ML))
# ML_32 <- as.data.frame(sapply(ML_3, count2_ML))
# ML_33 <- as.data.frame(sapply(ML_3, count3_ML))
# ML_34 <- as.data.frame(sapply(ML_3, count4_ML))
# ML_35 <- as.data.frame(sapply(ML_3, count5_ML))
# ML_3n <- as.data.frame(sapply(ML_3, notemptycount_ML))
# 
# ML_freq_3 <- data.frame(ML_3n, ML_30, ML_31, ML_32, ML_33, ML_34, ML_35) %>%
#   setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
#   add_column(group = "3", variable = c("Uebel", "AnstrengendWenden"), .before = "n")

## 1HC
ML_1HC <- data_all %>%
  filter(Exp == 1 & HMI == "HC") %>%
  select(all_of(ML_variables))

ML_1HC0 <- as.data.frame(sapply(ML_1HC, count0_ML))
ML_1HC1 <- as.data.frame(sapply(ML_1HC, count1_ML))
ML_1HC2 <- as.data.frame(sapply(ML_1HC, count2_ML))
ML_1HC3 <- as.data.frame(sapply(ML_1HC, count3_ML))
ML_1HC4 <- as.data.frame(sapply(ML_1HC, count4_ML))
ML_1HC5 <- as.data.frame(sapply(ML_1HC, count5_ML))
ML_1HCn <- as.data.frame(sapply(ML_1HC, notemptycount_ML))

ML_freq_1HC <- data.frame(ML_1HCn, ML_1HC0, ML_1HC1, ML_1HC2, ML_1HC3, ML_1HC4, ML_1HC5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 1, HMI = "1HC", variable = ML_variables, .before = "n")

## 1LC
ML_1LC <- data_all %>%
  filter(Exp == 1 & HMI == "LC") %>%
  select(all_of(ML_variables))

ML_1LC0 <- as.data.frame(sapply(ML_1LC, count0_ML))
ML_1LC1 <- as.data.frame(sapply(ML_1LC, count1_ML))
ML_1LC2 <- as.data.frame(sapply(ML_1LC, count2_ML))
ML_1LC3 <- as.data.frame(sapply(ML_1LC, count3_ML))
ML_1LC4 <- as.data.frame(sapply(ML_1LC, count4_ML))
ML_1LC5 <- as.data.frame(sapply(ML_1LC, count5_ML))
ML_1LCn <- as.data.frame(sapply(ML_1LC, notemptycount_ML))

ML_freq_1LC <- data.frame(ML_1LCn, ML_1LC0, ML_1LC1, ML_1LC2, ML_1LC3, ML_1LC4, ML_1LC5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 1, HMI = "LC", variable = ML_variables, .before = "n")

## 2HC
ML_2HC <- data_all %>%
  filter(Exp == 2 & HMI == "HC") %>%
  select(all_of(ML_variables))

ML_2HC0 <- as.data.frame(sapply(ML_2HC, count0_ML))
ML_2HC1 <- as.data.frame(sapply(ML_2HC, count1_ML))
ML_2HC2 <- as.data.frame(sapply(ML_2HC, count2_ML))
ML_2HC3 <- as.data.frame(sapply(ML_2HC, count3_ML))
ML_2HC4 <- as.data.frame(sapply(ML_2HC, count4_ML))
ML_2HC5 <- as.data.frame(sapply(ML_2HC, count5_ML))
ML_2HCn <- as.data.frame(sapply(ML_2HC, notemptycount_ML))

ML_freq_2HC <- data.frame(ML_2HCn, ML_2HC0, ML_2HC1, ML_2HC2, ML_2HC3, ML_2HC4, ML_2HC5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 2, HMI = "HC", variable = ML_variables, .before = "n")

## 2LC
ML_2LC <- data_all %>%
  filter(Exp == 2 & HMI == "LC") %>%
  select(all_of(ML_variables))

ML_2LC0 <- as.data.frame(sapply(ML_2LC, count0_ML))
ML_2LC1 <- as.data.frame(sapply(ML_2LC, count1_ML))
ML_2LC2 <- as.data.frame(sapply(ML_2LC, count2_ML))
ML_2LC3 <- as.data.frame(sapply(ML_2LC, count3_ML))
ML_2LC4 <- as.data.frame(sapply(ML_2LC, count4_ML))
ML_2LC5 <- as.data.frame(sapply(ML_2LC, count5_ML))
ML_2LCn <- as.data.frame(sapply(ML_2LC, notemptycount_ML))

ML_freq_2LC <- data.frame(ML_2LCn, ML_2LC0, ML_2LC1, ML_2LC2, ML_2LC3, ML_2LC4, ML_2LC5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 2, HMI = "LC", variable = ML_variables, .before = "n")

## 3HC
ML_3HC <- data_all %>%
  filter(Exp == 3 & HMI == "HC") %>%
  select(all_of(ML_variables))

ML_3HC0 <- as.data.frame(sapply(ML_3HC, count0_ML))
ML_3HC1 <- as.data.frame(sapply(ML_3HC, count1_ML))
ML_3HC2 <- as.data.frame(sapply(ML_3HC, count2_ML))
ML_3HC3 <- as.data.frame(sapply(ML_3HC, count3_ML))
ML_3HC4 <- as.data.frame(sapply(ML_3HC, count4_ML))
ML_3HC5 <- as.data.frame(sapply(ML_3HC, count5_ML))
ML_3HCn <- as.data.frame(sapply(ML_3HC, notemptycount_ML))

ML_freq_3HC <- data.frame(ML_3HCn, ML_3HC0, ML_3HC1, ML_3HC2, ML_3HC3, ML_3HC4, ML_3HC5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 3, HMI = "HC", variable = ML_variables, .before = "n")

## 3LC
ML_3LC <- data_all %>%
  filter(Exp == 3 & HMI == "LC") %>%
  select(all_of(ML_variables))

ML_3LC0 <- as.data.frame(sapply(ML_3LC, count0_ML))
ML_3LC1 <- as.data.frame(sapply(ML_3LC, count1_ML))
ML_3LC2 <- as.data.frame(sapply(ML_3LC, count2_ML))
ML_3LC3 <- as.data.frame(sapply(ML_3LC, count3_ML))
ML_3LC4 <- as.data.frame(sapply(ML_3LC, count4_ML))
ML_3LC5 <- as.data.frame(sapply(ML_3LC, count5_ML))
ML_3LCn <- as.data.frame(sapply(ML_3LC, notemptycount_ML))

ML_freq_3LC <- data.frame(ML_3LCn, ML_3LC0, ML_3LC1, ML_3LC2, ML_3LC3, ML_3LC4, ML_3LC5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 3, HMI = "LC", variable = ML_variables, .before = "n")

## 1 --- UebelAnstrengend
ML_1 <- data_all %>%
  filter(Exp == 1) %>%
  select(all_of(c("Uebel", "AnstrengendWenden")))

ML_10 <- as.data.frame(sapply(ML_1, count0_ML))
ML_11 <- as.data.frame(sapply(ML_1, count1_ML))
ML_12 <- as.data.frame(sapply(ML_1, count2_ML))
ML_13 <- as.data.frame(sapply(ML_1, count3_ML))
ML_14 <- as.data.frame(sapply(ML_1, count4_ML))
ML_15 <- as.data.frame(sapply(ML_1, count5_ML))
ML_1n <- as.data.frame(sapply(ML_1, notemptycount_ML))

ML_freq_1 <- data.frame(ML_1n, ML_10, ML_11, ML_12, ML_13, ML_14, ML_15) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 1, HMI = NA, variable = c("Uebel", "AnstrengendWenden"), .before = "n")

## 2 --- UebelAnstrengend
ML_2 <- data_all %>%
  filter(Exp == 2) %>%
  select(all_of(c("Uebel", "AnstrengendWenden")))

ML_20 <- as.data.frame(sapply(ML_2, count0_ML))
ML_21 <- as.data.frame(sapply(ML_2, count1_ML))
ML_22 <- as.data.frame(sapply(ML_2, count2_ML))
ML_23 <- as.data.frame(sapply(ML_2, count3_ML))
ML_24 <- as.data.frame(sapply(ML_2, count4_ML))
ML_25 <- as.data.frame(sapply(ML_2, count5_ML))
ML_2n <- as.data.frame(sapply(ML_2, notemptycount_ML))

ML_freq_2 <- data.frame(ML_2n, ML_20, ML_21, ML_22, ML_23, ML_24, ML_25) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 2, HMI = NA, variable = c("Uebel", "AnstrengendWenden"), .before = "n")

## 3 --- UebelAnstrengend
ML_3 <- data_all %>%
  filter(Exp == 3) %>%
  select(all_of(c("Uebel", "AnstrengendWenden")))

ML_30 <- as.data.frame(sapply(ML_3, count0_ML))
ML_31 <- as.data.frame(sapply(ML_3, count1_ML))
ML_32 <- as.data.frame(sapply(ML_3, count2_ML))
ML_33 <- as.data.frame(sapply(ML_3, count3_ML))
ML_34 <- as.data.frame(sapply(ML_3, count4_ML))
ML_35 <- as.data.frame(sapply(ML_3, count5_ML))
ML_3n <- as.data.frame(sapply(ML_3, notemptycount_ML))

ML_freq_3 <- data.frame(ML_3n, ML_30, ML_31, ML_32, ML_33, ML_34, ML_35) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(Exp = 3, HMI = NA, variable = c("Uebel", "AnstrengendWenden"), .before = "n")



## combine all 6 conditions/subgroups + Uebel/AnstrengendWenden
ML_freq <- bind_rows(ML_freq_1HC, ML_freq_1LC, ML_freq_2HC, ML_freq_2LC, ML_freq_3HC, ML_freq_3LC,
                     ML_freq_1, ML_freq_2, ML_freq_3)
rownames(ML_freq) <- c()

#### remove not needed data ####
rm(list=setdiff(ls(), c("data_all", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim",
                        "ML_freq")))

#### save results ####
write_excel_csv(summ_vorbefragung, "data/results/summary_vorbefragung.csv")
write_excel_csv(summ_nachbefragung, "data/results/summary_nachbefragung.csv")
write_excel_csv(ML_freq, "data/results/frequencies.csv")