# # ---------------------------- preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(reshape)

setwd("~/R/Multilab_Analysis")

## functions
mean_ML = function(x) mean(x, na.rm = TRUE)
sd_ML = function(x) sd(x, na.rm = TRUE)
median_ML = function(x) median(x, na.rm = TRUE)
min_ML = function(x) min(x, na.rm = TRUE)
max_ML = function(x) max(x, na.rm = TRUE)

ML_skim <- skim_with(numeric = sfl(n = length, mean = mean_ML, sd = sd_ML, median = median_ML, min =  min_ML, max = max_ML), append = FALSE)

# write function for summary stats #
fun_mean <- function(x){ return(data.frame(y=mean(x),label=round(mean(x,na.rm=T), 2)))}

# import data ####
# Read in files and modify
## for now: left out is: starts_with("Automarke"))
data_TT_USA <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8") %>%
  select(Exp, VPNr, 
         Alter, Geschlecht,
         Nation, NationGeb, NationGeb2,
         Erfahrung, KilometerJahr, Vorwissen,
         FAS.CC, FAS.ACC, FAS.LKA, FAS.NoFAS,
         OftCC, OftACC, OftLKA,
         starts_with("VSM"),
         starts_with("UsabFact")) %>%         
  filter(Exp == 3) %>%
  mutate(Exp = "TT_USA") %>%
  dplyr::rename(Frequency = Erfahrung) %>%
  dplyr::rename(Mileage = KilometerJahr) %>%
  mutate(Nation_2nd = ifelse(Nation == "US and German", "German", 
                             ifelse(Nation == "american and german", "German", 
                                    ifelse(Nation == "USA and Germany", "German",
                                           ifelse(Nation == "Deutsch - Amerikanisch (USA)", "German", "")))), .after = "Nation") %>%
  mutate(Nation = 1)

data_ON_GER <- read.csv("data/raw/ON_GER.csv", encoding = "UTF-8") %>%
  dplyr::rename(VPNr = X.U.FEFF.id) %>%
  select(VPNr, 
         Alter, Geschlecht,
         Nation.1., Nation.other., NationGeb, NationGeb2,
         Erfahrung, KmJahr, Vorwissen.VorwissenAut.,
         FAS.CC., FAS.ACC., FAS.LKA., FAS.None.,
         OftCC, OftACC, OftLKA,
         # TPComments,
         starts_with("VSM"),
         starts_with("UsabFa")) %>%
  dplyr::rename(Frequency = Erfahrung) %>%
  dplyr::rename(Mileage = KmJahr) %>%
  dplyr::rename(Vorwissen = Vorwissen.VorwissenAut.) %>%
  dplyr::rename(FAS.CC = FAS.CC.) %>%
  dplyr::rename(FAS.ACC = FAS.ACC.) %>%
  dplyr::rename(FAS.LKA = FAS.LKA.) %>%
  dplyr::rename(FAS.NoFAS = FAS.None.) %>%
  filter(Nation.1. == "Y") %>%
  dplyr::rename(Nation = Nation.1.) %>%
  dplyr::rename(Nation_2nd = Nation.other.) %>%
  add_column(Exp = "ON_GER", .before = "VPNr") %>%
  mutate(Nation = 1) %>%
  mutate(NationGeb = ifelse(NationGeb == "Y", 1, 0)) %>%
  mutate(FAS.CC = ifelse(FAS.CC == "Y", 1, 0)) %>%
  mutate(FAS.ACC = ifelse(FAS.ACC == "Y", 1, 0)) %>%
  mutate(FAS.LKA = ifelse(FAS.LKA == "Y", 1, 0)) %>%
  mutate(FAS.NoFAS = ifelse(FAS.NoFAS == "Y", 1, 0))
  
data_ON_USA <- read.csv("data/raw/ON_USA.csv", encoding = "UTF-8") %>%
  dplyr::rename(VPNr = X.U.FEFF.id) %>%
  select(VPNr, 
         Alter, Geschlecht,
         Nation.1., Nation.other., NationGeb, NationGeb2,
         Erfahrung, KmJahr, Vorwissen.VorwissenAut.,
         FAS.CC., FAS.ACC., FAS.LKA., FAS.None.,
         OftCC, OftACC, OftLKA,
         # TPComments,
         starts_with("VSM"),
         starts_with("UsabFa")) %>%
  dplyr::rename(Frequency = Erfahrung) %>%
  dplyr::rename(Mileage = KmJahr) %>%
  dplyr::rename(Vorwissen = Vorwissen.VorwissenAut.) %>%
  dplyr::rename(FAS.CC = FAS.CC.) %>%
  dplyr::rename(FAS.ACC = FAS.ACC.) %>%
  dplyr::rename(FAS.LKA = FAS.LKA.) %>%
  dplyr::rename(FAS.NoFAS = FAS.None.) %>%
  filter(Nation.1. == "Y") %>%
  dplyr::rename(Nation = Nation.1.) %>%
  dplyr::rename(Nation_2nd = Nation.other.) %>%
  add_column(Exp = "ON_USA", .before = "VPNr") %>%
  mutate(Nation = 1) %>%
  mutate(NationGeb = ifelse(NationGeb == "Y", 1, 0)) %>%
  mutate(FAS.CC = ifelse(FAS.CC == "Y", 1, 0)) %>%
  mutate(FAS.ACC = ifelse(FAS.ACC == "Y", 1, 0)) %>%
  mutate(FAS.LKA = ifelse(FAS.LKA == "Y", 1, 0)) %>%
  mutate(FAS.NoFAS = ifelse(FAS.NoFAS == "Y", 1, 0))

## rename 
names <- c("Exp", "VPNr", "Alter", "Geschlecht", 
           "Nation", "Nation_2nd", "NationGeb", "NationGeb2", 
           "Frequency", "Mileage", "Vorwissen",
           "FAS.CC", "FAS.ACC", "FAS.LKA", "FAS.NoFAS", "OftCC", "OftACC", "OftLKA",
           "VSM_01", "VSM_02", "VSM_03", "VSM_04", "VSM_05", "VSM_06", "VSM_07", "VSM_08", 
           "VSM_09", "VSM_10", "VSM_11", "VSM_12", "VSM_13", "VSM_14", "VSM_15", "VSM_16", 
           "VSM_17", "VSM_18", "VSM_19", "VSM_20", "VSM_21", "VSM_22", "VSM_23", "VSM_24",
           "UsabF_01", "UsabF_02", "UsabF_03", "UsabF_04", "UsabF_05", "UsabF_06", 
           "UsabF_07", "UsabF_08", "UsabF_09", "UsabF_10", "UsabF_11", "UsabF_12", 
           "UsabF_13", "UsabF_14", "UsabF_15", "UsabF_16", "UsabF_17", "UsabF_18", 
           "UsabF_19", "UsabF_20", "UsabF_21", "UsabF_22", "UsabF_23", "UsabF_24")
names(data_TT_USA) <- names
names(data_ON_USA) <- names
names(data_ON_GER) <- names

## merge
data_all <- bind_rows (data_TT_USA, data_ON_GER, data_ON_USA) %>%
  mutate(Exp = factor(Exp, levels = c("TT_USA", "ON_USA", "ON_GER"), ordered = TRUE))

## delete subsets
rm(list = setdiff(ls(), c("data_all", 
                          "data_TT_USA", "data_ON_GER", "data_ON_USA",
                          "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim", "fun_mean")))

# # descriptive analysis --------------------------------------------------
sum_Geschlecht <- data_all %>%
  select(Exp, Geschlecht) %>%
  mutate_at("Geschlecht", factor) %>%
  dplyr::group_by(Exp) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

sum_Alter <- data_all %>%
  select(Exp, Alter) %>%
  dplyr::group_by(Exp) %>%
  ML_skim(.) %>% 
  select (-c(skim_type, complete_rate))


# # visualization age -----------------------------------------------------
# 123_SD_Age  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_all, aes(x=Exp, y=Alter, fill=Exp)) + 
  scale_y_continuous(limits = c(15,80), breaks = seq(20,80,10)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#E37222", "#FFC371", "#666666")) +
  labs(y="Years", x="",
       title = "Age") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.7, hjust=0.5) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p

ggsave(filename = "data/results/figures/00_Age.png", p, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# #### calculate VSM and create dataset ----------------------------------------------------------------
## constants
# may be schosen randomly in order to create scores between 0 and 100
c_PDI = +30
c_IDV = 15
c_MAS = +70
c_UAI = +130
c_LTO = +65
c_IVR = -405

## create means
data_all_VSM <- data_all %>%
  select(Exp, starts_with("VSM"))
summary_data_all_VSM <- data_all_VSM %>%
  dplyr::group_by(Exp) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate, n_missing, numeric.n)) %>%
  dplyr::rename(Item = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## subsets  
VSM_TT_USA <- summary_data_all_VSM %>%
  filter(Exp == "TT_USA")
VSM_ON_USA <- summary_data_all_VSM %>%
  filter(Exp == "ON_USA")
VSM_ON_GER <- summary_data_all_VSM %>%
  filter(Exp == "ON_GER")

## score calculation
# TT_USA
TT_USA_PDI = round(35 * (VSM_TT_USA$mean[7] - VSM_TT_USA$mean[2]) + 25 * (VSM_TT_USA$mean[20] - VSM_TT_USA$mean[23]) + c_PDI, 2)
TT_USA_IDV = round(25 * (VSM_TT_USA$mean[4] - VSM_TT_USA$mean[1]) + 25 * (VSM_TT_USA$mean[9] - VSM_TT_USA$mean[6]) + c_IDV, 2)
TT_USA_MAS = round(35 * (VSM_TT_USA$mean[5] - VSM_TT_USA$mean[3]) + 35 * (VSM_TT_USA$mean[8] - VSM_TT_USA$mean[10]) + c_MAS, 2)
TT_USA_UAI = round(40 * (VSM_TT_USA$mean[18] - VSM_TT_USA$mean[15]) + 25 * (VSM_TT_USA$mean[21] - VSM_TT_USA$mean[24]) + c_UAI, 2)
TT_USA_LTO = round(40 * (VSM_TT_USA$mean[13] - VSM_TT_USA$mean[14]) + 25 * (VSM_TT_USA$mean[19] - VSM_TT_USA$mean[22]) + c_LTO, 2)
TT_USA_IVR = round(35 * (VSM_TT_USA$mean[12] - VSM_TT_USA$mean[11]) + 340 * (VSM_TT_USA$mean[17] - VSM_TT_USA$mean[16]) + c_IVR, 2)

# ON_USA
ON_USA_PDI = round(35 * (VSM_ON_USA$mean[7] - VSM_ON_USA$mean[2]) + 25 * (VSM_ON_USA$mean[20] - VSM_ON_USA$mean[23]) + c_PDI, 2)
ON_USA_IDV = round(25 * (VSM_ON_USA$mean[4] - VSM_ON_USA$mean[1]) + 25 * (VSM_ON_USA$mean[9] - VSM_ON_USA$mean[6]) + c_IDV, 2)
ON_USA_MAS = round(35 * (VSM_ON_USA$mean[5] - VSM_ON_USA$mean[3]) + 35 * (VSM_ON_USA$mean[8] - VSM_ON_USA$mean[10]) + c_MAS, 2)
ON_USA_UAI = round(40 * (VSM_ON_USA$mean[18] - VSM_ON_USA$mean[15]) + 25 * (VSM_ON_USA$mean[21] - VSM_ON_USA$mean[24]) + c_UAI, 2)
ON_USA_LTO = round(40 * (VSM_ON_USA$mean[13] - VSM_ON_USA$mean[14]) + 25 * (VSM_ON_USA$mean[19] - VSM_ON_USA$mean[22]) + c_LTO, 2)
ON_USA_IVR = round(35 * (VSM_ON_USA$mean[12] - VSM_ON_USA$mean[11]) + 340 * (VSM_ON_USA$mean[17] - VSM_ON_USA$mean[16]) + c_IVR, 2)

# ON_GER
ON_GER_PDI = round(35 * (VSM_ON_GER$mean[7] - VSM_ON_GER$mean[2]) + 25 * (VSM_ON_GER$mean[20] - VSM_ON_GER$mean[23]) + c_PDI, 2)
ON_GER_IDV = round(25 * (VSM_ON_GER$mean[4] - VSM_ON_GER$mean[1]) + 25 * (VSM_ON_GER$mean[9] - VSM_ON_GER$mean[6]) + c_IDV, 2)
ON_GER_MAS = round(35 * (VSM_ON_GER$mean[5] - VSM_ON_GER$mean[3]) + 35 * (VSM_ON_GER$mean[8] - VSM_ON_GER$mean[10]) + c_MAS, 2)
ON_GER_UAI = round(40 * (VSM_ON_GER$mean[18] - VSM_ON_GER$mean[15]) + 25 * (VSM_ON_GER$mean[21] - VSM_ON_GER$mean[24]) + c_UAI, 2)
ON_GER_LTO = round(40 * (VSM_ON_GER$mean[13] - VSM_ON_GER$mean[14]) + 25 * (VSM_ON_GER$mean[19] - VSM_ON_GER$mean[22]) + c_LTO, 2)
ON_GER_IVR = round(35 * (VSM_ON_GER$mean[12] - VSM_ON_GER$mean[11]) + 340 * (VSM_ON_GER$mean[17] - VSM_ON_GER$mean[16]) + c_IVR, 2)

VSM_TT_USA <- tibble("Exp" = "TT_USA", "PDI" = TT_USA_PDI, "IDV" = TT_USA_IDV, "MAS" = TT_USA_MAS, 
                     "UAI" = TT_USA_UAI, "LTO" = TT_USA_LTO, "IVR" = TT_USA_IVR)
VSM_ON_USA <- tibble("Exp" = "ON_USA", "PDI" = ON_USA_PDI, "IDV" = ON_USA_IDV, "MAS" = ON_USA_MAS, 
                     "UAI" = ON_USA_UAI, "LTO" = ON_USA_LTO, "IVR" = ON_USA_IVR)
VSM_ON_GER <- tibble("Exp" = "ON_GER", "PDI" = ON_GER_PDI, "IDV" = ON_GER_IDV, "MAS" = ON_GER_MAS, 
                     "UAI" = ON_GER_UAI, "LTO" = ON_GER_LTO, "IVR" = ON_GER_IVR)

VSM <- bind_rows(VSM_TT_USA, VSM_ON_USA, VSM_ON_GER)

## delete subsets
rm(list = setdiff(ls(), c("data_all", 
                          "data_TT_USA", "data_ON_GER", "data_ON_USA",
                          "sum_Alter", "sum_Geschlecht",
                          "VSM",
                          "mean_ML", "sd_ML", "median_ML", "min_ML", "max_ML", "ML_skim")))

# #### visualization VSM -----------------------------------------------
VSM_long <- reshape2::melt(VSM, id = c("Exp"),
                measured = c("PDI", "IDV", "MAS", "UAI", "LTO", "IVR"))

names(VSM_long) <-c("Exp", "Dimension", "Score")

VSM_long <- VSM_long %>%
  mutate(Exp = factor(Exp, levels = c("TT_USA", "ON_USA", "ON_GER"), ordered = TRUE))

labels_VSM = c("Power Distance", "Individualism", "Masculinity", "Uncertainty Avoidance",
              "Long Term Orientation", "Indulgence vs. Restraint")

p <- ggplot(VSM_long, aes(x=Dimension, y = Score, fill=Exp)) + 
  geom_bar(stat = "identity", width = 0.7, position=position_dodge()) +
  scale_x_discrete(labels = labels_VSM) +
  scale_y_continuous(limits = c(0,105), breaks = seq(0,105, 20)) +
  scale_fill_manual(values = c("#E37222", "#FFC371", "#666666")) +
  labs(y="Index Score", x="",
       title = "Cultural Dimensions by Hofstede") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = c(0.1, 0.87), 
        legend.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=15, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

ggsave(filename = "data/results/figures/00_VSM.png", p, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

## VSM reference
Country <- c("GER", "GER", "GER", "GER", "GER", "GER",
             "USA", "USA", "USA", "USA", "USA", "USA")
Dimension <- c("PDI", "IDV", "MAS", "UAI", "LTO", "IVR",
               "PDI", "IDV", "MAS", "UAI", "LTO", "IVR")
Score <- c(35, 67, 66, 65, 83, 40, 
           40, 91, 62, 46, 26, 68)

VSM_ref = tibble(Country, Dimension, Score) %>%
  mutate(Country = factor(Country, levels = c("USA", "GER"), ordered = TRUE)) %>%
  mutate(Dimension = factor(Dimension, levels = c("PDI", "IDV", "MAS", "UAI", "LTO", "IVR"), ordered = TRUE))

p <- ggplot(VSM_ref, aes(x=Dimension, y = Score, fill=Country)) + 
  geom_bar(stat = "identity", width = 0.7, position=position_dodge()) +
  scale_x_discrete(labels = labels_VSM) +
  scale_y_continuous(limits = c(0,105), breaks = seq(0,105, 20)) +
  scale_fill_manual(values = c("#f5c88c", "#E2E2E2")) +
  labs(y="Index Score", x="",
       title = "Cultural Dimensions by Hofstede - Reference Data") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = c(0.1, 0.87), 
        legend.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=15, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

ggsave(filename = "data/results/figures/00_VSM_ref.png", p, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# #### visualization Usability Factors -----------------------------------------------
UsabF_agg <- data_all %>%
  select(Exp, starts_with("UsabF")) %>%
  dplyr::group_by(Exp) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate, n_missing, numeric.n)) %>%
  dplyr::rename(Factor = skim_variable) %>%
  dplyr::rename(mean = numeric.mean) %>%
  dplyr::rename(sd = numeric.sd) %>%
  mutate(mean = mean-3)

# labels_UsabF_asInSurvey <- c("Appearance_Attractiveness", "Completeness", 
#                              "Controllability_Dependability", "Convenience", 
#                              "Craftsmanship", "Ease of Use", "Efficiency", 
#                              "Emotion_Affect", "Flexibility", "Fun", "Helpfulness", 
#                              "Identity", "Immersion", "Intuitive Operation", 
#                              "Learnability_Perspicuity", "Loyalty", "Novelty", 
#                              "Originality", "Overall", "Pragmatic Quality", "Simplicity", 
#                              "Social Influence", "Stimulation", "Trust_Credibility")

## determine order of factors
UsabF_order <- data_all %>%
  select(starts_with("UsabF")) %>%
  ML_skim(.) %>%
  select (-c(skim_type, complete_rate, n_missing, numeric.n, numeric.sd)) %>%
  dplyr::rename(Factor = skim_variable) %>%
  dplyr::rename(mean = numeric.mean) %>%
  mutate(mean = mean-3)

## adjust order
UsabF_agg<- UsabF_agg %>%
  mutate(Exp = factor(Exp, levels = c("TT_USA", "ON_USA", "ON_GER"), ordered = TRUE)) %>%
  mutate(Factor = factor(Factor, levels = c("UsabF_03", "UsabF_06", "UsabF_24", "UsabF_20", 
                                            "UsabF_11", "UsabF_05", "UsabF_04", "UsabF_15", 
                                            "UsabF_02", "UsabF_21", "UsabF_07", "UsabF_14", 
                                            "UsabF_09", "UsabF_19", "UsabF_01", "UsabF_16", 
                                            "UsabF_23", "UsabF_10", "UsabF_08", "UsabF_12", 
                                            "UsabF_17", "UsabF_18", "UsabF_22", "UsabF_13")))

## add labels (in order!)
labels_UsabF <- c("Controllability /\nDependability", "Ease of Use", "Trust / Credibility", 
                  "Pragmatic Quality", "Helpfulness", "Craftsmanship", "Convenience", 
                  "Learnability / Perspicuity", "Completeness", "Simplicity", "Efficiency", 
                  "Intuitive Operation", "Flexibility", "Overall", "Appearance /\nAttractiveness", 
                  "Loyalty", "Stimulation", "Fun", "Emotion / Affect", "Identity", "Novelty", 
                  "Originality", "Social Influence", "Immersion")

## plot_allFactors
p <- ggplot(UsabF_agg, aes(x=Factor, y = mean, fill=Exp)) + 
  geom_bar(stat = "identity", width = 0.7, position=position_dodge()) +
  geom_errorbar(width=.7, aes(ymin = mean - sd, ymax = mean + sd), colour="grey", 
                size = 0.2, position=position_dodge()) +
  scale_x_discrete(labels = labels_UsabF) +
  scale_y_continuous(limits = c(-3, 3.5), breaks = seq(-3, 3, 1), 
                     labels = c("-3\nnot\nimportant\nat all", "-2", "-1", "0", "1", "2", "3\nvery\nimportant")) +
  scale_fill_manual(values = c("#E37222", "#FFC371", "#666666")) +
  labs(y="Importance score", x="",
       title = "Usability Factors") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = c(0.07, 0.15), 
        legend.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

ggsave(filename = "data/results/figures/00_UsabF_all.png", p, 
       width = 12, height = 6, dpi = 600, units = "in", device='png')

## plot_Factors_max
UsabF_agg_max <- UsabF_agg %>%
  filter(Factor == "UsabF_03" | Factor == "UsabF_06" | Factor == "UsabF_24" | Factor == "UsabF_20" | Factor == "UsabF_11" | Factor == "UsabF_05" | Factor == "UsabF_04" | Factor == "UsabF_15" | Factor == "UsabF_02" | Factor == "UsabF_21" | Factor == "UsabF_07" | Factor == "UsabF_14")

labels_UsabF_max <- c("Controllability /\nDependability", "Ease of Use", "Trust / Credibility", 
                  "Pragmatic Quality", "Helpfulness", "Craftsmanship", "Convenience", 
                  "Learnability / Perspicuity", "Completeness", "Simplicity", "Efficiency", 
                  "Intuitive Operation")
                  
p <- ggplot(UsabF_agg_max, aes(x=Factor, y = mean, fill=Exp)) + 
  geom_bar(stat = "identity", width = 0.7, position=position_dodge()) +
  geom_errorbar(width=.7, aes(ymin = mean - sd, ymax = mean + sd), colour="grey", 
                size = 0.2, position=position_dodge()) +
  scale_x_discrete(labels = labels_UsabF_max) +
  scale_y_continuous(limits = c(-3, 3.5), breaks = seq(-3, 3, 1), 
                     labels = c("-3\nnot\nimportant\nat all", "-2", "-1", "0", "1", "2", "3\nvery\nimportant")) +
  scale_fill_manual(values = c("#E37222", "#FFC371", "#666666")) +
  labs(y="Importance score", x="",
       title = "Usability Factors - most important factors") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = c(0.07, 0.15), 
        legend.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

ggsave(filename = "data/results/figures/00_UsabF_max.png", p, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

## plot_Factors_min
UsabF_agg_min <- UsabF_agg %>%
  filter(Factor == "UsabF_09" | Factor == "UsabF_19" | Factor == "UsabF_01" | Factor == "UsabF_16" | Factor == "UsabF_23" | Factor == "UsabF_10" | Factor == "UsabF_08" | Factor == "UsabF_12" | Factor == "UsabF_17" | Factor == "UsabF_18" | Factor == "UsabF_22" | Factor == "UsabF_13")

labels_UsabF_min <- c("Flexibility", "Overall", "Appearance /\nAttractiveness", 
                      "Loyalty", "Stimulation", "Fun", "Emotion / Affect", "Identity", "Novelty", 
                      "Originality", "Social Influence", "Immersion")

p <- ggplot(UsabF_agg_min, aes(x=Factor, y = mean, fill=Exp)) + 
  geom_bar(stat = "identity", width = 0.7, position=position_dodge()) +
  geom_errorbar(width=.7, aes(ymin = mean - sd, ymax = mean + sd), colour="grey", 
                size = 0.2, position=position_dodge()) +
  scale_x_discrete(labels = labels_UsabF_min) +
  scale_y_continuous(limits = c(-3, 3.5), breaks = seq(-3, 3, 1), 
                     labels = c("-3\nnot\nimportant\nat all", "-2", "-1", "0", "1", "2", "3\nvery\nimportant")) +
  scale_fill_manual(values = c("#E37222", "#FFC371", "#666666")) +
  labs(y="Importance score", x="",
       title = "Usability Factors - less important factors") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = c(0.07, 0.15), 
        legend.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

ggsave(filename = "data/results/figures/00_UsabF_min.png", p, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

## for scores: add skim functions (median, min, max); for VSM not useful, since the calculation is done with a mean