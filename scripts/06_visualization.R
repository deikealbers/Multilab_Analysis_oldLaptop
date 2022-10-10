# # ---------------------------- preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(skimr)
library(grid)
library(stringr)

setwd("~/R/Multilab_Analysis")

# write function for summary ####
# adjusted from skim() function
mean_ML = function(x) mean(x, na.rm = TRUE)
sd_ML = function(x) sd(x, na.rm = TRUE)
skim_ML <- skim_with(numeric = sfl(mean = mean_ML, sd = sd_ML), append = FALSE)

# write function for summary stats #
fun_mean <- function(x){ return(data.frame(y=mean(x),label=round(mean(x,na.rm=T), 2)))}
fun_median <- function(x){ return(data.frame(y=median(x),label=round(median(x,na.rm=T), 2)))}

# import data ####
# Read in files
data_all <- read.csv("data/preprocessed/Lime+AutLvl+UsabQ+shortI+ER_all.csv", encoding = "UTF-8")

summ_nachbefragung <- read.csv("data/results/summary_nachbefragung.csv", encoding = "UTF-8")

# adjust data sets: subsets, factors etc. ####
data <- data_all %>%
  select(Exp, HMI, VPNr, # meta
         # SD
         Alter, Erfahrung, KilometerJahr, Vorwissen,
         starts_with("FAS."), starts_with("Oft"),
         # other
         Wetter, Licht,
         # post questionnaires
         SUS_score, UMUX_score,
         Trust, Acceptance,
         starts_with("UEQ_"),
         AnstrengendWenden, Uebel,
         # short interviews
         LevelObserved_Rep_score,
         BothAllow_Observed_score, EmailsAllow_Observed_score, HandsOffAllow_Observed_score, 
         TransProblems_score,
         AvailImplem_Rep_score, AvailReasonCorrect_score, 
         LevelObserved_Instr_score, 
         ER_overall,
         ends_with("_ER"),
         ends_with("LevelObserved_Rep"), 
         ends_with("BothAllow_Observed"), ends_with("EmailsAllow_Observed"), ends_with("HandsOffAllow_Observed"), 
         ends_with("TransProblems"), 
         ends_with("AvailImplem_Rep"), ends_with("AvailReasonCorrect"), 
         ends_with("LevelObserved_Instr")
         ) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE)) %>%
  dplyr::rename(Age = Alter, Experience = Erfahrung, Mileage = KilometerJahr, Knowledge = Vorwissen) %>%
  dplyr::rename(Weather = Wetter, Light = Licht) %>%
  dplyr::rename(Effort = AnstrengendWenden, Nausea = Uebel) 

data_12 <- data %>%
  filter(Exp != "TT_USA")
data_23 <- data %>%
  filter(Exp != "Sim_GER")

## UEQ statistics  
UEQ_summ <- summ_nachbefragung %>%
  dplyr::rename(scale = X.U.FEFF.variable) %>%
  select(scale, Exp, HMI, mean, sd) %>%
  filter(str_detect(scale, "^UEQ")) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE))

UEQ_summ_12 <- UEQ_summ %>% 
  filter(Exp != "TT_USA")
UEQ_summ_23 <- UEQ_summ %>% 
  filter(Exp != "Sim_GER")

# # -------------------------------------------------------- 12 visualization #### -------------------------------------------------
# # ---------------------------- 12_SD #### -----------------------------------------------------
# Age
# Frequency
# Mileage
# Knowledge
# starts_with("FAS.")
# starts_with("Oft"),
# Weather
# Light

# 12_SD_Age  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Age, fill=HMI)) + 
  scale_y_continuous(limits = c(15,75), breaks = seq(20,70,10)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() + 
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Years", x="",
       title = "Age") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.4, hjust=0.5) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Age <- g

ggsave(filename = "data/results/figures/12_SD_Age.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_SD_Frequency  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Experience, fill=HMI)) + 
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, 1), 
                     labels = c("1 -\nnever",
                                "2 -\nless than\nonce a month", 
                                "3 -\nseveral times\na month", 
                                "4 -\nseveral times\na week", 
                                "5 -\nevery day")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Frequency: \n	How often do you drive a car?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=2.1, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Frequency <- g

ggsave(filename = "data/results/figures/12_SD_Frequency.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 12_SD_Mileage  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Mileage, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1),
                     labels = c("< 5.000 km",
                                "5.001 km -\n10.000 km",
                                "10.001 km -\n20.000 km",
                                ">20.000 km"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Mileage: \nHow many kilometers have you driven in the last 12 months?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1., hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Mileage <- g

ggsave(filename = "data/results/figures/12_SD_Mileage.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 12_SD_Knowledge  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Knowledge, fill=HMI)) + 
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, 1),
                     labels = c("0 - no\nknowledge",
                                "1",
                                "2",
                                "3",
                                "4 -\nexpert"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Prior Knowledge: \nDo you have prior knowledge in the field of automated driving?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Knowledge <- g

ggsave(filename = "data/results/figures/12_SD_Knowledge.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')
# 12_SD_FAS -----------------------------------------------------------
## subset subscales ##
FAS_CC <- data_12 %>%
  select(Exp, VPNr, HMI, FAS.CC) %>%
  dplyr::rename(score = FAS.CC) %>%
  add_column(scale = "FAS_CC", .after = "VPNr") 
FAS_ACC <- data_12 %>%
  select(Exp, VPNr, HMI, FAS.ACC) %>%
  dplyr::rename(score = FAS.ACC) %>%
  add_column(scale = "FAS_ACC", .after = "VPNr") 
FAS_LKA <- data_12 %>%
  select(Exp, VPNr, HMI, FAS.LKA) %>%
  dplyr::rename(score = FAS.LKA) %>%
  add_column(scale = "FAS_LKA", .after = "VPNr")
FAS_NoFAS <- data_12 %>%
  select(Exp, VPNr, HMI, FAS.NoFAS) %>%
  dplyr::rename(score = FAS.NoFAS) %>%
  add_column(scale = "FAS_NoFAS", .after = "VPNr")

## build subsets ##
FAS <- bind_rows(FAS_CC, FAS_ACC, FAS_LKA, FAS_NoFAS) %>%
  mutate(scale = factor(scale, levels = c("FAS_CC", "FAS_ACC", "FAS_LKA", "FAS_NoFAS"), ordered = TRUE))

FAS_perc <- FAS %>%
  select(-c(VPNr)) %>%
  group_by(Exp, HMI, scale) %>%
  skim_ML() %>%
  select(Exp, HMI, scale, numeric.mean) %>%
  dplyr::rename(mean = numeric.mean)

## lables ##
labels_FAS = c("CC", "ACC", "LKA", "No FAS")

## n ##
p <- ggplot(FAS, aes(x=scale, y = score, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_FAS) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Familiarity with ADAS [n]", x="",
       title = "Which driver assistance systems do you already have experience with?") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_FAS <- g

ggsave(filename = "data/results/figures/12_SD_FAS.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')


## Percentage ##
p <- ggplot(FAS_perc, aes(x=scale, y = mean*100, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_FAS) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, 20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Familiarity with ADAS [%]", x="",
       title = "Which driver assistance systems do you already have experience with?") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_FAS_perc <- g

ggsave(filename = "data/results/figures/12_SD_FAS_perc.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 12_SD_Oft_box -----------------------------------------------------------
## subset subscales ##
Oft_CC <- data_12 %>%
  select(Exp, VPNr, HMI, OftCC) %>%
  dplyr::rename(score = OftCC) %>%
  add_column(scale = "Oft_CC", .after = "VPNr")
Oft_ACC <- data_12 %>%
  select(Exp, VPNr, HMI, OftACC) %>%
  dplyr::rename(score = OftACC) %>%
  add_column(scale = "Oft_ACC", .after = "VPNr")
Oft_LKA <- data_12 %>%
  select(Exp, VPNr, HMI, OftLKA) %>%
  dplyr::rename(score = OftLKA) %>%
  add_column(scale = "Oft_LKA", .after = "VPNr")

## build subset ##
Oft <- bind_rows(Oft_CC, Oft_ACC, Oft_LKA) %>%
  mutate(scale = factor(scale, levels = c("Oft_CC", "Oft_ACC", "Oft_LKA"), ordered = TRUE))

## lables ##
labels_Oft = c("CC", "ACC", "LKA")

p <- ggplot(Oft, aes(x=scale, y=score, fill=HMI)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_Oft) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "seldom", "every month", "every week", "every day", "several times\na day")) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="", x="",
       title = "How often do you use the system ...") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Oft_box <- g

ggsave(filename = "data/results/figures/12_SD_Oft_box.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')
# 12_SD_Oft_dot -----------------------------------------------------------
## subset subscales ##
Oft_CC <- data_12 %>%
  select(Exp, VPNr, HMI, OftCC) %>%
  dplyr::rename(score = OftCC) %>%
  add_column(scale = "Oft_CC", .after = "VPNr")
Oft_ACC <- data_12 %>%
  select(Exp, VPNr, HMI, OftACC) %>%
  dplyr::rename(score = OftACC) %>%
  add_column(scale = "Oft_ACC", .after = "VPNr")
Oft_LKA <- data_12 %>%
  select(Exp, VPNr, HMI, OftLKA) %>%
  dplyr::rename(score = OftLKA) %>%
  add_column(scale = "Oft_LKA", .after = "VPNr")

## build subset ##
Oft <- bind_rows(Oft_CC, Oft_ACC, Oft_LKA) %>%
  mutate(scale = factor(scale, levels = c("Oft_CC", "Oft_ACC", "Oft_LKA"), ordered = TRUE))

## lables ##
labels_Oft = c("CC", "ACC", "LKA")

p <- ggplot(Oft, aes(x=scale, y=score, fill=HMI)) + 
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.35, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  scale_x_discrete(labels = labels_Oft) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "seldom", "every month", "every week", "every day", "several times\na day")) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="", x="",
       title = "How often do you use the system ...") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Oft_dot <- g

ggsave(filename = "data/results/figures/12_SD_Oft_dot.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')


# 12_SD_Weather  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Weather, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1),
                     labels = c("sunny,\nblue sky",
                                "lightly\nclouded",
                                "heavily\nclouded",
                                "light\nrain"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Weather conditions at beginning of experiment") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Weather <- g

ggsave(filename = "data/results/figures/12_SD_Weather.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')
# 12_SD_Light  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Light, fill=HMI)) + 
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 1),
                     labels = c("very bright,\nblinding",
                                "bright",
                                "gloomy,\ndusky"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.08, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Light conditions at beginning of experiment") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SD_Light <- g

ggsave(filename = "data/results/figures/12_SD_Light.png", g, 
       width = 8.5, height = 5, dpi = 300, units = "in", device='png')
# # ---------------------------- 12_post questionnaires #### -----------------------------------------------------
# contains
  # SUS
  # UMUX
  # Trust
  # Acceptance
  # UEQ_subscales

# 12_pQ_SUS  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=SUS_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  geom_violin(fill= "transparent") +
  # geom_boxplot(width = 0.4, notch = TRUE, notchwidth = 0.6, fill = "transparent", outlier.color = "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
                stackratio=1.3, dotsize=0.55, binwidth = 3.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Score", x="",
       title = "System Usability Scale") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=.2, hjust=1.7) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_SUS <- g

ggsave(filename = "data/results/figures/12_pQ_SUS.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_pQ_UMUX  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=UMUX_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  # geom_boxplot(width = 0.4, notch = TRUE, notchwidth = 0.6, fill = "transparent", outlier.color = "transparent") +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.55, binwidth = 3.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Score", x="",
       title = "Usability Metric of User Experience") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.7) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_UMUX <- g

ggsave(filename = "data/results/figures/12_pQ_UMUX.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_pQ_Acceptance  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=Acceptance, fill=HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 - \nstrongly \ndisagree",
                                "2", "3", "4", "5 - \nstrongly \nagree")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Acceptance: \nIf my car were fitted with a system like this, I’d use it when driving.") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1., hjust=0.7) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_Acceptance <- g

ggsave(filename = "data/results/figures/12_pQ_Acceptance.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 12_pQ_Trust  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x = HMI, y = Trust, fill = HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 - \nstrongly \ndisagree",
                                "2", "3", "4", "5 - \nstrongly \nagree")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Trust: \nI trusted the system I just used.") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.0) +
  stat_summary(fun = median, geom ="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size = 9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_Trust <- g

ggsave(filename = "data/results/figures/12_pQ_Trust.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')



# 12_pQ_UEQ_boxplot -----------------------------------------------------------
## subset subscales ##
UEQ_Attractiveness <- data_12 %>%
  select(Exp, VPNr, HMI, UEQ_Attractiveness) %>%
  dplyr::rename(score = UEQ_Attractiveness) %>%
  add_column(scale = "UEQ_Attractiveness", .after = "VPNr")
UEQ_Perspicuity <- data_12 %>%
  select(Exp, VPNr, HMI, UEQ_Perspicuity) %>%
  dplyr::rename(score = UEQ_Perspicuity) %>%
  add_column(scale = "UEQ_Perspicuity", .after = "VPNr")
UEQ_Efficiency <- data_12 %>%
  select(Exp, VPNr, HMI, UEQ_Efficiency) %>%
  dplyr::rename(score = UEQ_Efficiency) %>%
  add_column(scale = "UEQ_Efficiency", .after = "VPNr")
UEQ_Dependability <- data_12 %>%
  select(Exp, VPNr, HMI, UEQ_Dependability) %>%
  dplyr::rename(score = UEQ_Dependability) %>%
  add_column(scale = "UEQ_Dependability", .after = "VPNr")
UEQ_Stimulation <- data_12 %>%
  select(Exp, VPNr, HMI, UEQ_Stimulation) %>%
  dplyr::rename(score = UEQ_Stimulation) %>%
  add_column(scale = "UEQ_Stimulation", .after = "VPNr")
UEQ_Novelty <- data_12 %>%
  select(Exp, VPNr, HMI, UEQ_Novelty) %>%
  dplyr::rename(score = UEQ_Novelty) %>%
  add_column(scale = "UEQ_Novelty", .after = "VPNr")
## build subset ##
UEQ_12 <- bind_rows(UEQ_Attractiveness, UEQ_Perspicuity, UEQ_Efficiency, UEQ_Dependability, UEQ_Stimulation, UEQ_Novelty) %>%
  mutate(scale = factor(scale, levels = c("UEQ_Attractiveness", "UEQ_Perspicuity", "UEQ_Efficiency", "UEQ_Dependability", "UEQ_Stimulation", "UEQ_Novelty"), ordered = TRUE))

## lables ##
labels_UEQ = c("Attractiveness", "Perspicuity", "Efficiency", "Dependability", "Stimulation", "Novelty")

p <- ggplot(UEQ_12, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .8, ymax = 3), 
            fill = alpha("#92C46E", 0.02)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.8, ymax = .8), 
            fill = alpha("#E9E898", 0.02)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -3, ymax = -.8), 
            fill = alpha("#F09C9A", 0.02)) + #E86865
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_UEQ) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Score", x="",
       title = "User Experience Questionnaire") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_UEQ_box <- g

ggsave(filename = "data/results/figures/12_pQ_UEQ_box.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 12_pQ_UEQ_barplot -----------------------------------------------------------
p <- ggplot(UEQ_summ_12, aes(x=as.factor(scale), y = mean, fill=as.factor(HMI) )) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .8, ymax = 3), 
            fill = alpha("#92C46E", 0.3)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.8, ymax = .8), 
            fill = alpha("#E9E898", 0.3)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -3, ymax = -.8), 
            fill = alpha("#F09C9A", 0.3)) + #E86865
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(width=.25, aes(ymin = mean - sd, ymax = mean + sd), colour="black", size = 0.2) +
  scale_x_discrete(labels = labels_UEQ) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Score", x="",
       title = "User Experience Questionnaire") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_UEQ_bar <- g

ggsave(filename = "data/results/figures/12_pQ_UEQ_bar.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 12_pQ_Nausea  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x = HMI, y = Nausea, fill = HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 -\nnot at all \nnauseous",
                                "2", "3", "4", "5 -\nvery nauseous")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.1, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Nausea: \nDid the turning make you nauseous?") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.3) +
  stat_summary(fun = median, geom ="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size = 9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_Nausea <- g

ggsave(filename = "data/results/figures/12_pQ_Nausea.png", g, 
       width = 7.5, height = 5, dpi = 300, units = "in", device='png')



# 12_pQ_Effort  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_12, aes(x = HMI, y = Effort, fill = HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 -\nnot at all \nstrenuous",
                                "2", "3", "4", "5 -\nvery strenuous")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.1, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Effort: \nHow strenuous did you find turning at the end of the test track?") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.3) +
  stat_summary(fun = median, geom ="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size = 9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_pQ_Effort <- g

ggsave(filename = "data/results/figures/12_pQ_Effort.png", g, 
       width = 7.5, height = 5, dpi = 300, units = "in", device='png')

# # ---------------------------- 12_short interviews #### ------------------------------------------------
# 12_SI_LevelObserved_Rep_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=LevelObserved_Rep_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported automation level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_LevelObserved_Rep <- g

ggsave(filename = "data/results/figures/12_SI_LevelObserved_Rep.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_SI_LevelObserved_Rep_singleTC ------------------------------------
## build subset ##
d12_SI_LevelObserved_Rep_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("LevelObserved_Rep")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_LevelObserved_Rep_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. reported automation level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_LevelObserved_Rep_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_LevelObserved_Rep_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_BothAllow_Observed_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=BothAllow_Observed_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,24), breaks = seq(0,24, 4)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported allowance to: \nDrive hands-off and write e-mails in this level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_BothAllow_Observed <- g

ggsave(filename = "data/results/figures/12_SI_BothAllow_Observed.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_EmailsAllow_Observed_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=EmailsAllow_Observed_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported allowance to: \nWrite e-mails while driving in this level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_EmailsAllow_Observed <- g

ggsave(filename = "data/results/figures/12_SI_EmailsAllow_Observed.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_EmailsAllow_Observed_singleTC ------------------------------------
## build subset ##
d12_SI_EmailsAllow_Observed_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("EmailsAllow_Observed")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_EmailsAllow_Observed_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. reported allowance to: \nWrite e-mails while driving in this level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_EmailsAllow_Observed_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_EmailsAllow_Observed_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')
# 12_SI_HandsOffAllow_Observed_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=HandsOffAllow_Observed_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported allowance to: \nDrive hands-off in this level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_HandsOffAllow_Observed <- g

ggsave(filename = "data/results/figures/12_SI_HandsOffAllow_Observed.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_HandsOffAllow_Observed_singleTC ------------------------------------
## build subset ##
d12_SI_HandsOffAllow_Observed_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("HandsOffAllow_Observed")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_HandsOffAllow_Observed_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. reported allowance to: \nDrive hands-off in this level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_HandsOffAllow_Observed_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_HandsOffAllow_Observed_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_TransProblems_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=TransProblems_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,6), breaks = seq(0,6, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of mentions", x="",
       title = "Reported problems with switching automation levels") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1, hjust=1.2) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_TransProblems <- g

ggsave(filename = "data/results/figures/12_SI_TransProblems.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_TransProblems_singleTC ------------------------------------
## build subset ##
d12_SI_TransProblems_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("TransProblems")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_TransProblems_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of mentions per test case [%]", x="Test case", 
       title = "Reported problems with switching automation levels") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("3", "5", "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(.9, 0.9), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_TransProblems_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_TransProblems_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_SI_AvailImplem_Rep_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=AvailImplem_Rep_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Implemented change of available levels vs. \nreported availability change") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_AvailImplem_Rep <- g

ggsave(filename = "data/results/figures/12_SI_AvailImplem_Rep.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_AvailImplem_Rep_singleTC ------------------------------------
## build subset ##
d12_SI_AvailImplem_Rep_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("AvailImplem_Rep")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_AvailImplem_Rep_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Implemented change of available levels vs. \nreported availability change") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_AvailImplem_Rep_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_AvailImplem_Rep_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_SI_AvailReasonCorrect_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=AvailReasonCorrect_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Implemented reason for change of available levels vs. \nreported reason for change of available levels") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.3, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_AvailReasonCorrect <- g

ggsave(filename = "data/results/figures/12_SI_AvailReasonCorrect.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_AvailReasonCorrect_singleTC ------------------------------------
## build subset ##
d12_SI_AvailReasonCorrect_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("AvailReasonCorrect")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_AvailReasonCorrect_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Implemented reason for change of available levels vs. \nreported reason for change of available levels") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("6", "10", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_AvailReasonCorrect_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_AvailReasonCorrect_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 12_SI_LevelObserved_Instr_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=LevelObserved_Instr_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. instructed automation level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.9, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_LevelObserved_Instr <- g

ggsave(filename = "data/results/figures/12_SI_LevelObserved_Instr.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_SI_LevelObserved_Instr_singleTC ------------------------------------
## build subset ##
d12_SI_LevelObserved_Instr_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("LevelObserved_Instr")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_SI_LevelObserved_Instr_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. instructed automation level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_SI_LevelObserved_Instr_singleTC <- g

ggsave(filename = "data/results/figures/12_SI_LevelObserved_Instr_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


#  # ---------------------------- 12_experimenter rating #### ---------------------------------------------
# 12_ER_ExpRating_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_12, aes(x=HMI, y=ER_overall, fill=HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), labels = c("1 - \nno problem",
                                                                       "2 - \nhesitation",
                                                                       "3 - \nminor errors",
                                                                       "4 - \nmajor errors",
                                                                       "5 - \nhelp of \nexperimenter")) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Experimenter Rating") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.1, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_ER <- g

ggsave(filename = "data/results/figures/12_ER.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 12_ER_ExpRating_singleTC ------------------------------------
## build subset ##
d12_ER_ExpRating_singleTC <- data_12 %>%
  select(Exp, HMI, ends_with("_ER")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d12_ER_ExpRating_singleTC, aes(x = TC, y=mean)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="", x="Test case", 
       title = "Mean score in experimeter rating") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), labels = c("1 - \nno problem",
                                                                       "2 - \nhesitation",
                                                                       "3 - \nminor errors",
                                                                       "4 - \nmajor errors",
                                                                       "5 - \nhelp of \nexperimenter")) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.9), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00") #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_ER_singleTC <- g

ggsave(filename = "data/results/figures/12_ER_singleTC.png", g, 
       width = 7, height = 6, dpi = 300, units = "in", device='png')


# # -------------------------------------------------------- 23 visualization #### -------------------------------------------------
# # ---------------------------- 23_SD #### -----------------------------------------------------
# Age
# Frequency
# Mileage
# Knowledge
# starts_with("FAS.")
# starts_with("Oft"),
# Weather
# Light

# 23_SD_Age  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Age, fill=HMI)) + 
  scale_y_continuous(limits = c(15,75), breaks = seq(20,70,10)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() + 
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
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
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Age <- g

ggsave(filename = "data/results/figures/23_SD_Age.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_SD_Frequency  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Experience, fill=HMI)) + 
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, 1), 
                     labels = c("1 -\nnever",
                                "2 -\nless than\nonce a month", 
                                "3 -\nseveral times\na month", 
                                "4 -\nseveral times\na week", 
                                "5 -\nevery day")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Frequency: \n	How often do you drive a car?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=2.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Frequency <- g

ggsave(filename = "data/results/figures/23_SD_Frequency.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 23_SD_Mileage  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Mileage, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1),
                     labels = c("< 5.000 km",
                                "5.001 km -\n10.000 km",
                                "10.001 km -\n20.000 km",
                                ">20.000 km"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Mileage: \nHow many kilometers have you driven in the last 12 months?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1., hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Mileage <- g

ggsave(filename = "data/results/figures/23_SD_Mileage.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 23_SD_Knowledge  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Knowledge, fill=HMI)) + 
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, 1),
                     labels = c("0 - no\nknowledge",
                                "1",
                                "2",
                                "3",
                                "4 -\nexpert"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Prior Knowledge: \nDo you have prior knowledge in the field of automated driving?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Knowledge <- g

ggsave(filename = "data/results/figures/23_SD_Knowledge.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')
# 23_SD_FAS -----------------------------------------------------------
## subset subscales ##
FAS_CC <- data_23 %>%
  select(Exp, VPNr, HMI, FAS.CC) %>%
  dplyr::rename(score = FAS.CC) %>%
  add_column(scale = "FAS_CC", .after = "VPNr")
FAS_ACC <- data_23 %>%
  select(Exp, VPNr, HMI, FAS.ACC) %>%
  dplyr::rename(score = FAS.ACC) %>%
  add_column(scale = "FAS_ACC", .after = "VPNr")
FAS_LKA <- data_23 %>%
  select(Exp, VPNr, HMI, FAS.LKA) %>%
  dplyr::rename(score = FAS.LKA) %>%
  add_column(scale = "FAS_LKA", .after = "VPNr")
FAS_NoFAS <- data_23 %>%
  select(Exp, VPNr, HMI, FAS.NoFAS) %>%
  dplyr::rename(score = FAS.NoFAS) %>%
  add_column(scale = "FAS_NoFAS", .after = "VPNr")

## build subsets ##
FAS <- bind_rows(FAS_CC, FAS_ACC, FAS_LKA, FAS_NoFAS) %>%
  mutate(scale = factor(scale, levels = c("FAS_CC", "FAS_ACC", "FAS_LKA", "FAS_NoFAS"), ordered = TRUE))

FAS_perc <- FAS %>%
  select(-c(VPNr)) %>%
  group_by(Exp, HMI, scale) %>%
  skim_ML() %>%
  select(Exp, HMI, scale, numeric.mean) %>%
  dplyr::rename(mean = numeric.mean)

## lables ##
labels_FAS = c("CC", "ACC", "LKA", "No FAS")

## n ##
p <- ggplot(FAS, aes(x=scale, y = score, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_FAS) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Familiarity with ADAS [n]", x="",
       title = "Which driver assistance systems do you already have experience with?") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#A2AD00", "#E37222","#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_FAS <- g

ggsave(filename = "data/results/figures/23_SD_FAS.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')


## Percentage ##
p <- ggplot(FAS_perc, aes(x=scale, y = mean*100, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_FAS) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, 20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Familiarity with ADAS [%]", x="",
       title = "Which driver assistance systems do you already have experience with?") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#A2AD00", "#E37222","#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_FAS_perc <- g

ggsave(filename = "data/results/figures/23_SD_FAS_perc.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 23_SD_Oft_box -----------------------------------------------------------
## subset subscales ##
Oft_CC <- data_23 %>%
  select(Exp, VPNr, HMI, OftCC) %>%
  dplyr::rename(score = OftCC) %>%
  add_column(scale = "Oft_CC", .after = "VPNr")
Oft_ACC <- data_23 %>%
  select(Exp, VPNr, HMI, OftACC) %>%
  dplyr::rename(score = OftACC) %>%
  add_column(scale = "Oft_ACC", .after = "VPNr")
Oft_LKA <- data_23 %>%
  select(Exp, VPNr, HMI, OftLKA) %>%
  dplyr::rename(score = OftLKA) %>%
  add_column(scale = "Oft_LKA", .after = "VPNr")

## build subset ##
Oft <- bind_rows(Oft_CC, Oft_ACC, Oft_LKA) %>%
  mutate(scale = factor(scale, levels = c("Oft_CC", "Oft_ACC", "Oft_LKA"), ordered = TRUE))

## lables ##
labels_Oft = c("CC", "ACC", "LKA")

p <- ggplot(Oft, aes(x=scale, y=score, fill=HMI)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_Oft) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "seldom", "every month", "every week", "every day", "several times\na day")) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="", x="",
       title = "How often do you use the system ...") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#A2AD00", "#E37222","#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Oft_box <- g

ggsave(filename = "data/results/figures/23_SD_Oft_box.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')
# 23_SD_Oft_dot -----------------------------------------------------------
## subset subscales ##
Oft_CC <- data_23 %>%
  select(Exp, VPNr, HMI, OftCC) %>%
  dplyr::rename(score = OftCC) %>%
  add_column(scale = "Oft_CC", .after = "VPNr")
Oft_ACC <- data_23 %>%
  select(Exp, VPNr, HMI, OftACC) %>%
  dplyr::rename(score = OftACC) %>%
  add_column(scale = "Oft_ACC", .after = "VPNr")
Oft_LKA <- data_23 %>%
  select(Exp, VPNr, HMI, OftLKA) %>%
  dplyr::rename(score = OftLKA) %>%
  add_column(scale = "Oft_LKA", .after = "VPNr")

## build subset ##
Oft <- bind_rows(Oft_CC, Oft_ACC, Oft_LKA) %>%
  mutate(scale = factor(scale, levels = c("Oft_CC", "Oft_ACC", "Oft_LKA"), ordered = TRUE))

## lables ##
labels_Oft = c("CC", "ACC", "LKA")

p <- ggplot(Oft, aes(x=scale, y=score, fill=HMI)) + 
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.35, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  scale_x_discrete(labels = labels_Oft) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "seldom", "every month", "every week", "every day", "several times\na day")) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="", x="",
       title = "How often do you use the system ...") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#A2AD00", "#E37222","#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Oft_dot <- g

ggsave(filename = "data/results/figures/23_SD_Oft_dot.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')


# 23_SD_Weather  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Weather, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1),
                     labels = c("sunny,\nblue sky",
                                "lightly\nclouded",
                                "heavily\nclouded",
                                "light\nrain"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Weather conditions at beginning of experiment") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Weather <- g

ggsave(filename = "data/results/figures/23_SD_Weather.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')
# 23_SD_Light  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Light, fill=HMI)) + 
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 1),
                     labels = c("very bright,\nblinding",
                                "bright",
                                "gloomy,\ndusky"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.08, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Light conditions at beginning of experiment") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SD_Light <- g

ggsave(filename = "data/results/figures/23_SD_Light.png", g, 
       width = 8.5, height = 5, dpi = 300, units = "in", device='png')


# # ---------------------------- 23_post questionnaires #### -----------------------------------------------------
# contains
# SUS
# UMUX
# Trust
# Acceptance
# UEQ_subscales

# 23_pQ_SUS  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=SUS_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  geom_violin(fill= "transparent") +
  # geom_boxplot(width = 0.4, notch = TRUE, notchwidth = 0.6, fill = "transparent", outlier.color = "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.55, binwidth = 3.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Score", x="",
       title = "System Usability Scale") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=.3, hjust=1.7) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_SUS <- g

ggsave(filename = "data/results/figures/23_pQ_SUS.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_pQ_UMUX  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=UMUX_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  # geom_boxplot(width = 0.4, notch = TRUE, notchwidth = 0.6, fill = "transparent", outlier.color = "transparent") +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.55, binwidth = 3.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Score", x="",
       title = "Usability Metric of User Experience") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.7) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_UMUX <- g

ggsave(filename = "data/results/figures/23_pQ_UMUX.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_pQ_Acceptance  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=Acceptance, fill=HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 - \nstrongly \ndisagree",
                                "2", "3", "4", "5 - \nstrongly \nagree")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Acceptance: \nIf my car were fitted with a system like this, I’d use it when driving.") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1., hjust=0.7) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_Acceptance <- g

ggsave(filename = "data/results/figures/23_pQ_Acceptance.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 23_pQ_Trust  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x = HMI, y = Trust, fill = HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 - \nstrongly \ndisagree",
                                "2", "3", "4", "5 - \nstrongly \nagree")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Trust: \nI trusted the system I just used.") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.5, hjust=.5) +
  stat_summary(fun = median, geom ="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size = 9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_Trust <- g

ggsave(filename = "data/results/figures/23_pQ_Trust.png", g, 
       width = 6.5, height = 5, dpi = 300, units = "in", device='png')

# 23_pQ_UEQ_boxplot -----------------------------------------------------------
## subset subscales ##
UEQ_Attractiveness <- data_23 %>%
  select(Exp, VPNr, HMI, UEQ_Attractiveness) %>%
  dplyr::rename(score = UEQ_Attractiveness) %>%
  add_column(scale = "UEQ_Attractiveness", .after = "VPNr")
UEQ_Perspicuity <- data_23 %>%
  select(Exp, VPNr, HMI, UEQ_Perspicuity) %>%
  dplyr::rename(score = UEQ_Perspicuity) %>%
  add_column(scale = "UEQ_Perspicuity", .after = "VPNr")
UEQ_Efficiency <- data_23 %>%
  select(Exp, VPNr, HMI, UEQ_Efficiency) %>%
  dplyr::rename(score = UEQ_Efficiency) %>%
  add_column(scale = "UEQ_Efficiency", .after = "VPNr")
UEQ_Dependability <- data_23 %>%
  select(Exp, VPNr, HMI, UEQ_Dependability) %>%
  dplyr::rename(score = UEQ_Dependability) %>%
  add_column(scale = "UEQ_Dependability", .after = "VPNr")
UEQ_Stimulation <- data_23 %>%
  select(Exp, VPNr, HMI, UEQ_Stimulation) %>%
  dplyr::rename(score = UEQ_Stimulation) %>%
  add_column(scale = "UEQ_Stimulation", .after = "VPNr")
UEQ_Novelty <- data_23 %>%
  select(Exp, VPNr, HMI, UEQ_Novelty) %>%
  dplyr::rename(score = UEQ_Novelty) %>%
  add_column(scale = "UEQ_Novelty", .after = "VPNr")
## build subset ##
UEQ_23 <- bind_rows(UEQ_Attractiveness, UEQ_Perspicuity, UEQ_Efficiency, UEQ_Dependability, UEQ_Stimulation, UEQ_Novelty) %>%
  mutate(scale = factor(scale, levels = c("UEQ_Attractiveness", "UEQ_Perspicuity", "UEQ_Efficiency", "UEQ_Dependability", "UEQ_Stimulation", "UEQ_Novelty"), ordered = TRUE))

## lables ##
labels_UEQ = c("Attractiveness", "Perspicuity", "Efficiency", "Dependability", "Stimulation", "Novelty")

p <- ggplot(UEQ_23, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .8, ymax = 3), 
            fill = alpha("#92C46E", 0.02)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.8, ymax = .8), 
            fill = alpha("#E9E898", 0.02)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -3, ymax = -.8), 
            fill = alpha("#F09C9A", 0.02)) + #E86865
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_UEQ) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Score", x="",
       title = "User Experience Questionnaire") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#A2AD00", "#E37222","#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_UEQ_box <- g

ggsave(filename = "data/results/figures/23_pQ_UEQ_box.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 23_pQ_UEQ_barplot -----------------------------------------------------------
p <- ggplot(UEQ_summ_23, aes(x=as.factor(scale), y = mean, fill=as.factor(HMI) )) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .8, ymax = 3), 
            fill = alpha("#92C46E", 0.3)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.8, ymax = .8), 
            fill = alpha("#E9E898", 0.3)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -3, ymax = -.8), 
            fill = alpha("#F09C9A", 0.3)) + #E86865
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(width=.25, aes(ymin = mean - sd, ymax = mean + sd), colour="black", size = 0.2) +
  scale_x_discrete(labels = labels_UEQ) +
  scale_y_continuous(limits = c(-3,3.19), breaks = seq(-3,3,1)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Score", x="",
       title = "User Experience Questionnaire") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#A2AD00", "#E37222","#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_UEQ_bar <- g

ggsave(filename = "data/results/figures/23_pQ_UEQ_bar.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 23_pQ_Nausea  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x = HMI, y = Nausea, fill = HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 -\nnot at all \nnauseous",
                                "2", "3", "4", "5 -\nvery nauseous")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.1, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Nausea: \nDid the turning make you nauseous?") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.3) +
  stat_summary(fun = median, geom ="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size = 9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_Nausea <- g

ggsave(filename = "data/results/figures/23_pQ_Nausea.png", g, 
       width = 7.5, height = 5, dpi = 300, units = "in", device='png')



# 23_pQ_Effort  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data_23, aes(x = HMI, y = Effort, fill = HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), 
                     labels = c("1 -\nnot at all \nstrenuous",
                                "2", "3", "4", "5 -\nvery strenuous")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.1, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Effort: \nHow strenuous did you find turning at the end of the test track?") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.3) +
  stat_summary(fun = median, geom ="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size = 9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_pQ_Effort <- g

ggsave(filename = "data/results/figures/23_pQ_Effort.png", g, 
       width = 7.5, height = 5, dpi = 300, units = "in", device='png')

# # ---------------------------- 23_short interviews #### ------------------------------------------------
# 23_SI_LevelObserved_Rep_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=LevelObserved_Rep_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported automation level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_LevelObserved_Rep <- g

ggsave(filename = "data/results/figures/23_SI_LevelObserved_Rep.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_SI_LevelObserved_Rep_singleTC ------------------------------------
## build subset ##
d23_SI_LevelObserved_Rep_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("LevelObserved_Rep")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_LevelObserved_Rep_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. reported automation level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_LevelObserved_Rep_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_LevelObserved_Rep_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_BothAllow_Observed_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=BothAllow_Observed_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,24), breaks = seq(0,24, 4)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported allowance to: \nDrive hands-off and write e-mails in this level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.2, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_BothAllow_Observed <- g

ggsave(filename = "data/results/figures/23_SI_BothAllow_Observed.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_EmailsAllow_Observed_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=EmailsAllow_Observed_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="", 
       title = "Observed automation level vs. reported allowance to: \nWrite e-mails while driving in this level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.7, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_EmailsAllow_Observed <- g

ggsave(filename = "data/results/figures/23_SI_EmailsAllow_Observed.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_EmailsAllow_Observed_singleTC ------------------------------------
## build subset ##
d23_SI_EmailsAllow_Observed_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("EmailsAllow_Observed")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_EmailsAllow_Observed_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. reported allowance to: \nWrite e-mails while driving in this level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_EmailsAllow_Observed_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_EmailsAllow_Observed_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')
# 23_SI_HandsOffAllow_Observed_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=HandsOffAllow_Observed_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. reported allowance to: \nDrive hands-off in this level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0.9, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_HandsOffAllow_Observed <- g

ggsave(filename = "data/results/figures/23_SI_HandsOffAllow_Observed.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_HandsOffAllow_Observed_singleTC ------------------------------------
## build subset ##
d23_SI_HandsOffAllow_Observed_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("HandsOffAllow_Observed")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_HandsOffAllow_Observed_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. reported allowance to: \nDrive hands-off in this level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_HandsOffAllow_Observed_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_HandsOffAllow_Observed_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_TransProblems_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=TransProblems_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,6), breaks = seq(0,6, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of mentions", x="",
       title = "Reported problems with switching automation levels") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1, hjust=1.2) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_TransProblems <- g

ggsave(filename = "data/results/figures/23_SI_TransProblems.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_TransProblems_singleTC ------------------------------------
## build subset ##
d23_SI_TransProblems_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("TransProblems")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_TransProblems_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of mentions per test case [%]", x="Test case", 
       title = "Reported problems with switching automation levels") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("3", "5", "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(.9, 0.9), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_TransProblems_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_TransProblems_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_SI_AvailImplem_Rep_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=AvailImplem_Rep_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Implemented change of available levels vs. \nreported availability change") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.8, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_AvailImplem_Rep <- g

ggsave(filename = "data/results/figures/23_SI_AvailImplem_Rep.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_SI_AvailImplem_Rep_singleTC ------------------------------------
## build subset ##
d23_SI_AvailImplem_Rep_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("AvailImplem_Rep")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_AvailImplem_Rep_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Implemented change of available levels vs. \nreported availability change") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_AvailImplem_Rep_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_AvailImplem_Rep_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_SI_AvailReasonCorrect_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=AvailReasonCorrect_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Implemented reason for change of available levels vs. \nreported reason for change of available levels") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.3, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_AvailReasonCorrect <- g

ggsave(filename = "data/results/figures/23_SI_AvailReasonCorrect.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_AvailReasonCorrect_singleTC ------------------------------------
## build subset ##
d23_SI_AvailReasonCorrect_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("AvailReasonCorrect")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_AvailReasonCorrect_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Implemented reason for change of available levels vs. \nreported reason for change of available levels") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("6", "10", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_AvailReasonCorrect_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_AvailReasonCorrect_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


# 23_SI_LevelObserved_Instr_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=LevelObserved_Instr_score, fill=HMI)) + 
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="Number of matching answers", x="",
       title = "Observed automation level vs. instructed automation level") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.9, hjust=1.1) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_LevelObserved_Instr <- g

ggsave(filename = "data/results/figures/23_SI_LevelObserved_Instr.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_SI_LevelObserved_Instr_singleTC ------------------------------------
## build subset ##
d23_SI_LevelObserved_Instr_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("LevelObserved_Instr")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_SI_LevelObserved_Instr_singleTC, aes(x = TC, y=mean*100)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="Proportion of matching answers per test case [%]", x="Test case", 
       title = "Observed automation level vs. instructed automation level") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.1), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_SI_LevelObserved_Instr_singleTC <- g

ggsave(filename = "data/results/figures/23_SI_LevelObserved_Instr_singleTC.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')


#  # ---------------------------- 23_experimenter rating #### ---------------------------------------------
# 23_ER_ExpRating_score ----------------------------------------------
## basic plot ##
p <- ggplot(data_23, aes(x=HMI, y=ER_overall, fill=HMI)) + 
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), labels = c("1 - \nno problem",
                                                                       "2 - \nhesitation",
                                                                       "3 - \nminor errors",
                                                                       "4 - \nmajor errors",
                                                                       "5 - \nhelp of \nexperimenter")) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Experimenter Rating") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.1, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_ER <- g

ggsave(filename = "data/results/figures/23_ER.png", g, 
       width = 6, height = 6, dpi = 300, units = "in", device='png')

# 23_ER_ExpRating_singleTC ------------------------------------
## build subset ##
d23_ER_ExpRating_singleTC <- data_23 %>%
  select(Exp, HMI, ends_with("_ER")) %>%
  group_by(Exp, HMI) %>%
  skim_ML() %>%
  select(Exp, HMI, skim_variable, numeric.mean) %>%
  dplyr::rename(TC = skim_variable) %>%
  dplyr::rename(mean = numeric.mean)

## basic plot ##
p <- ggplot(d23_ER_ExpRating_singleTC, aes(x = TC, y=mean)) +
  geom_line(aes(group = HMI, color = HMI), size = 0.8) +
  geom_point(aes(color = HMI), size = 2.5) +
  labs(y="", x="Test case", 
       title = "Mean score in experimeter rating") +
  facet_grid(~Exp) +
  scale_color_manual(values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5, 1), labels = c("1 - \nno problem",
                                                                       "2 - \nhesitation",
                                                                       "3 - \nminor errors",
                                                                       "4 - \nmajor errors",
                                                                       "5 - \nhelp of \nexperimenter")) +
  scale_x_discrete(labels= c("1", "2", "3", "4",
                             "5", "6", "7", "8",
                             "9", "10", "11", "12")) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_line(size = 0.2, linetype = 2),
        legend.position = c(0.9, 0.9), 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.9, hjust=0.5, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222") #DAD7CB #E37222
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_ER_singleTC <- g

ggsave(filename = "data/results/figures/23_ER_singleTC.png", g, 
       width = 7, height = 6, dpi = 300, units = "in", device='png')



# # -------------------------------------------------------- 123 visualization #### -------------------------------------------------
# # ---------------------------- 123_SD #### -----------------------------------------------------
# Age
# Frequency
# Mileage
# Knowledge
# starts_with("FAS.")
# starts_with("Oft"),
# Weather
# Light

# 123_SD_Age  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data, aes(x=HMI, y=Age, fill=HMI)) + 
  scale_y_continuous(limits = c(15,75), breaks = seq(20,70,10)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot() + 
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
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
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Age <- g

ggsave(filename = "data/results/figures/123_SD_Age.png", g, 
       width = 9, height = 6, dpi = 300, units = "in", device='png')

# 123_SD_Frequency  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data, aes(x=HMI, y=Experience, fill=HMI)) + 
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, 1), 
                     labels = c("1 -\nnever",
                                "2 -\nless than\nonce a month", 
                                "3 -\nseveral times\na month", 
                                "4 -\nseveral times\na week", 
                                "5 -\nevery day")) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Frequency: \n	How often do you drive a car?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=2.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Frequency <- g

ggsave(filename = "data/results/figures/123_SD_Frequency.png", g, 
       width = 9.75, height = 5, dpi = 300, units = "in", device='png')

# 123_SD_Mileage  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data, aes(x=HMI, y=Mileage, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1),
                     labels = c("< 5.000 km",
                                "5.001 km -\n10.000 km",
                                "10.001 km -\n20.000 km",
                                ">20.000 km"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Mileage: \nHow many kilometers have you driven in the last 12 months?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1., hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Mileage <- g

ggsave(filename = "data/results/figures/123_SD_Mileage.png", g, 
       width = 9.75, height = 5, dpi = 300, units = "in", device='png')

# 123_SD_Knowledge  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data, aes(x=HMI, y=Knowledge, fill=HMI)) + 
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, 1),
                     labels = c("0 - no\nknowledge",
                                "1",
                                "2",
                                "3",
                                "4 -\nexpert"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Prior Knowledge: \nDo you have prior knowledge in the field of automated driving?") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Knowledge <- g

ggsave(filename = "data/results/figures/123_SD_Knowledge.png", g, 
       width = 9.75, height = 5, dpi = 300, units = "in", device='png')
# 123_SD_FAS -----------------------------------------------------------
## subset subscales ##
FAS_CC <- data %>%
  select(Exp, VPNr, HMI, FAS.CC) %>%
  dplyr::rename(score = FAS.CC) %>%
  add_column(scale = "FAS_CC", .after = "VPNr")
FAS_ACC <- data %>%
  select(Exp, VPNr, HMI, FAS.ACC) %>%
  dplyr::rename(score = FAS.ACC) %>%
  add_column(scale = "FAS_ACC", .after = "VPNr")
FAS_LKA <- data %>%
  select(Exp, VPNr, HMI, FAS.LKA) %>%
  dplyr::rename(score = FAS.LKA) %>%
  add_column(scale = "FAS_LKA", .after = "VPNr")
FAS_NoFAS <- data %>%
  select(Exp, VPNr, HMI, FAS.NoFAS) %>%
  dplyr::rename(score = FAS.NoFAS) %>%
  add_column(scale = "FAS_NoFAS", .after = "VPNr")

## build subsets ##
FAS <- bind_rows(FAS_CC, FAS_ACC, FAS_LKA, FAS_NoFAS) %>%
  mutate(scale = factor(scale, levels = c("FAS_CC", "FAS_ACC", "FAS_LKA", "FAS_NoFAS"), ordered = TRUE))

FAS_perc <- FAS %>%
  select(-c(VPNr)) %>%
  group_by(Exp, HMI, scale) %>%
  skim_ML() %>%
  select(Exp, HMI, scale, numeric.mean) %>%
  dplyr::rename(mean = numeric.mean)

## lables ##
labels_FAS = c("CC", "ACC", "LKA", "No FAS")

## n ##
p <- ggplot(FAS, aes(x=scale, y = score, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_FAS) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Familiarity with ADAS [n]", x="",
       title = "Which driver assistance systems do you already have experience with?") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_FAS <- g

ggsave(filename = "data/results/figures/123_SD_FAS.png", g, 
       width = 15, height = 6, dpi = 600, units = "in", device='png')


## Percentage ##
p <- ggplot(FAS_perc, aes(x=scale, y = mean*100, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_FAS) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, 20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Familiarity with ADAS [%]", x="",
       title = "Which driver assistance systems do you already have experience with?") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_FAS_perc <- g

ggsave(filename = "data/results/figures/123_SD_FAS_perc.png", g, 
       width = 15, height = 6, dpi = 600, units = "in", device='png')

# 123_SD_Oft_box -----------------------------------------------------------
## subset subscales ##
Oft_CC <- data %>%
  select(Exp, VPNr, HMI, OftCC) %>%
  dplyr::rename(score = OftCC) %>%
  add_column(scale = "Oft_CC", .after = "VPNr")
Oft_ACC <- data %>%
  select(Exp, VPNr, HMI, OftACC) %>%
  dplyr::rename(score = OftACC) %>%
  add_column(scale = "Oft_ACC", .after = "VPNr")
Oft_LKA <- data %>%
  select(Exp, VPNr, HMI, OftLKA) %>%
  dplyr::rename(score = OftLKA) %>%
  add_column(scale = "Oft_LKA", .after = "VPNr")

## build subset ##
Oft <- bind_rows(Oft_CC, Oft_ACC, Oft_LKA) %>%
  mutate(scale = factor(scale, levels = c("Oft_CC", "Oft_ACC", "Oft_LKA"), ordered = TRUE))

## lables ##
labels_Oft = c("CC", "ACC", "LKA")

p <- ggplot(Oft, aes(x=scale, y=score, fill=HMI)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_Oft) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "seldom", "every month", "every week", "every day", "several times\na day")) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="", x="",
       title = "How often do you use the system ...") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Oft_box <- g

ggsave(filename = "data/results/figures/123_SD_Oft_box.png", g, 
       width = 15, height = 6, dpi = 600, units = "in", device='png')
# 123_SD_Oft_dot -----------------------------------------------------------
## subset subscales ##
Oft_CC <- data %>%
  select(Exp, VPNr, HMI, OftCC) %>%
  dplyr::rename(score = OftCC) %>%
  add_column(scale = "Oft_CC", .after = "VPNr")
Oft_ACC <- data %>%
  select(Exp, VPNr, HMI, OftACC) %>%
  dplyr::rename(score = OftACC) %>%
  add_column(scale = "Oft_ACC", .after = "VPNr")
Oft_LKA <- data %>%
  select(Exp, VPNr, HMI, OftLKA) %>%
  dplyr::rename(score = OftLKA) %>%
  add_column(scale = "Oft_LKA", .after = "VPNr")

## build subset ##
Oft <- bind_rows(Oft_CC, Oft_ACC, Oft_LKA) %>%
  mutate(scale = factor(scale, levels = c("Oft_CC", "Oft_ACC", "Oft_LKA"), ordered = TRUE))

## lables ##
labels_Oft = c("CC", "ACC", "LKA")

p <- ggplot(Oft, aes(x=scale, y=score, fill=HMI)) + 
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.35, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  scale_x_discrete(labels = labels_Oft) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "seldom", "every month", "every week", "every day", "several times\na day")) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="", x="",
       title = "How often do you use the system ...") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=0, hjust=1.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Oft_dot <- g

ggsave(filename = "data/results/figures/123_SD_Oft_dot.png", g, 
       width = 15, height = 6, dpi = 600, units = "in", device='png')


# 123_SD_Weather  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data, aes(x=HMI, y=Weather, fill=HMI)) + 
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3, 1),
                     labels = c("sunny,\nblue sky",
                                "lightly\nclouded",
                                "heavily\nclouded",
                                "light\nrain"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.13, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Weather conditions at beginning of experiment") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Weather <- g

ggsave(filename = "data/results/figures/123_SD_Weather.png", g, 
       width = 9.75, height = 5, dpi = 300, units = "in", device='png')
# 123_SD_Light  ----------------------------------------------------- 
## basic plot ##
p <- ggplot(data, aes(x=HMI, y=Light, fill=HMI)) + 
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 1),
                     labels = c("very bright,\nblinding",
                                "bright",
                                "gloomy,\ndusky"
                     )) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.1, dotsize=0.08, binwidth = 0.5) +
  facet_wrap(~Exp) + 
  scale_fill_manual("HMI", values = c("HC" = "#3070b3", "LC" = "#98C6EA")) +
  labs(y="", x="",
       title = "Light conditions at beginning of experiment") +
  stat_summary(fun = mean, geom="point",colour="black", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.3, hjust=0.5) +
  stat_summary(fun = median, geom="point",colour="black", size = 4, shape = 4) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "plain"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.y=element_text(color = "black", size=9, face = "plain"),
        legend.position = "none")
p
## change color of facet box (code by CharlotteWoolley , 17 May 2018: https://github.com/tidyverse/ggplot2/issues/2096)
g <- ggplot_gtable(ggplot_build(p))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_123_SD_Light <- g

ggsave(filename = "data/results/figures/123_SD_Light.png", g, 
       width = 12.75, height = 5, dpi = 300, units = "in", device='png')
