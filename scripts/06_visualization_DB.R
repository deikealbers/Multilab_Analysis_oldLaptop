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
## data Deact --- deactivations TC06
data_all_Deact <- read.csv("data/drivingdata/data_all_Deact_corrected.csv", encoding = "UTF-8") %>%
  rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE))

data_12_Deact <- data_all_Deact %>%
  filter(Exp == "Sim_GER" | Exp == "TT_GER")
data_23_Deact <- data_all_Deact %>%
  filter(Exp == "TT_GER" | Exp == "TT_USA")

## data L2Hoff --- Hoff detections in L2, TC06-TC08
data_all_L2Hoff <- read.csv("data/drivingdata/data_all_L2Hoff_corrected.csv", encoding = "UTF-8") %>%
  rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE))

data_12_L2Hoff <- data_all_L2Hoff %>%
  filter(Exp == "Sim_GER" | Exp == "TT_GER")
data_23_L2Hoff <- data_all_L2Hoff %>%
  filter(Exp == "TT_GER" | Exp == "TT_USA")

## data TO --- Take-over scenarios TC10, TC12
data_all_TO <- read.csv("data/drivingdata/data_all_TO_corrected.csv", encoding = "UTF-8") %>%
  rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE)) %>%
  mutate(TC10_TimeToL0_max25 = ifelse(TC10_TimeToL0 > 25, 25, TC10_TimeToL0)) %>%
  mutate(TC12_TimeToL0_max25 = ifelse(TC12_TimeToL0 > 25, 25, TC12_TimeToL0)) %>%
  mutate(TC10_TimeToFirstAction_max25 = ifelse(TC10_TimeToFirstAction > 25, 25, TC10_TimeToFirstAction)) %>%
  mutate(TC12_TimeToFirstAction_max25 = ifelse(TC12_TimeToFirstAction > 25, 25, TC12_TimeToFirstAction)) 

data_12_TO <- data_all_TO %>%
  filter(Exp == "Sim_GER" | Exp == "TT_GER")
data_23_TO <- data_all_TO %>%
  filter(Exp == "TT_GER" | Exp == "TT_USA")


# # # notes on plots --------------------------------------------------------



# # -------------------------------------------------------- 12 visualization #### -------------------------------------------------
# # ---------------------------- 12_DB_Deact #### -----------------------------------------------------
# Deact

# 12_DB_Deact #### -----------------------------------------------------
p <- ggplot(data_12_Deact, aes(x=HMI, y=Deactivation, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1)) +
  facet_grid(~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of deactivations", x="",
       title = "Number of deactivations of L2 after notification\nof non-availability of L3 (due to sensor error)") +
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
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#DAD7CB", "#A2AD00")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_12_DB_Deact <- g

ggsave(filename = "data/results/figures/12_DB_Deact.png", g, 
       width = 5, height = 5, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_Deact", "data_12_Deact", "data_23_Deact",
                        "data_all_L2Hoff", "data_12_L2Hoff", "data_23_L2Hoff",
                        "data_all_TO", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_DB_Deact")))

# # ---------------------------- 12_DB_L2Hoff #### -----------------------------------------------------
# L2Hoff

# 12_DB_L2Hoff #### -----------------------------------------------------
## subset subscales ##
L2Hoff_Hoff_n_warnings <- data_12_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings) %>%
  dplyr::rename(score = Hoff_n_warnings) %>%
  add_column(scale = "overall", .after = "VPNr")
L2Hoff_Hoff_n_warnings_s1 <- data_12_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings_s1) %>%
  dplyr::rename(score = Hoff_n_warnings_s1) %>%
  add_column(scale = "stage_1", .after = "VPNr")
L2Hoff_Hoff_n_warnings_s2 <- data_12_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings_s2) %>%
  dplyr::rename(score = Hoff_n_warnings_s2) %>%
  add_column(scale = "stage_2", .after = "VPNr")
L2Hoff_Hoff_n_warnings_s3 <- data_12_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings_s3) %>%
  dplyr::rename(score = Hoff_n_warnings_s3) %>%
  add_column(scale = "stage_3", .after = "VPNr")

## build subset ##
L2Hoff <- bind_rows(L2Hoff_Hoff_n_warnings, L2Hoff_Hoff_n_warnings_s1,
                    L2Hoff_Hoff_n_warnings_s2, L2Hoff_Hoff_n_warnings_s3) %>%
  mutate(scale = factor(scale, levels = c("overall", "stage_1", "stage_2", "stage_3"), ordered = TRUE))

## lables ##
labels_L2Hoff = c("overall", "stage 1", "stage 2", "stage 3")

p <- ggplot(L2Hoff, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 10),
             fill = scales::alpha("#696969", 0.005)) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.25, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=-.1) +
  scale_x_discrete(labels = labels_L2Hoff) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of warnings", x="",
       title = "Number of hands-off detection warnings during L2 driving") +
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

plot_12_DB_L2Hoff <- g

ggsave(filename = "data/results/figures/12_DB_L2Hoff.png", g, 
       width = 11, height = 6, dpi = 600, units = "in", device='png')

# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_Deact", "data_12_Deact", "data_23_Deact",
                        "data_all_L2Hoff", "data_12_L2Hoff", "data_23_L2Hoff",
                        "data_all_TO", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_DB_Deact",
                        "plot_12_DB_L2Hoff")))

# # ---------------------------- 12_DB_TO #### -----------------------------------------------------
# TimeToL0
# TimeToFirstAction
# FirstAction
# 12_DB_TO_TimeToL0 #### -----------------------------------------------------
## subset subscales ##
TO_TC10_TimeToL0 <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_TimeToL0_max25) %>%
  dplyr::rename(score = TC10_TimeToL0_max25) %>%
  add_column(scale = "TC10", .after = "VPNr")

TO_TC12_TimeToL0 <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_TimeToL0_max25) %>%
  dplyr::rename(score = TC12_TimeToL0_max25) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_TimeToL0 <- bind_rows(TO_TC10_TimeToL0, TO_TC12_TimeToL0) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12"), ordered = TRUE))

## lables ##
labels_TO_TimeToL0 = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_TimeToL0, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf),
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.97, ymax = 20.03),
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03),
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_TimeToL0) +
  scale_y_continuous(limits = c(0,27), breaks = seq(0,27,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Time [s]", x="",
       title = "Take-over time after start of request to intervene") +
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

plot_12_DB_TO_TimeToL0 <- g

ggsave(filename = "data/results/figures/12_DB_TO_TimeToL0.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_DB_TO_TimeToFirstAction #### -----------------------------------------------------
## subset subscales ##
TO_TC10_TimeToFirstAction <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_TimeToFirstAction_max25) %>%
  dplyr::rename(score = TC10_TimeToFirstAction_max25) %>%
  add_column(scale = "TC10", .after = "VPNr")

TO_TC12_TimeToFirstAction <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_TimeToFirstAction_max25) %>%
  dplyr::rename(score = TC12_TimeToFirstAction_max25) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_TimeToFirstAction <- bind_rows(TO_TC10_TimeToFirstAction, TO_TC12_TimeToFirstAction) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12"), ordered = TRUE))

## lables ##
labels_TO_TimeToFirstAction = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_TimeToFirstAction, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf),
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.97, ymax = 20.03),
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03),
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_TimeToFirstAction) +
  scale_y_continuous(limits = c(0,27), breaks = seq(0,27,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Time [s]", x="",
       title = "Time until first action after start of request to intervene") +
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

plot_12_DB_TO_TimeToFirstAction <- g

ggsave(filename = "data/results/figures/12_DB_TO_TimeToFirstAction.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')



# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_Deact", "data_12_Deact", "data_23_Deact",
                        "data_all_L2Hoff", "data_12_L2Hoff", "data_23_L2Hoff",
                        "data_all_TO", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_DB_Deact",
                        "plot_12_DB_L2Hoff",
                        "plot_12_DB_TO_TimeToL0", "plot_12_DB_TO_TimeToFirstAction")))

# # -------------------------------------------------------- 23 visualization #### -------------------------------------------------
# # ---------------------------- 23_DB_Deact #### -----------------------------------------------------
# Deact

# 23_DB_Deact #### -----------------------------------------------------
p <- ggplot(data_23_Deact, aes(x=HMI, y=Deactivation, fill=HMI)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1)) +
  facet_grid(~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of deactivations", x="",
       title = "Number of deactivations of L2 after notification\nof non-availability of L3 (due to sensor error)") +
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
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#A2AD00", "#E37222")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_DB_Deact <- g

ggsave(filename = "data/results/figures/23_DB_Deact.png", g, 
       width = 5, height = 5, dpi = 600, units = "in", device='png')

# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_Deact", "data_12_Deact", "data_23_Deact",
                        "data_all_L2Hoff", "data_12_L2Hoff", "data_23_L2Hoff",
                        "data_all_TO", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_DB_Deact",
                        "plot_12_DB_L2Hoff",
                        "plot_12_DB_TO_TimeToL0", "plot_12_DB_TO_TimeToFirstAction",
                        "plot_23_DB_Deact")))





# # ---------------------------- 23_DB_L2Hoff #### -----------------------------------------------------
# L2Hoff

# 23_DB_L2Hoff #### -----------------------------------------------------
## subset subscales ##
L2Hoff_Hoff_n_warnings <- data_23_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings) %>%
  dplyr::rename(score = Hoff_n_warnings) %>%
  add_column(scale = "overall", .after = "VPNr")
L2Hoff_Hoff_n_warnings_s1 <- data_23_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings_s1) %>%
  dplyr::rename(score = Hoff_n_warnings_s1) %>%
  add_column(scale = "stage_1", .after = "VPNr")
L2Hoff_Hoff_n_warnings_s2 <- data_23_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings_s2) %>%
  dplyr::rename(score = Hoff_n_warnings_s2) %>%
  add_column(scale = "stage_2", .after = "VPNr")
L2Hoff_Hoff_n_warnings_s3 <- data_23_L2Hoff %>%
  select(Exp, VPNr, HMI, Hoff_n_warnings_s3) %>%
  dplyr::rename(score = Hoff_n_warnings_s3) %>%
  add_column(scale = "stage_3", .after = "VPNr")

## build subset ##
L2Hoff <- bind_rows(L2Hoff_Hoff_n_warnings, L2Hoff_Hoff_n_warnings_s1,
                    L2Hoff_Hoff_n_warnings_s2, L2Hoff_Hoff_n_warnings_s3) %>%
  mutate(scale = factor(scale, levels = c("overall", "stage_1", "stage_2", "stage_3"), ordered = TRUE))

## lables ##
labels_L2Hoff = c("overall", "stage 1", "stage 2", "stage 3")

p <- ggplot(L2Hoff, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 10),
            fill = scales::alpha("#696969", 0.005)) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.25, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=-.1) +
  scale_x_discrete(labels = labels_L2Hoff) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of warnings", x="",
       title = "Number of hands-off detection warnings during L2 driving") +
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
fills <- c("#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_23_DB_L2Hoff <- g

ggsave(filename = "data/results/figures/23_DB_L2Hoff.png", g, 
       width = 11, height = 6, dpi = 600, units = "in", device='png')

# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_Deact", "data_12_Deact", "data_23_Deact",
                        "data_all_L2Hoff", "data_12_L2Hoff", "data_23_L2Hoff",
                        "data_all_TO", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_DB_Deact",
                        "plot_12_DB_L2Hoff",
                        "plot_12_DB_TO_TimeToL0", "plot_12_DB_TO_TimeToFirstAction",
                        "plot_23_DB_Deact",
                        "plot_23_DB_L2Hoff")))



# # ---------------------------- 23_DB_TO #### -----------------------------------------------------
# TimeToL0
# TimeToFirstAction
# FirstAction
# 23_DB_TO_TimeToL0 #### -----------------------------------------------------
## subset subscales ##
TO_TC10_TimeToL0 <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_TimeToL0_max25) %>%
  dplyr::rename(score = TC10_TimeToL0_max25) %>%
  add_column(scale = "TC10", .after = "VPNr")

TO_TC12_TimeToL0 <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_TimeToL0_max25) %>%
  dplyr::rename(score = TC12_TimeToL0_max25) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_TimeToL0 <- bind_rows(TO_TC10_TimeToL0, TO_TC12_TimeToL0) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12"), ordered = TRUE))

## lables ##
labels_TO_TimeToL0 = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_TimeToL0, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf),
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.97, ymax = 20.03),
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03),
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_TimeToL0) +
  scale_y_continuous(limits = c(0,27), breaks = seq(0,27,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Time [s]", x="",
       title = "Take-over time after start of request to intervene") +
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
fills <- c("#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_data_23_DB_TO_TimeToL0 <- g

ggsave(filename = "data/results/figures/23_DB_TO_TimeToL0.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_DB_TO_TimeToFirstAction #### -----------------------------------------------------
## subset subscales ##
TO_TC10_TimeToFirstAction <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_TimeToFirstAction_max25) %>%
  dplyr::rename(score = TC10_TimeToFirstAction_max25) %>%
  add_column(scale = "TC10", .after = "VPNr")

TO_TC12_TimeToFirstAction <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_TimeToFirstAction_max25) %>%
  dplyr::rename(score = TC12_TimeToFirstAction_max25) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_TimeToFirstAction <- bind_rows(TO_TC10_TimeToFirstAction, TO_TC12_TimeToFirstAction) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12"), ordered = TRUE))

## lables ##
labels_TO_TimeToFirstAction = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_TimeToFirstAction, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 27),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf),
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.97, ymax = 20.03),
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03),
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_TimeToFirstAction) +
  scale_y_continuous(limits = c(0,27), breaks = seq(0,27,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Time [s]", x="",
       title = "Time until first action after start of request to intervene") +
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
fills <- c("#A2AD00", "#E37222", "#3070b3", "#98C6EA")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

plot_data_23_DB_TO_TimeToFirstAction <- g

ggsave(filename = "data/results/figures/23_DB_TO_TimeToFirstAction.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_Deact", "data_12_Deact", "data_23_Deact",
                        "data_all_L2Hoff", "data_12_L2Hoff", "data_23_L2Hoff",
                        "data_all_TO", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_DB_Deact",
                        "plot_12_DB_L2Hoff",
                        "plot_12_DB_TO_TimeToL0", "plot_12_DB_TO_TimeToFirstAction",
                        "plot_23_DB_Deact",
                        "plot_23_DB_L2Hoff",
                        "plot_23_DB_TO_TimeToL0", "plot_23_DB_TO_TimeToFirstAction")))

# ############## notes  ############ --------------------------------------
      # es fehlt: Darstellung der Take-over actions
      # Idee: Häufigkeit jeder Aktion (1-4) darstellen; 
      # evtl. sogar nach Warnstufe aufgeschlüsselt. 
      # Mindestens die erste Aktion kann man so darstellen
      # Hier sollte man vermutlich TC10 und TC12 einzeln darstellen.
      # für Stufe Notbremsung sollten aber nur Reaktionen <25s eingeschlossen werden, 
      # vll. sogar noch weniger. Sonst wird es ja verzerrt

      # max25 muss auch beachtet werden für die inferenzstatistischen Tests; 
      # die Modifizierung muss also evtl. schon im anderen Skript vorgenommen werden