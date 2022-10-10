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
## data Take-over scenarios TC10, TC12
data_all_TO <- read.csv("data/eyetracking/data_all_TO.csv", encoding = "UTF-8") %>%
  rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE))

data_12_TO <- data_all_TO %>%
  filter(Exp == "Sim_GER" | Exp == "TT_GER")
data_23_TO <- data_all_TO %>%
  filter(Exp == "TT_GER" | Exp == "TT_USA")

## data Attention Ratios TC01, TC04, TC07
data_all_AR <- read.csv("data/eyetracking/data_all_AR.csv", encoding = "UTF-8") %>%
  rename(Exp = X.U.FEFF.Exp) %>%
  mutate(Exp = ifelse(Exp == 1, "Sim_GER", ifelse(Exp == 2, "TT_GER", ifelse(Exp == 3, "TT_USA", Exp)))) %>%
  mutate(Exp = factor(Exp, levels = c("Sim_GER", "TT_GER", "TT_USA"), ordered = TRUE))

data_12_AR <- data_all_AR %>%
  filter(Exp == "Sim_GER" | Exp == "TT_GER")
data_23_AR <- data_all_AR %>%
  filter(Exp == "TT_GER" | Exp == "TT_USA")

# # # notes on plots --------------------------------------------------------
# AR
#   - left to right: HMIs top to bottom: experiments;
#   - within boxes: attention ratios L0, L2, L3
# TO
#   - left to right: HMIs TC10, TC12; top to bottom: experiments;
#   - within boxes: metrics for all AOIs
#       - n_glances_after_start
#       - total_duration
#       - mean_duration
#       - max_duration
#       - 1st_glance_duration_without_start
# 
#   - ic: 


# # -------------------------------------------------------- 12 visualization #### -------------------------------------------------
# # ---------------------------- 12_ET_AR #### -----------------------------------------------------
# AR_all
# AR_ic

# 12_ET_AR_all  ----------------------------------------------------- 
## subset subscales ##
AR_L0_ic <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_ic) %>%
  dplyr::rename(score = TC01_AR_ic) %>%
  add_column(scale = "L0_ic", .after = "VPNr")
AR_L0_street <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_street) %>%
  dplyr::rename(score = TC01_AR_street) %>%
  add_column(scale = "L0_street", .after = "VPNr")
AR_L0_surt <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_surt) %>%
  dplyr::rename(score = TC01_AR_surt) %>%
  add_column(scale = "L0_surt", .after = "VPNr")
AR_L0_wheel <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_wheel) %>%
  dplyr::rename(score = TC01_AR_wheel) %>%
  add_column(scale = "L0_wheel", .after = "VPNr")

AR_L2_ic <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_ic) %>%
  dplyr::rename(score = TC07_AR_ic) %>%
  add_column(scale = "L2_ic", .after = "VPNr")
AR_L2_street <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_street) %>%
  dplyr::rename(score = TC07_AR_street) %>%
  add_column(scale = "L2_street", .after = "VPNr")
AR_L2_surt <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_surt) %>%
  dplyr::rename(score = TC07_AR_surt) %>%
  add_column(scale = "L2_surt", .after = "VPNr")
AR_L2_wheel <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_wheel) %>%
  dplyr::rename(score = TC07_AR_wheel) %>%
  add_column(scale = "L2_wheel", .after = "VPNr")

AR_L3_ic <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_ic) %>%
  dplyr::rename(score = TC04_AR_ic) %>%
  add_column(scale = "L3_ic", .after = "VPNr")
AR_L3_street <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_street) %>%
  dplyr::rename(score = TC04_AR_street) %>%
  add_column(scale = "L3_street", .after = "VPNr")
AR_L3_surt <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_surt) %>%
  dplyr::rename(score = TC04_AR_surt) %>%
  add_column(scale = "L3_surt", .after = "VPNr")
AR_L3_wheel <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_wheel) %>%
  dplyr::rename(score = TC04_AR_wheel) %>%
  add_column(scale = "L3_wheel", .after = "VPNr")


## build subset ##
AR_all <- bind_rows(AR_L0_ic, AR_L0_street, AR_L0_surt, AR_L0_wheel, 
                AR_L2_ic, AR_L2_street, AR_L2_surt, AR_L2_wheel,
                AR_L3_ic, AR_L3_street, AR_L3_surt, AR_L3_wheel) %>%
  mutate(scale = factor(scale, levels = c("L0_ic", "L0_street", "L0_surt", "L0_wheel",
                                          "L2_ic", "L2_street", "L2_surt", "L2_wheel",
                                          "L3_ic", "L3_street", "L3_surt", "L3_wheel"), ordered = TRUE))

## lables ##
# labels_AR_all = c("L0_ic", "L0_street", "L0_surt", "L0_wheel",
#               "L2_ic", "L2_street", "L2_surt", "L2_wheel",
#               "L3_ic", "L3_street", "L3_surt", "L3_wheel")

labels_AR_all = c("ic", "street", "surt", "wheel", 
                  "ic", "street", "surt", "wheel",
                  "ic", "street", "surt", "wheel")

p <- ggplot(AR_all, aes(x=scale, y=score, fill=HMI)) + 
  # geom_rect(aes(xmin = 0.55, xmax = 4.45, ymin = -Inf, ymax = Inf), 
  #           fill = scales::alpha("#2F2F2F", 0.001)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FF03", 0.002)) +
  geom_rect(aes(xmin = 8.5, xmax = 12.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FFFF", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 8.495, xmax = 8.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 100, label = "L0", size = 3, vjust = 1) +
  annotate("text", x = 6.5, y = 100, label = "L2", size = 3, vjust = 1) +
  annotate("text", x = 10.5, y = 100, label = "L3", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_AR_all) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Eyes on AOI [%]", x="",
       title = "Attention Ratios while driving in L0, L2 and L3") +
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

plot_12_ET_AR_all <- g

ggsave(filename = "data/results/figures/12_ET_AR_all.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_AR_surt ----------------------------------------------------- 
## subset subscales ##
AR_L0_surt <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_surt) %>%
  dplyr::rename(score = TC01_AR_surt) %>%
  add_column(scale = "L0_surt", .after = "VPNr")
AR_L2_surt <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_surt) %>%
  dplyr::rename(score = TC07_AR_surt) %>%
  add_column(scale = "L2_surt", .after = "VPNr")
AR_L3_surt <- data_12_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_surt) %>%
  dplyr::rename(score = TC04_AR_surt) %>%
  add_column(scale = "L3_surt", .after = "VPNr")

## build subset ##
AR_surt <- bind_rows(AR_L0_surt, AR_L2_surt, AR_L3_surt) %>%
  mutate(scale = factor(scale, levels = c("L0_surt", "L2_surt", "L3_surt"), ordered = TRUE))

## lables ##
labels_AR_surt = c("L0", "L2", "L3")

p <- ggplot(AR_surt, aes(x=scale, y=score, fill=HMI)) + 
  # geom_rect(aes(xmin = 0.55, xmax = 4.45, ymin = -Inf, ymax = Inf), 
  #           fill = scales::alpha("#2F2F2F", 0.001)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FF03", 0.002)) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FFFF", 0.002)) +
  geom_rect(aes(xmin = 1.495, xmax = 1.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 2.495, xmax = 2.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_AR_surt) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Eyes on AOI [%]", x="",
       title = "Attention Ratio SuRT while driving in L0, L2 and L3") +
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

plot_12_ET_AR_surt <- g

ggsave(filename = "data/results/figures/12_ET_AR_surt.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt")))

# # ---------------------------- 12_ET_TO #### -----------------------------------------------------
# overall
  # n_gazes_till_EB_or_TO
  # glance at start - distribution
# all AOIs 
  # n_glances_after_start
  # total_duration
  # mean_duration
  # max_duration
  # 1st_glance_duration_without_start
# ic
  # nth_gaze_to_ic
  # glance_allocation_time
  # n_glances_after_start
  # total_duration
  # mean_duration
  # max_duration
  # 1st_glance_duration_without_start

# 12_ET_TO_n_gazes_till_EB_or_TO #### -----------------------------------------------------
## subset subscales ##
TO_TC10_n_gazes_till_EB_or_TO <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_n_gazes_till_EB_or_TO) %>%
  dplyr::rename(score = TC10_n_gazes_till_EB_or_TO) %>%
  add_column(scale = "TC10", .after = "VPNr")

TO_TC12_n_gazes_till_EB_or_TO <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_n_gazes_till_EB_or_TO) %>%
  dplyr::rename(score = TC12_n_gazes_till_EB_or_TO) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_all_n_gazes_till_EB_or_TO <- bind_rows(TO_TC10_n_gazes_till_EB_or_TO, TO_TC12_n_gazes_till_EB_or_TO) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12"), ordered = TRUE))

## lables ##
labels_TO_all_n_gazes_till_EB_or_TO = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_all_n_gazes_till_EB_or_TO, aes(x=scale, y=score, fill=HMI)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_all_n_gazes_till_EB_or_TO) +
  scale_y_continuous(limits = c(0,13), breaks = seq(0,12,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances to AOIs until\ntake-over or start of emergency brake") +
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

plot_12_ET_TO_all_n_gazes_till_EB_or_TO <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_n_gazes_till_EB_or_TO.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_all_glance_at_start -----------------------------------------------------------
## subset subscales ##
TO_TC10_ic_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_glance_at_start) %>%
  dplyr::rename(score = TC10_ic_glance_at_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_glance_at_start) %>%
  dplyr::rename(score = TC10_street_glance_at_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_glance_at_start) %>%
  dplyr::rename(score = TC10_surt_glance_at_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_glance_at_start) %>%
  dplyr::rename(score = TC10_wheel_glance_at_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_glance_at_start) %>%
  dplyr::rename(score = TC12_ic_glance_at_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_glance_at_start) %>%
  dplyr::rename(score = TC12_street_glance_at_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_glance_at_start) %>%
  dplyr::rename(score = TC12_surt_glance_at_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_glance_at_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_glance_at_start) %>%
  dplyr::rename(score = TC12_wheel_glance_at_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")

## build subset ##
TO_all_glance_at_start <- bind_rows(TO_TC10_ic_glance_at_start, TO_TC10_street_glance_at_start,
                                    TO_TC10_surt_glance_at_start, TO_TC10_wheel_glance_at_start,
                                    TO_TC12_ic_glance_at_start, TO_TC12_street_glance_at_start,
                                    TO_TC12_surt_glance_at_start, TO_TC12_wheel_glance_at_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

TO_all_glance_at_start_perc <- TO_all_glance_at_start %>%
  select(-c(VPNr)) %>%
  group_by(Exp, HMI, scale) %>%
  skim_ML() %>%
  select(Exp, HMI, scale, numeric.mean) %>%
  dplyr::rename(mean = numeric.mean)

## lables ##
labels_TO_all_glance_at_start = c("ic", "street", "surt", "wheel", 
                                  "ic", "street", "surt", "wheel")

## n ##
p <- ggplot(TO_all_glance_at_start, aes(x=scale, y = score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 30),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 30),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 30, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 30, label = "RtI without time budget", size = 3, vjust = 1) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_TO_all_glance_at_start) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Frequency", x="",
       title = "Frequency of glances to AOI at start of request to intervene") +
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

plot_12_ET_TO_all_glance_at_start <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_glance_at_start.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


## Percentage ##
p <- ggplot(TO_all_glance_at_start_perc, aes(x=scale, y = mean*100, fill=HMI)) + 
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 105),
            fill = scales::alpha("#F8FAA0", 0.05)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 105, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 105, label = "RtI without time budget", size = 3, vjust = 1) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_TO_all_glance_at_start) +
  scale_y_continuous(limits = c(0,105), breaks = seq(0,100, 20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Proportion [%]", x="",
       title = "Proportion of glances to AOI at start of request to intervene") +
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

plot_12_ET_TO_all_glance_at_start_perc <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_glance_at_start_perc.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc")))

# 12_ET_TO_all_n_glances_after_start_dot #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_n_glances_after_start) %>%
  dplyr::rename(score = TC10_street_n_glances_after_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC10_surt_n_glances_after_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC10_wheel_n_glances_after_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_n_glances_after_start) %>%
  dplyr::rename(score = TC12_street_n_glances_after_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC12_surt_n_glances_after_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC12_wheel_n_glances_after_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC10_street_n_glances_after_start,
                                          TO_TC10_surt_n_glances_after_start, TO_TC10_wheel_n_glances_after_start,
                                          TO_TC12_ic_n_glances_after_start, TO_TC12_street_n_glances_after_start,
                                          TO_TC12_surt_n_glances_after_start, TO_TC12_wheel_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_n_glances_after_start = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_n_glances_after_start = c("ic", "street", "surt", "wheel", 
                                        "ic", "street", "surt", "wheel")

p <- ggplot(TO_all_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 8, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 8, label = "RtI without time budget", size = 3, vjust = 1) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.15, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_n_glances_after_start) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances after start of request to intervene") +
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

plot_12_ET_TO_all_n_glances_after_start_dot <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_n_glances_after_start_dot.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_all_n_glances_after_start_box #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_n_glances_after_start) %>%
  dplyr::rename(score = TC10_street_n_glances_after_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC10_surt_n_glances_after_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC10_wheel_n_glances_after_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_n_glances_after_start) %>%
  dplyr::rename(score = TC12_street_n_glances_after_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC12_surt_n_glances_after_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC12_wheel_n_glances_after_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC10_street_n_glances_after_start,
                                              TO_TC10_surt_n_glances_after_start, TO_TC10_wheel_n_glances_after_start,
                                              TO_TC12_ic_n_glances_after_start, TO_TC12_street_n_glances_after_start,
                                              TO_TC12_surt_n_glances_after_start, TO_TC12_wheel_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_n_glances_after_start = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_n_glances_after_start = c("ic", "street", "surt", "wheel", 
                                            "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 8, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 8, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_n_glances_after_start) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances after start of request to intervene") +
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

plot_12_ET_TO_all_n_glances_after_start_box <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_n_glances_after_start_box.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_all_total_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_total_duration) %>%
  dplyr::rename(score = TC10_ic_total_duration) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_total_duration) %>%
  dplyr::rename(score = TC10_street_total_duration) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_total_duration) %>%
  dplyr::rename(score = TC10_surt_total_duration) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_total_duration) %>%
  dplyr::rename(score = TC10_wheel_total_duration) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_total_duration) %>%
  dplyr::rename(score = TC12_ic_total_duration) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_total_duration) %>%
  dplyr::rename(score = TC12_street_total_duration) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_total_duration) %>%
  dplyr::rename(score = TC12_surt_total_duration) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_total_duration) %>%
  dplyr::rename(score = TC12_wheel_total_duration) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_total_duration <- bind_rows(TO_TC10_ic_total_duration, TO_TC10_street_total_duration,
                                       TO_TC10_surt_total_duration, TO_TC10_wheel_total_duration,
                                       TO_TC12_ic_total_duration, TO_TC12_street_total_duration,
                                       TO_TC12_surt_total_duration, TO_TC12_wheel_total_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_total_duration = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_total_duration = c("ic", "street", "surt", "wheel", 
                                     "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_total_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_total_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Total duration: Eyes on AOI after start of request to intervene") +
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

plot_12_ET_TO_all_total_duration <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_total_duration.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# 12_ET_TO_all_mean_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_mean_duration) %>%
  dplyr::rename(score = TC10_ic_mean_duration) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_mean_duration) %>%
  dplyr::rename(score = TC10_street_mean_duration) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_mean_duration) %>%
  dplyr::rename(score = TC10_surt_mean_duration) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_mean_duration) %>%
  dplyr::rename(score = TC10_wheel_mean_duration) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_mean_duration) %>%
  dplyr::rename(score = TC12_ic_mean_duration) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_mean_duration) %>%
  dplyr::rename(score = TC12_street_mean_duration) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_mean_duration) %>%
  dplyr::rename(score = TC12_surt_mean_duration) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_mean_duration) %>%
  dplyr::rename(score = TC12_wheel_mean_duration) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_mean_duration <- bind_rows(TO_TC10_ic_mean_duration, TO_TC10_street_mean_duration,
                                      TO_TC10_surt_mean_duration, TO_TC10_wheel_mean_duration,
                                      TO_TC12_ic_mean_duration, TO_TC12_street_mean_duration,
                                      TO_TC12_surt_mean_duration, TO_TC12_wheel_mean_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_mean_duration = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_mean_duration = c("ic", "street", "surt", "wheel", 
                                    "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_mean_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_mean_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Mean duration: Eyes on AOI after start of request to intervene") +
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

plot_12_ET_TO_all_mean_duration <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_mean_duration.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# 12_ET_TO_all_max_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_max_duration) %>%
  dplyr::rename(score = TC10_ic_max_duration) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_max_duration) %>%
  dplyr::rename(score = TC10_street_max_duration) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_max_duration) %>%
  dplyr::rename(score = TC10_surt_max_duration) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_max_duration) %>%
  dplyr::rename(score = TC10_wheel_max_duration) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_max_duration) %>%
  dplyr::rename(score = TC12_ic_max_duration) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_max_duration) %>%
  dplyr::rename(score = TC12_street_max_duration) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_max_duration) %>%
  dplyr::rename(score = TC12_surt_max_duration) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_max_duration) %>%
  dplyr::rename(score = TC12_wheel_max_duration) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_max_duration <- bind_rows(TO_TC10_ic_max_duration, TO_TC10_street_max_duration,
                                     TO_TC10_surt_max_duration, TO_TC10_wheel_max_duration,
                                     TO_TC12_ic_max_duration, TO_TC12_street_max_duration,
                                     TO_TC12_surt_max_duration, TO_TC12_wheel_max_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_max_duration = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_max_duration = c("ic", "street", "surt", "wheel", 
                                   "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_max_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_max_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Max duration: Eyes on AOI after start of request to intervene") +
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

plot_12_ET_TO_all_max_duration <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_max_duration.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# 12_ET_TO_all_1st_glance_duration_without_start #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_street_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_street_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_surt_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_wheel_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_street_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_street_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_surt_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_wheel_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_1st_glance_duration_without_start <- bind_rows(TO_TC10_ic_1st_glance_duration_without_start, TO_TC10_street_1st_glance_duration_without_start,
                                                          TO_TC10_surt_1st_glance_duration_without_start, TO_TC10_wheel_1st_glance_duration_without_start,
                                                          TO_TC12_ic_1st_glance_duration_without_start, TO_TC12_street_1st_glance_duration_without_start,
                                                          TO_TC12_surt_1st_glance_duration_without_start, TO_TC12_wheel_1st_glance_duration_without_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_1st_glance_duration_without_start = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_1st_glance_duration_without_start = c("ic", "street", "surt", "wheel", 
                                                        "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_1st_glance_duration_without_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_1st_glance_duration_without_start) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Duration of first glance to AOI after start of request to intervene") +
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

plot_12_ET_TO_all_1st_glance_duration_without_start <- g

ggsave(filename = "data/results/figures/12_ET_TO_all_1st_glance_duration_without_start.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc",
                        "plot_12_ET_TO_all_n_glances_after_start", "plot_12_ET_TO_all_total_duration",
                        "plot_12_ET_TO_all_mean_duration", "plot_12_ET_TO_all_max_duration", 
                        "plot_12_ET_TO_all_1st_glance_duration_without_start")))

# 12_ET_TO_ic_nth_gaze_to_ic_dot #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_nth_gaze_to_ic <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC10_nth_gaze_to_ic) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_nth_gaze_to_ic <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC12_nth_gaze_to_ic) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_nth_gaze_to_ic <- bind_rows(TO_TC10_ic_nth_gaze_to_ic, TO_TC12_ic_nth_gaze_to_ic) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_nth_gaze_to_ic = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_nth_gaze_to_ic, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  # stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  # geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.15, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_nth_gaze_to_ic) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number", x="",
       title = "First glance to ic after [...] glances to other AOIs\nafter start of request to intervene") +
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

plot_12_ET_TO_ic_nth_gaze_to_ic_dot <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_nth_gaze_to_ic_dot.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_ic_nth_gaze_to_ic_box #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_nth_gaze_to_ic <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC10_nth_gaze_to_ic) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_nth_gaze_to_ic <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC12_nth_gaze_to_ic) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_nth_gaze_to_ic <- bind_rows(TO_TC10_ic_nth_gaze_to_ic, TO_TC12_ic_nth_gaze_to_ic) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_nth_gaze_to_ic = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_nth_gaze_to_ic, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_nth_gaze_to_ic) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number", x="",
       title = "First glance to ic after [...] glances to other AOIs\nafter start of request to intervene") +
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

plot_12_ET_TO_ic_nth_gaze_to_ic_box <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_nth_gaze_to_ic_box.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_ic_glance_allocation_time #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_glance_allocation_time <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_glance_allocation_time) %>%
  dplyr::rename(score = TC10_ic_glance_allocation_time) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_glance_allocation_time <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_glance_allocation_time) %>%
  dplyr::rename(score = TC12_ic_glance_allocation_time) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_glance_allocation_time <- bind_rows(TO_TC10_ic_glance_allocation_time, TO_TC12_ic_glance_allocation_time) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_glance_allocation_time = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_glance_allocation_time, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_ic_glance_allocation_time) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Time [s]", x="",
       title = "Glance allocation time to ic after start of request to intervene") +
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

plot_12_ET_TO_ic_glance_allocation_time <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_glance_allocation_time.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_ic_n_glances_after_start_dot #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC12_ic_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_n_glances_after_start = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 7),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 7),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.2, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_n_glances_after_start) +
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances to ic after start of request to intervene") +
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

plot_12_ET_TO_ic_n_glances_after_start_dot <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_n_glances_after_start_dot.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# 12_ET_TO_ic_n_glances_after_start_box #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_n_glances_after_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC12_ic_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_n_glances_after_start = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_n_glances_after_start) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances to ic after start of request to intervene") +
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

plot_12_ET_TO_ic_n_glances_after_start_box <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_n_glances_after_start_box.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_ic_total_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_total_duration) %>%
  dplyr::rename(score = TC10_ic_total_duration) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_total_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_total_duration) %>%
  dplyr::rename(score = TC12_ic_total_duration) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_total_duration <- bind_rows(TO_TC10_ic_total_duration, TO_TC12_ic_total_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_total_duration = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_total_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_total_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Total duration: Eyes on ic after start of request to intervene") +
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

plot_12_ET_TO_ic_total_duration <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_total_duration.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_ic_mean_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_mean_duration) %>%
  dplyr::rename(score = TC10_ic_mean_duration) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_mean_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_mean_duration) %>%
  dplyr::rename(score = TC12_ic_mean_duration) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_mean_duration <- bind_rows(TO_TC10_ic_mean_duration, TO_TC12_ic_mean_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_mean_duration = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_mean_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_mean_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Mean duration: Eyes on ic after start of request to intervene") +
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

plot_12_ET_TO_ic_mean_duration <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_mean_duration.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# 12_ET_TO_ic_max_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_max_duration) %>%
  dplyr::rename(score = TC10_ic_max_duration) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_max_duration <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_max_duration) %>%
  dplyr::rename(score = TC12_ic_max_duration) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_max_duration <- bind_rows(TO_TC10_ic_max_duration, TO_TC12_ic_max_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_max_duration = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_max_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_max_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Max duration: Eyes on ic after start of request to intervene") +
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

plot_12_ET_TO_ic_max_duration <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_max_duration.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 12_ET_TO_ic_1st_glance_duration_without_start #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_1st_glance_duration_without_start <- data_12_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_1st_glance_duration_without_start <- bind_rows(TO_TC10_ic_1st_glance_duration_without_start, TO_TC12_ic_1st_glance_duration_without_start) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_1st_glance_duration_without_start = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_1st_glance_duration_without_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_1st_glance_duration_without_start) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Duration of first glance to ic after start of request to intervene") +
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

plot_12_ET_TO_ic_1st_glance_duration_without_start <- g

ggsave(filename = "data/results/figures/12_ET_TO_ic_1st_glance_duration_without_start.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc",
                        "plot_12_ET_TO_all_n_glances_after_start", "plot_12_ET_TO_all_total_duration",
                        "plot_12_ET_TO_all_mean_duration", "plot_12_ET_TO_all_max_duration", 
                        "plot_12_ET_TO_all_1st_glance_duration_without_start",
                        "plot_12_ET_TO_ic_nth_gaze_to_ic_dot", "plot_12_ET_TO_ic_nth_gaze_to_ic_box",
                        "plot_12_ET_TO_ic_glance_allocation_time", "plot_12_ET_TO_ic_n_glances_after_start_dot", "plot_12_ET_TO_ic_n_glances_after_start_box",
                        "plot_12_ET_TO_ic_total_duration", "plot_12_ET_TO_ic_mean_duration", "plot_12_ET_TO_ic_max_duration", 
                        "plot_12_ET_TO_ic_1st_glance_duration_without_start")))
# # -------------------------------------------------------- 23 visualization #### -------------------------------------------------
# # ---------------------------- 23_ET_AR #### -----------------------------------------------------
# AR_all
# AR_ic

# 23_ET_AR_all  ----------------------------------------------------- 
## subset subscales ##
AR_L0_ic <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_ic) %>%
  dplyr::rename(score = TC01_AR_ic) %>%
  add_column(scale = "L0_ic", .after = "VPNr")
AR_L0_street <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_street) %>%
  dplyr::rename(score = TC01_AR_street) %>%
  add_column(scale = "L0_street", .after = "VPNr")
AR_L0_surt <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_surt) %>%
  dplyr::rename(score = TC01_AR_surt) %>%
  add_column(scale = "L0_surt", .after = "VPNr")
AR_L0_wheel <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_wheel) %>%
  dplyr::rename(score = TC01_AR_wheel) %>%
  add_column(scale = "L0_wheel", .after = "VPNr")

AR_L2_ic <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_ic) %>%
  dplyr::rename(score = TC07_AR_ic) %>%
  add_column(scale = "L2_ic", .after = "VPNr")
AR_L2_street <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_street) %>%
  dplyr::rename(score = TC07_AR_street) %>%
  add_column(scale = "L2_street", .after = "VPNr")
AR_L2_surt <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_surt) %>%
  dplyr::rename(score = TC07_AR_surt) %>%
  add_column(scale = "L2_surt", .after = "VPNr")
AR_L2_wheel <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_wheel) %>%
  dplyr::rename(score = TC07_AR_wheel) %>%
  add_column(scale = "L2_wheel", .after = "VPNr")

AR_L3_ic <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_ic) %>%
  dplyr::rename(score = TC04_AR_ic) %>%
  add_column(scale = "L3_ic", .after = "VPNr")
AR_L3_street <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_street) %>%
  dplyr::rename(score = TC04_AR_street) %>%
  add_column(scale = "L3_street", .after = "VPNr")
AR_L3_surt <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_surt) %>%
  dplyr::rename(score = TC04_AR_surt) %>%
  add_column(scale = "L3_surt", .after = "VPNr")
AR_L3_wheel <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_wheel) %>%
  dplyr::rename(score = TC04_AR_wheel) %>%
  add_column(scale = "L3_wheel", .after = "VPNr")


## build subset ##
AR_all <- bind_rows(AR_L0_ic, AR_L0_street, AR_L0_surt, AR_L0_wheel, 
                    AR_L2_ic, AR_L2_street, AR_L2_surt, AR_L2_wheel,
                    AR_L3_ic, AR_L3_street, AR_L3_surt, AR_L3_wheel) %>%
  mutate(scale = factor(scale, levels = c("L0_ic", "L0_street", "L0_surt", "L0_wheel",
                                          "L2_ic", "L2_street", "L2_surt", "L2_wheel",
                                          "L3_ic", "L3_street", "L3_surt", "L3_wheel"), ordered = TRUE))

## lables ##
# labels_AR_all = c("L0_ic", "L0_street", "L0_surt", "L0_wheel",
#               "L2_ic", "L2_street", "L2_surt", "L2_wheel",
#               "L3_ic", "L3_street", "L3_surt", "L3_wheel")

labels_AR_all = c("ic", "street", "surt", "wheel", 
                  "ic", "street", "surt", "wheel",
                  "ic", "street", "surt", "wheel")

p <- ggplot(AR_all, aes(x=scale, y=score, fill=HMI)) + 
  # geom_rect(aes(xmin = 0.55, xmax = 4.45, ymin = -Inf, ymax = Inf), 
  #           fill = scales::alpha("#2F2F2F", 0.001)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FF03", 0.002)) +
  geom_rect(aes(xmin = 8.5, xmax = 12.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FFFF", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 8.495, xmax = 8.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 100, label = "L0", size = 3, vjust = 1) +
  annotate("text", x = 6.5, y = 100, label = "L2", size = 3, vjust = 1) +
  annotate("text", x = 10.5, y = 100, label = "L3", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_AR_all) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Eyes on AOI [%]", x="",
       title = "Attention Ratios while driving in L0, L2 and L3") +
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

plot_23_ET_AR_all <- g

ggsave(filename = "data/results/figures/23_ET_AR_all.png", g, 
       width = 10, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_AR_surt ----------------------------------------------------- 
## subset subscales ##
AR_L0_surt <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC01_AR_surt) %>%
  dplyr::rename(score = TC01_AR_surt) %>%
  add_column(scale = "L0_surt", .after = "VPNr")
AR_L2_surt <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC07_AR_surt) %>%
  dplyr::rename(score = TC07_AR_surt) %>%
  add_column(scale = "L2_surt", .after = "VPNr")
AR_L3_surt <- data_23_AR %>%
  select(Exp, VPNr, HMI, TC04_AR_surt) %>%
  dplyr::rename(score = TC04_AR_surt) %>%
  add_column(scale = "L3_surt", .after = "VPNr")

## build subset ##
AR_surt <- bind_rows(AR_L0_surt, AR_L2_surt, AR_L3_surt) %>%
  mutate(scale = factor(scale, levels = c("L0_surt", "L2_surt", "L3_surt"), ordered = TRUE))

## lables ##
labels_AR_surt = c("L0", "L2", "L3")

p <- ggplot(AR_surt, aes(x=scale, y=score, fill=HMI)) + 
  # geom_rect(aes(xmin = 0.55, xmax = 4.45, ymin = -Inf, ymax = Inf), 
  #           fill = scales::alpha("#2F2F2F", 0.001)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FF03", 0.002)) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 100),
            fill = scales::alpha("#00FFFF", 0.002)) +
  geom_rect(aes(xmin = 1.495, xmax = 1.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 2.495, xmax = 2.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_AR_surt) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Eyes on AOI [%]", x="",
       title = "Attention Ratio SuRT while driving in L0, L2 and L3") +
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

plot_23_ET_AR_surt <- g

ggsave(filename = "data/results/figures/23_ET_AR_surt.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc",
                        "plot_12_ET_TO_all_n_glances_after_start", "plot_12_ET_TO_all_total_duration",
                        "plot_12_ET_TO_all_mean_duration", "plot_12_ET_TO_all_max_duration", 
                        "plot_12_ET_TO_all_1st_glance_duration_without_start",
                        "plot_12_ET_TO_ic_nth_gaze_to_ic_dot", "plot_12_ET_TO_ic_nth_gaze_to_ic_box",
                        "plot_12_ET_TO_ic_glance_allocation_time", "plot_12_ET_TO_ic_n_glances_after_start_dot", "plot_12_ET_TO_ic_n_glances_after_start_box",
                        "plot_12_ET_TO_ic_total_duration", "plot_12_ET_TO_ic_mean_duration", "plot_12_ET_TO_ic_max_duration", 
                        "plot_12_ET_TO_ic_1st_glance_duration_without_start",
                        "plot_23_ET_AR_all", "plot_23_ET_AR_surt")))

# # ---------------------------- 23_ET_TO #### -----------------------------------------------------
# overall
# n_gazes_till_EB_or_TO
# glance at start - distribution
# all AOIs 
# n_glances_after_start
# total_duration
# mean_duration
# max_duration
# 1st_glance_duration_without_start
# ic
# nth_gaze_to_ic
# glance_allocation_time
# n_glances_after_start
# total_duration
# mean_duration
# max_duration
# 1st_glance_duration_without_start

# 23_ET_TO_n_gazes_till_EB_or_TO #### -----------------------------------------------------
## subset subscales ##
TO_TC10_n_gazes_till_EB_or_TO <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_n_gazes_till_EB_or_TO) %>%
  dplyr::rename(score = TC10_n_gazes_till_EB_or_TO) %>%
  add_column(scale = "TC10", .after = "VPNr")

TO_TC12_n_gazes_till_EB_or_TO <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_n_gazes_till_EB_or_TO) %>%
  dplyr::rename(score = TC12_n_gazes_till_EB_or_TO) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_all_n_gazes_till_EB_or_TO <- bind_rows(TO_TC10_n_gazes_till_EB_or_TO, TO_TC12_n_gazes_till_EB_or_TO) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12"), ordered = TRUE))

## lables ##
labels_TO_all_n_gazes_till_EB_or_TO = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_all_n_gazes_till_EB_or_TO, aes(x=scale, y=score, fill=HMI)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=0.5) +
  scale_x_discrete(labels = labels_TO_all_n_gazes_till_EB_or_TO) +
  scale_y_continuous(limits = c(0,13), breaks = seq(0,12,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances to AOIs until\ntake-over or start of emergency brake") +
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

plot_23_ET_TO_all_n_gazes_till_EB_or_TO <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_n_gazes_till_EB_or_TO.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_all_glance_at_start -----------------------------------------------------------
## subset subscales ##
TO_TC10_ic_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_glance_at_start) %>%
  dplyr::rename(score = TC10_ic_glance_at_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_glance_at_start) %>%
  dplyr::rename(score = TC10_street_glance_at_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_glance_at_start) %>%
  dplyr::rename(score = TC10_surt_glance_at_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_glance_at_start) %>%
  dplyr::rename(score = TC10_wheel_glance_at_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_glance_at_start) %>%
  dplyr::rename(score = TC12_ic_glance_at_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_glance_at_start) %>%
  dplyr::rename(score = TC12_street_glance_at_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_glance_at_start) %>%
  dplyr::rename(score = TC12_surt_glance_at_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_glance_at_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_glance_at_start) %>%
  dplyr::rename(score = TC12_wheel_glance_at_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")

## build subset ##
TO_all_glance_at_start <- bind_rows(TO_TC10_ic_glance_at_start, TO_TC10_street_glance_at_start,
                                    TO_TC10_surt_glance_at_start, TO_TC10_wheel_glance_at_start,
                                    TO_TC12_ic_glance_at_start, TO_TC12_street_glance_at_start,
                                    TO_TC12_surt_glance_at_start, TO_TC12_wheel_glance_at_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

TO_all_glance_at_start_perc <- TO_all_glance_at_start %>%
  select(-c(VPNr)) %>%
  group_by(Exp, HMI, scale) %>%
  skim_ML() %>%
  select(Exp, HMI, scale, numeric.mean) %>%
  dplyr::rename(mean = numeric.mean)

## lables ##
labels_TO_all_glance_at_start = c("ic", "street", "surt", "wheel", 
                                  "ic", "street", "surt", "wheel")

## n ##
p <- ggplot(TO_all_glance_at_start, aes(x=scale, y = score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 30),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 30),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 30, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 30, label = "RtI without time budget", size = 3, vjust = 1) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_TO_all_glance_at_start) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Frequency", x="",
       title = "Frequency of glances to AOI at start of request to intervene") +
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

plot_23_ET_TO_all_glance_at_start <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_glance_at_start.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


## Percentage ##
p <- ggplot(TO_all_glance_at_start_perc, aes(x=scale, y = mean*100, fill=HMI)) + 
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 105),
            fill = scales::alpha("#F8FAA0", 0.05)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 105, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 105, label = "RtI without time budget", size = 3, vjust = 1) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(labels = labels_TO_all_glance_at_start) +
  scale_y_continuous(limits = c(0,105), breaks = seq(0,100, 20)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Proportion [%]", x="",
       title = "Proportion of glances to AOI at start of request to intervene") +
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

plot_23_ET_TO_all_glance_at_start_perc <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_glance_at_start_perc.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc",
                        "plot_12_ET_TO_all_n_glances_after_start", "plot_12_ET_TO_all_total_duration",
                        "plot_12_ET_TO_all_mean_duration", "plot_12_ET_TO_all_max_duration", 
                        "plot_12_ET_TO_all_1st_glance_duration_without_start",
                        "plot_12_ET_TO_ic_nth_gaze_to_ic_dot", "plot_12_ET_TO_ic_nth_gaze_to_ic_box",
                        "plot_12_ET_TO_ic_glance_allocation_time", "plot_12_ET_TO_ic_n_glances_after_start_dot", "plot_12_ET_TO_ic_n_glances_after_start_box",
                        "plot_12_ET_TO_ic_total_duration", "plot_12_ET_TO_ic_mean_duration", "plot_12_ET_TO_ic_max_duration", 
                        "plot_12_ET_TO_ic_1st_glance_duration_without_start",
                        "plot_23_ET_AR_all", "plot_23_ET_AR_surt",
                        "plot_23_ET_TO_all_n_gazes_till_EB_or_TO", "plot_23_ET_TO_glance_at_start", "plot_23_ET_TO_glance_at_start_perc")))

# 23_ET_TO_all_n_glances_after_start_dot #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_n_glances_after_start) %>%
  dplyr::rename(score = TC10_street_n_glances_after_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC10_surt_n_glances_after_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC10_wheel_n_glances_after_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_n_glances_after_start) %>%
  dplyr::rename(score = TC12_street_n_glances_after_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC12_surt_n_glances_after_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC12_wheel_n_glances_after_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC10_street_n_glances_after_start,
                                          TO_TC10_surt_n_glances_after_start, TO_TC10_wheel_n_glances_after_start,
                                          TO_TC12_ic_n_glances_after_start, TO_TC12_street_n_glances_after_start,
                                          TO_TC12_surt_n_glances_after_start, TO_TC12_wheel_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_n_glances_after_start = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_n_glances_after_start = c("ic", "street", "surt", "wheel", 
                                        "ic", "street", "surt", "wheel")

p <- ggplot(TO_all_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 8, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 8, label = "RtI without time budget", size = 3, vjust = 1) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.15, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_n_glances_after_start) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances after start of request to intervene") +
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

plot_23_ET_TO_all_n_glances_after_start_dot <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_n_glances_after_start_dot.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_all_n_glances_after_start_box #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_n_glances_after_start) %>%
  dplyr::rename(score = TC10_street_n_glances_after_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC10_surt_n_glances_after_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC10_wheel_n_glances_after_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_n_glances_after_start) %>%
  dplyr::rename(score = TC12_street_n_glances_after_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_n_glances_after_start) %>%
  dplyr::rename(score = TC12_surt_n_glances_after_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_n_glances_after_start) %>%
  dplyr::rename(score = TC12_wheel_n_glances_after_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC10_street_n_glances_after_start,
                                          TO_TC10_surt_n_glances_after_start, TO_TC10_wheel_n_glances_after_start,
                                          TO_TC12_ic_n_glances_after_start, TO_TC12_street_n_glances_after_start,
                                          TO_TC12_surt_n_glances_after_start, TO_TC12_wheel_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_n_glances_after_start = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_n_glances_after_start = c("ic", "street", "surt", "wheel", 
                                        "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  annotate("text", x = 2.5, y = 8, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 8, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_n_glances_after_start) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances after start of request to intervene") +
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

plot_23_ET_TO_all_n_glances_after_start_box <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_n_glances_after_start_box.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_all_total_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_total_duration) %>%
  dplyr::rename(score = TC10_ic_total_duration) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_total_duration) %>%
  dplyr::rename(score = TC10_street_total_duration) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_total_duration) %>%
  dplyr::rename(score = TC10_surt_total_duration) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_total_duration) %>%
  dplyr::rename(score = TC10_wheel_total_duration) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_total_duration) %>%
  dplyr::rename(score = TC12_ic_total_duration) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_total_duration) %>%
  dplyr::rename(score = TC12_street_total_duration) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_total_duration) %>%
  dplyr::rename(score = TC12_surt_total_duration) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_total_duration) %>%
  dplyr::rename(score = TC12_wheel_total_duration) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_total_duration <- bind_rows(TO_TC10_ic_total_duration, TO_TC10_street_total_duration,
                                   TO_TC10_surt_total_duration, TO_TC10_wheel_total_duration,
                                   TO_TC12_ic_total_duration, TO_TC12_street_total_duration,
                                   TO_TC12_surt_total_duration, TO_TC12_wheel_total_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_total_duration = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_total_duration = c("ic", "street", "surt", "wheel", 
                                 "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_total_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_total_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Total duration: Eyes on AOI after start of request to intervene") +
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

plot_23_ET_TO_all_total_duration <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_total_duration.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# 23_ET_TO_all_mean_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_mean_duration) %>%
  dplyr::rename(score = TC10_ic_mean_duration) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_mean_duration) %>%
  dplyr::rename(score = TC10_street_mean_duration) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_mean_duration) %>%
  dplyr::rename(score = TC10_surt_mean_duration) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_mean_duration) %>%
  dplyr::rename(score = TC10_wheel_mean_duration) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_mean_duration) %>%
  dplyr::rename(score = TC12_ic_mean_duration) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_mean_duration) %>%
  dplyr::rename(score = TC12_street_mean_duration) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_mean_duration) %>%
  dplyr::rename(score = TC12_surt_mean_duration) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_mean_duration) %>%
  dplyr::rename(score = TC12_wheel_mean_duration) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_mean_duration <- bind_rows(TO_TC10_ic_mean_duration, TO_TC10_street_mean_duration,
                                  TO_TC10_surt_mean_duration, TO_TC10_wheel_mean_duration,
                                  TO_TC12_ic_mean_duration, TO_TC12_street_mean_duration,
                                  TO_TC12_surt_mean_duration, TO_TC12_wheel_mean_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_mean_duration = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_mean_duration = c("ic", "street", "surt", "wheel", 
                                "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_mean_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_mean_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Mean duration: Eyes on AOI after start of request to intervene") +
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

plot_23_ET_TO_all_mean_duration <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_mean_duration.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# 23_ET_TO_all_max_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_max_duration) %>%
  dplyr::rename(score = TC10_ic_max_duration) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_max_duration) %>%
  dplyr::rename(score = TC10_street_max_duration) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_max_duration) %>%
  dplyr::rename(score = TC10_surt_max_duration) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_max_duration) %>%
  dplyr::rename(score = TC10_wheel_max_duration) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_max_duration) %>%
  dplyr::rename(score = TC12_ic_max_duration) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_max_duration) %>%
  dplyr::rename(score = TC12_street_max_duration) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_max_duration) %>%
  dplyr::rename(score = TC12_surt_max_duration) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_max_duration) %>%
  dplyr::rename(score = TC12_wheel_max_duration) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_max_duration <- bind_rows(TO_TC10_ic_max_duration, TO_TC10_street_max_duration,
                                 TO_TC10_surt_max_duration, TO_TC10_wheel_max_duration,
                                 TO_TC12_ic_max_duration, TO_TC12_street_max_duration,
                                 TO_TC12_surt_max_duration, TO_TC12_wheel_max_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_max_duration = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_max_duration = c("ic", "street", "surt", "wheel", 
                               "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_max_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_max_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Max duration: Eyes on AOI after start of request to intervene") +
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

plot_23_ET_TO_all_max_duration <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_max_duration.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# 23_ET_TO_all_1st_glance_duration_without_start #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_ic", .after = "VPNr")
TO_TC10_street_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_street_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_street_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_street", .after = "VPNr")
TO_TC10_surt_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_surt_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_surt_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_surt", .after = "VPNr")
TO_TC10_wheel_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_wheel_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_wheel_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10_wheel", .after = "VPNr")

TO_TC12_ic_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_ic", .after = "VPNr")
TO_TC12_street_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_street_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_street_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_street", .after = "VPNr")
TO_TC12_surt_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_surt_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_surt_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_surt", .after = "VPNr")
TO_TC12_wheel_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_wheel_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_wheel_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12_wheel", .after = "VPNr")



## build subset ##
TO_all_1st_glance_duration_without_start <- bind_rows(TO_TC10_ic_1st_glance_duration_without_start, TO_TC10_street_1st_glance_duration_without_start,
                                                      TO_TC10_surt_1st_glance_duration_without_start, TO_TC10_wheel_1st_glance_duration_without_start,
                                                      TO_TC12_ic_1st_glance_duration_without_start, TO_TC12_street_1st_glance_duration_without_start,
                                                      TO_TC12_surt_1st_glance_duration_without_start, TO_TC12_wheel_1st_glance_duration_without_start) %>%
  mutate(scale = factor(scale, levels = c("TC10_ic", "TC10_street", "TC10_surt", "TC10_wheel",
                                          "TC12_ic", "TC12_street", "TC12_surt", "TC12_wheel"), ordered = TRUE))

## lables ##
# labels_TO_all_1st_glance_duration_without_start = c("RtI_planned_ic", "RtI_planned_street", "RtI_planned_surt","RtI_planned_wheel",
#                                             "RtI_malfunction_ic", "RtI_malfunction_street", "RtI_malfunction_surt","RtI_malfunction_wheel")

labels_TO_all_1st_glance_duration_without_start = c("ic", "street", "surt", "wheel", 
                                                    "ic", "street", "surt", "wheel")


p <- ggplot(TO_all_1st_glance_duration_without_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 4.495, xmax = 4.505, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 4.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 4.505, xmax = 8.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  annotate("text", x = 2.5, y = 20, label = "RtI with time budget", size = 3, vjust = 1) +
  annotate("text", x = 6.45, y = 20, label = "RtI without time budget", size = 3, vjust = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  scale_x_discrete(labels = labels_TO_all_1st_glance_duration_without_start) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Duration of first glance to AOI after start of request to intervene") +
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

plot_23_ET_TO_all_1st_glance_duration_without_start <- g

ggsave(filename = "data/results/figures/23_ET_TO_all_1st_glance_duration_without_start.png", g, 
       width = 8, height = 6, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc",
                        "plot_12_ET_TO_all_n_glances_after_start", "plot_12_ET_TO_all_total_duration",
                        "plot_12_ET_TO_all_mean_duration", "plot_12_ET_TO_all_max_duration", 
                        "plot_12_ET_TO_all_1st_glance_duration_without_start",
                        "plot_12_ET_TO_ic_nth_gaze_to_ic_dot", "plot_12_ET_TO_ic_nth_gaze_to_ic_box",
                        "plot_12_ET_TO_ic_glance_allocation_time", "plot_12_ET_TO_ic_n_glances_after_start_dot", "plot_12_ET_TO_ic_n_glances_after_start_box",
                        "plot_12_ET_TO_ic_total_duration", "plot_12_ET_TO_ic_mean_duration", "plot_12_ET_TO_ic_max_duration", 
                        "plot_12_ET_TO_ic_1st_glance_duration_without_start",
                        "plot_23_ET_AR_all", "plot_23_ET_AR_surt",
                        "plot_23_ET_TO_all_n_gazes_till_EB_or_TO", "plot_23_ET_TO_glance_at_start", "plot_23_ET_TO_glance_at_start_perc",
                        "plot_23_ET_TO_all_n_glances_after_start", "plot_23_ET_TO_all_total_duration",
                        "plot_23_ET_TO_all_mean_duration", "plot_23_ET_TO_all_max_duration", 
                        "plot_23_ET_TO_all_1st_glance_duration_without_start")))

# 23_ET_TO_ic_nth_gaze_to_ic_dot #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_nth_gaze_to_ic <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC10_nth_gaze_to_ic) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_nth_gaze_to_ic <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC12_nth_gaze_to_ic) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_nth_gaze_to_ic <- bind_rows(TO_TC10_ic_nth_gaze_to_ic, TO_TC12_ic_nth_gaze_to_ic) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_nth_gaze_to_ic = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_nth_gaze_to_ic, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  # stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  # geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.15, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_nth_gaze_to_ic) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number", x="",
       title = "First glance to ic after [...] glances to other AOIs\nafter start of request to intervene") +
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

plot_23_ET_TO_ic_nth_gaze_to_ic_dot <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_nth_gaze_to_ic_dot.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_ic_nth_gaze_to_ic_box #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_nth_gaze_to_ic <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC10_nth_gaze_to_ic) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_nth_gaze_to_ic <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_nth_gaze_to_ic) %>%
  dplyr::rename(score = TC12_nth_gaze_to_ic) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_nth_gaze_to_ic <- bind_rows(TO_TC10_ic_nth_gaze_to_ic, TO_TC12_ic_nth_gaze_to_ic) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_nth_gaze_to_ic = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_nth_gaze_to_ic, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 5),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_nth_gaze_to_ic) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number", x="",
       title = "First glance to ic after [...] glances to other AOIs\nafter start of request to intervene") +
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

plot_23_ET_TO_ic_nth_gaze_to_ic_box <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_nth_gaze_to_ic_box.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_ic_glance_allocation_time #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_glance_allocation_time <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_glance_allocation_time) %>%
  dplyr::rename(score = TC10_ic_glance_allocation_time) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_glance_allocation_time <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_glance_allocation_time) %>%
  dplyr::rename(score = TC12_ic_glance_allocation_time) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_glance_allocation_time <- bind_rows(TO_TC10_ic_glance_allocation_time, TO_TC12_ic_glance_allocation_time) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_glance_allocation_time = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_glance_allocation_time, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_glance_allocation_time) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Time [s]", x="",
       title = "Glance allocation time to ic after start of request to intervene") +
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

plot_23_ET_TO_ic_glance_allocation_time <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_glance_allocation_time.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# 23_ET_TO_ic_n_glances_after_start_dot #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC12_ic_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_n_glances_after_start = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 7),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 7),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_violin(fill= "transparent") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio=1.3, dotsize=0.2, binwidth = 0.5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_n_glances_after_start) +
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7,2)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances to ic after start of request to intervene") +
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

plot_23_ET_TO_ic_n_glances_after_start_dot <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_n_glances_after_start_dot.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_ic_n_glances_after_start_box #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC10_ic_n_glances_after_start) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_n_glances_after_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_n_glances_after_start) %>%
  dplyr::rename(score = TC12_ic_n_glances_after_start) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_n_glances_after_start <- bind_rows(TO_TC10_ic_n_glances_after_start, TO_TC12_ic_n_glances_after_start) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_n_glances_after_start = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_n_glances_after_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 8),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_n_glances_after_start) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Number of glances", x="",
       title = "Number of glances to ic after start of request to intervene") +
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

plot_23_ET_TO_ic_n_glances_after_start_box <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_n_glances_after_start_box.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_ic_total_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_total_duration) %>%
  dplyr::rename(score = TC10_ic_total_duration) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_total_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_total_duration) %>%
  dplyr::rename(score = TC12_ic_total_duration) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_total_duration <- bind_rows(TO_TC10_ic_total_duration, TO_TC12_ic_total_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_total_duration = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_total_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_total_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Total duration: Eyes on ic after start of request to intervene") +
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

plot_23_ET_TO_ic_total_duration <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_total_duration.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')

# 23_ET_TO_ic_mean_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_mean_duration) %>%
  dplyr::rename(score = TC10_ic_mean_duration) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_mean_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_mean_duration) %>%
  dplyr::rename(score = TC12_ic_mean_duration) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_mean_duration <- bind_rows(TO_TC10_ic_mean_duration, TO_TC12_ic_mean_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_mean_duration = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_mean_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_mean_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Mean duration: Eyes on ic after start of request to intervene") +
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

plot_23_ET_TO_ic_mean_duration <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_mean_duration.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# 23_ET_TO_ic_max_duration #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_max_duration) %>%
  dplyr::rename(score = TC10_ic_max_duration) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_max_duration <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_max_duration) %>%
  dplyr::rename(score = TC12_ic_max_duration) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_max_duration <- bind_rows(TO_TC10_ic_max_duration, TO_TC12_ic_max_duration) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_max_duration = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_max_duration, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_max_duration) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Max duration: Eyes on ic after start of request to intervene") +
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

plot_23_ET_TO_ic_max_duration <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_max_duration.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# 23_ET_TO_ic_1st_glance_duration_without_start #### -----------------------------------------------------
## subset subscales ##
TO_TC10_ic_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC10_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC10_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC10", .after = "VPNr")
TO_TC12_ic_1st_glance_duration_without_start <- data_23_TO %>%
  select(Exp, VPNr, HMI, TC12_ic_1st_glance_duration_without_start) %>%
  dplyr::rename(score = TC12_ic_1st_glance_duration_without_start) %>%
  add_column(scale = "TC12", .after = "VPNr")

## build subset ##
TO_ic_1st_glance_duration_without_start <- bind_rows(TO_TC10_ic_1st_glance_duration_without_start, TO_TC12_ic_1st_glance_duration_without_start) %>%
  mutate(scale = factor(scale, levels = c("TC10", "TC12_ic"), ordered = TRUE))

labels_TO_ic_1st_glance_duration_without_start = c("RtI with\ntime budget", "RtI without\ntime budget")

p <- ggplot(TO_ic_1st_glance_duration_without_start, aes(x=scale, y=score, fill=HMI)) + 
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F8FAA0", 0.002)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 20),
            fill = scales::alpha("#F59562", 0.002)) +
  geom_rect(aes(xmin = 1.497, xmax = 1.503, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#2F2F2F", 1)) +
  geom_rect(aes(xmin = 0.505, xmax = 1.505, ymin = 19.94, ymax = 20.0), 
            fill = scales::alpha("red", 0.8)) +
  geom_rect(aes(xmin = 1.505, xmax = 2.505, ymin = 5.97, ymax = 6.03), 
            fill = scales::alpha("red", 0.8)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 16) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.8, hjust=1.5) +
  scale_x_discrete(labels = labels_TO_ic_1st_glance_duration_without_start) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  facet_grid(HMI ~ Exp) +
  scale_fill_manual(values = c("#3070b3", "#98C6EA")) +
  labs(y="Duration [s]", x="",
       title = "Duration of first glance to ic after start of request to intervene") +
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

plot_23_ET_TO_ic_1st_glance_duration_without_start <- g

ggsave(filename = "data/results/figures/23_ET_TO_ic_1st_glance_duration_without_start.png", g, 
       width = 6, height = 6, dpi = 600, units = "in", device='png')


# #### remove not needed data ---------------------------------------------
rm(list=setdiff(ls(), c("data_all_AR", "data_all_TO", "data_12_AR", "data_23_AR", "data_12_TO", "data_23_TO",
                        "fun_mean", "fun_median", "mean_ML", "sd_ML", "skim_ML",
                        "plot_12_ET_AR_all", "plot_12_ET_AR_surt",
                        "plot_12_ET_TO_all_n_gazes_till_EB_or_TO", "plot_12_ET_TO_glance_at_start", "plot_12_ET_TO_glance_at_start_perc",
                        "plot_12_ET_TO_all_n_glances_after_start", "plot_12_ET_TO_all_total_duration",
                        "plot_12_ET_TO_all_mean_duration", "plot_12_ET_TO_all_max_duration", 
                        "plot_12_ET_TO_all_1st_glance_duration_without_start",
                        "plot_12_ET_TO_ic_nth_gaze_to_ic_dot", "plot_12_ET_TO_ic_nth_gaze_to_ic_box",
                        "plot_12_ET_TO_ic_glance_allocation_time", "plot_12_ET_TO_ic_n_glances_after_start_dot", "plot_12_ET_TO_ic_n_glances_after_start_box",
                        "plot_12_ET_TO_ic_total_duration", "plot_12_ET_TO_ic_mean_duration", "plot_12_ET_TO_ic_max_duration", 
                        "plot_12_ET_TO_ic_1st_glance_duration_without_start",
                        "plot_23_ET_AR_all", "plot_23_ET_AR_surt",
                        "plot_23_ET_TO_all_n_gazes_till_EB_or_TO", "plot_23_ET_TO_glance_at_start", "plot_23_ET_TO_glance_at_start_perc",
                        "plot_23_ET_TO_all_n_glances_after_start", "plot_23_ET_TO_all_total_duration",
                        "plot_23_ET_TO_all_mean_duration", "plot_23_ET_TO_all_max_duration", 
                        "plot_23_ET_TO_all_1st_glance_duration_without_start",
                        "plot_23_ET_TO_ic_nth_gaze_to_ic_dot", "plot_23_ET_TO_ic_nth_gaze_to_ic_box",
                        "plot_23_ET_TO_ic_glance_allocation_time", "plot_23_ET_TO_ic_n_glances_after_start_dot", "plot_23_ET_TO_ic_n_glances_after_start_box",
                        "plot_23_ET_TO_ic_total_duration", "plot_23_ET_TO_ic_mean_duration", "plot_23_ET_TO_ic_max_duration", 
                        "plot_23_ET_TO_ic_1st_glance_duration_without_start")))