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
data_all_TO <- read.csv("data/eyetracking/data_all_TO.csv", encoding = "UTF-8")
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
labels_AR_all = c("L0_ic", "L0_street", "L0_surt", "L0_wheel",
              "L2_ic", "L2_street", "L2_surt", "L2_wheel",
              "L3_ic", "L3_street", "L3_surt", "L3_wheel")

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

plot_12_ET_AR_surt <- g

ggsave(filename = "data/results/figures/12_ET_AR_surt.png", g, 
       width = 5, height = 6, dpi = 600, units = "in", device='png')
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
