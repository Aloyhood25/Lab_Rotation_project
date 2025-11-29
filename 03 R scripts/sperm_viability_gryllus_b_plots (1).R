#sperm viability and motility plots for Gryllus bimaculatus data---------
#Oliver Otti
#start date: 2024-06-26

#remove all objects in the environment
rm(list=ls())

#load packages
library(tidyverse)#version 2.0.0
library(ggpubr)#version 0.6.0
library(ggsci)#version 2.9
library(ggbeeswarm)#version 0.7.1
library(ggtext)#version 0.1.2
library(wesanderson)#version 0.6.1

#check package version
packageVersion("tidyverse")

#load data of sperm viability Gryllus bimaculatus----------
cricket <- read.csv("01 Raw data/anne_treatment_sperm_viability.csv")

#check data
head(cricket)
str(cricket)

#convert variables to factors
cricket$treatment <- as.factor(cricket$treatment)#create factor variable for 
#treatment
cricket$cricket_ID <- as.factor(cricket$cricket_ID)
cricket$time <- as.factor(cricket$time)

#proportion of live sperm
cricket <- cricket %>%
  mutate(prop_live = live / total)

#order treatment factor levels
cricket$treatment <- factor(cricket$treatment, 
                           levels = c("control", "own", "foreign"))

#ggplot proportion of live sperm over time per population
ggplot(cricket, aes(x = time, y = prop_live, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.5), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  facet_wrap(~cricket_population) +
  scale_color_manual(values = wes_palette("Zissou1", n = 5)[c(4,1,5)]) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Time in minutes", y = "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))

#save plot
ggsave("04 R plots/sperm_viability_gryllus_b_facet_population.pdf",
       width = 10, height = 6, dpi = 300)

#ggplot proportion of live sperm over time
ggplot(cricket, aes(x = time, y = prop_live, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
               dodge.width = 0.5), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  scale_color_manual(values = wes_palette("Zissou1", n = 5)[c(4,1,5)]) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Time in minutes", y = "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))

#save plot
ggsave("04 R plots/sperm_viability_gryllus_b.pdf", 
       width = 8, height = 6, dpi = 300)

#filter data for treatment = control and own
cricket_filtered <- cricket %>%
  filter(treatment %in% c("control", "own"))

#ggplot proportion of live sperm over time
ggplot(cricket_filtered, aes(x = time, y = prop_live, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.1), size = 1, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.1)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.1)) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.1)) +
  scale_color_manual(values = wes_palette("Royal1", n = 2)[c(1,2)], 
                     labels=c("control","microbes")) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Time in minutes", y = "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 20))

#save plot
ggsave("04 R plots/sperm_viability_gryllus_b_control_own.pdf", 
       width = 6, height = 6, dpi = 300)

#filter data for population B only control and own
filtered_cricket <- cricket %>%
  filter(treatment %in% c("control", "own") ,
           cricket_population == "B")

#ggplot proportion of live sperm over time for only population B
ggplot(filtered_cricket, aes(x = time, y = prop_live, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.1), size = 1, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.1)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.1)) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.1)) +
  scale_color_manual(values = wes_palette("Royal1", n = 2)[c(1,2)],
                     labels=c("control","microbes")) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Time in minutes", y = "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 20))

#save plot
ggsave("04 R plots/sperm_viability_gryllus_b_pop_B_control_own.pdf", 
       width = 8, height = 6, dpi = 300)

#ggplot proportion of live sperm for all populations & time points as facets
ggplot(cricket, aes(x = cricket_population, y = prop_live, color = treatment)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.5), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  facet_wrap(~time) +
  scale_color_manual(values = wes_palette("Zissou1", n = 5)[c(4,1,5)]) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Cricket population", y =  "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))


#filter data for 30 minutes time point only
cricket_30min <- cricket %>%
  filter(time == "30")

#order treatment factor levels
cricket_30min$treatment <- factor(cricket_30min$treatment, 
                                  levels = c("control", "own", "foreign"))

#ggplot proportion of live sperm at 30 minutes per population
ggplot(cricket_30min, aes(x = cricket_population, y = prop_live, color = treatment)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.5), size = 1, alpha = 0.3) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  scale_color_manual(values = wes_palette("Zissou1", n = 5)[c(4,1,5)]) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Cricket population", y =  "Proportion of live sperm", 
       color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 20))


#save plot
ggsave("04 R plots/sperm_viability_gryllus_b_30min.pdf", 
       width = 6, height = 6, dpi = 300)

#ggplot spaghetti style to account for paired data
ggplot(cricket_30min, aes(x = treatment, y = prop_live, 
                          color = cricket_population)) +
  # individual trajectories ("spaghetti" lines)
  geom_line(aes(group = cricket_ID), alpha = 0.3, linewidth = 0.7) +
  geom_point(alpha = 0.7, size = 2) +
  
  # population means (dashed line)
  stat_summary(
    aes(group = cricket_population),
    fun = mean, geom = "line",
    color = "black", linetype = "dashed", linewidth = 1
  ) +
  
  # standard error bars around mean
  stat_summary(
    aes(group = cricket_population),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1, color = "black", linewidth = 0.8
  ) +
  
  # optional: mean points on top of everything
  stat_summary(
    aes(group = cricket_population),
    fun = mean, geom = "point",
    shape = 21, fill = "white", color = "black", size = 3, stroke = 1
  ) +
  
  facet_wrap(~ cricket_population) +
  theme_classic(base_size = 13) +
  labs(
    x = "Microbe treatment",
    y = "Proportion of live sperm",
    color = "Population"
  ) +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )


#save plot
ggsave("04 R plots/sperm_viability_gryllus_b_30min_spaghetti.pdf",
       width = 6,
       height = 6,
       dpi = 300)


#load sperm motility data------------------------
motility <- read.csv("01 raw data/sperm_motility_gryllus_b.csv")

#check data
head(motility)
str(motility)

#convert variables to factors
motility$treatment <- as.factor(motility$treatment)
motility$Cricket_ID <- as.factor(motility$Cricket_ID)
motility$Cricketpopulation <- as.factor(motility$Cricketpopulation)
motility$Time <- as.factor(motility$Time)

#ggplot sperm motility over time
ggplot(motility, aes(x = Time, y = Temporal_Noise, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.5), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = .75, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  scale_color_manual(values = wes_palette("Royal1", n = 2)[c(1,2)]) +
  scale_y_continuous(breaks = seq(0,16,2)) +
  labs(x = "Time in minutes", 
       y = expression(paste("Sperm motility (temporal noise ",sigma,")")),
       color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 20))

#save plot
ggsave("04 R plots/sperm_motility_gryllus_b.pdf", 
       width = 6, height = 6, dpi = 300)

#ggplot sperm motility per population over time
ggplot(motility, aes(x = Time, y = Temporal_Noise, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.5), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = .75, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  facet_wrap(~Cricketpopulation) +
  scale_color_manual(values = wes_palette("Royal1", n = 2)[c(1,2)]) +
  scale_y_continuous(breaks = seq(0,16,2)) +
  labs(x = "Time in minutes", 
       y = expression(paste("Sperm motility (temporal noise ",sigma,")")),
       color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))

#save plot  
ggsave("04 R plots/sperm_motility_gryllus_b_facet_population.pdf", 
       width = 8, height = 6, dpi = 300)

#ggplot sperm motility over time for only population B
motility_filtered <- motility %>%
  filter(Cricketpopulation == "B")

ggplot(motility_filtered, aes(x = Time, y = Temporal_Noise, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.1), size = 1, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.1)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.1)) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = .75, 
               position = position_dodge(width = 0.1)) +
  scale_color_manual(values = wes_palette("Royal1", n = 2)[c(1,2)]) +
  scale_y_continuous(breaks = seq(0,16,2)) +
  labs(x = "Time in minutes", 
       y = expression(paste("Sperm motility (temporal noise ",sigma,")")),
       color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 20))

#save plot
ggsave("04 R plots/sperm_motility_gryllus_b_pop_B_control_own.pdf", 
             width = 6, height = 6, dpi = 300)

#Anne's data for Gryllus bimaculatus sperm viability plots----------

#load Anne's data on baseline sperm viability population A and B
anne_data <- read.csv("01 raw data/anne_baseline_sperm_viability.csv")

#check data
head(anne_data)
str(anne_data)

#convert variables to factors
anne_data$cricket_ID <- as.factor(anne_data$cricket_ID)
anne_data$time <- as.factor(anne_data$time)

#proportion of live sperm
anne_data <- anne_data %>%
  mutate(prop_live = live / total)               

#ggplot proportion of live sperm over time per population
ggplot(anne_data, aes(x = time, y = prop_live, color = population)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.75), size = 1, alpha = 0.2) +
  stat_summary(aes(group=population),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.75)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.75)) +
  scale_color_manual(values = wes_palette("Zissou1", n = 5)[c(4,1,5)]) +
  labs(x = "Time in minutes", y = "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))+
  ylim(0, 1)

#save plot
ggsave("04 R plots/sperm_viability_anne_baseline.pdf", 
       width = 8, height = 6, dpi = 300)               

#Anne's data bacterial treatment effect on sperm viability-----------

#load Anne's data on bacterial treatment effect on sperm viability
anne_treatment <- read.csv("01 raw data/anne_treatment_sperm_viability.csv")

#check data
head(anne_treatment)
str(anne_treatment)

#convert variables to factors
anne_treatment$cricket_ID <- as.factor(anne_treatment$cricket_ID)
anne_treatment$time <- as.factor(anne_treatment$time)

#create new variable for toxin treatment foreign vs own microbes
anne_treatment <- anne_treatment %>%
  mutate(treatment = ifelse(population == "A" & toxin == "A", "own",
                        ifelse(population == "B" & toxin == "B", "own",
                                 ifelse(population == "A" & toxin == "B", "foreign",
                                        ifelse(population == "B" & toxin == "A", "foreign", NA)))))


#proportion of live sperm
anne_treatment <- anne_treatment %>%
  mutate(prop_live = live / total)

#ggplot proportion of live sperm over time per treatment
ggplot(anne_treatment, aes(x = time, y = prop_live, color = treatment)) +
  geom_point(position = 
               position_jitterdodge(jitter.width = 0.2, 
                                    dodge.width = 0.75), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.75)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.75)) +
  facet_wrap(~population) +
  scale_color_manual(values = wes_palette("Royal1", n = 2)[c(1,2)]) +
  labs(x = "Time in minutes", y = "Proportion of live sperm", color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))+
  ylim(0, 1)

#save plot
ggsave("04 R plots/sperm_viability_anne_treatment.pdf", 
       width = 10, height = 6, dpi = 300)

#filter data for 30 minutes time point only
anne_treatment_30min <- anne_treatment %>%
  filter(time == "30")

#ggplot proportion of live sperm at 30 minutes per population
ggplot(anne_treatment_30min, aes(x = population, y = prop_live, color = treatment)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                             dodge.width = 0.5), size = 1, alpha = 0.2) +
  stat_summary(aes(group=treatment),fun = mean, geom = "line", linewidth = 1, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.5)) +
  scale_color_manual(values = wes_palette("Zissou1", n = 5)[c(4,1,5)]) +
  labs(x = "Cricket population", y =  "Proportion of live sperm", 
       color = "Treatment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 20))+
  ylim(0, 1)

#save plot
ggsave("04 R plots/sperm_viability_anne_treatment_30min.pdf", 
       width = 8, height = 6, dpi = 300)

#ggplot spaghetti style to account for paired data
ggplot(anne_treatment_30min, aes(x = treatment, y = prop_live, 
                          color = population)) +
  # individual trajectories ("spaghetti" lines)
  geom_line(aes(group = cricket_ID), alpha = 0.3, linewidth = 0.7) +
  geom_point(alpha = 0.7, size = 2) +
  
  # population means (dashed line)
  stat_summary(
    aes(group = population),
    fun = mean, geom = "line",
    color = "black", linetype = "dashed", linewidth = 1
  ) +
  
  # standard error bars around mean
  stat_summary(
    aes(group = population),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1, color = "black", linewidth = 0.8
  ) +
  
  # optional: mean points on top of everything
  stat_summary(
    aes(group = population),
    fun = mean, geom = "point",
    shape = 21, fill = "white", color = "black", size = 3, stroke = 1
  ) +
  
  facet_wrap(~ population) +
  theme_classic(base_size = 13) +
  labs(
    x = "Microbe treatment",
    y = "Proportion of live sperm",
    color = "Population"
  ) +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )

#save plot
ggsave("04 R plots/sperm_viability_gryllus_b_anne_treatment_30min_spaghetti.pdf", 
       width = 10, height = 6, dpi = 300)

