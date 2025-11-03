rm(list=ls())

#load additional packages to have all the necessary functions for graphs and
# available
library(tidyverse)#this package contains all we need, is this not nice
library(wesanderson)
library(ggplot2)
library(dplR)
library(dplyr)
#load data into R
my_data <- read.csv('01 Raw data /sperm_motility_generic.csv', header=T)
#Description of variables in the data set
#sperm_age: age of the sperm sample in days (2, 5, 40, 48)
#time_points: time points of measurement in minutes (0, 2, 4, 6)
#treatment_1: presence or absence of seminal fluid (with_SF, without_SF)
#treatment_2: age of the seminal fluid (young_SF, old_SF)
#mean_sperm_motility: mean sperm motility measured at each time point


head(my_data)
#assign 'min' to the time points 
my_data $ time.factor <- paste(my_data $ time_points, "min")
#order the levels
my_data $ time.factor <- ordered(my_data $ time.factor, 
                                 levels = c("0 min", "2 min", "4 min", "6 min"))
my_data $ sperm_age.factor <- paste(my_data $ sperm_age, "days")


#order the age levels
my_data $ sperm_age.factor <- ordered(my_data $ sperm_age.factor, 
                                      levels = c("2 days", "5 days", "40 days", "48 days"))
glimpse(my_data)


# Create age groups based on threshold
age <- c(2, 5, 40, 48)

threshold = 30

#my_data $ age_group <- ifelse(my_data $ age <= threshold, 'young', 'old')

glimpse(my_data)
tail(my_data)
#rename the treatment columns 
my_data <- my_data %>%
  rename(
    SF_presence = treatment_1,
    SF_age = treatment_2
  )
glimpse(my_data)

general_mean_SM <- my_data %>%
  group_by(sperm_age.factor, SF_presence, SF_age, time.factor) %>%
  summarise(mean_SM = mean(mean_sperm_motility, na.rm = TRUE),
            se_SM = sd(mean_sperm_motility)/sqrt(n()))

glimpse(general_mean_SM)
head(general_mean_SM)

levels(general_mean_SM$time.factor) <- c("0", "2", "4", "6")
# Plot sperm motility over time by age group and SF presence
ggplot()+
  geom_point(data = general_mean_SM, aes(x = time.factor, y = mean_SM, 
          color = SF_presence, shape = SF_age), size = 2) +
  facet_wrap(~ sperm_age.factor) +
  geom_line()+
  geom_line(data = general_mean_SM, aes(x = time.factor, y = mean_SM, 
          group = interaction(SF_presence, SF_age), color = SF_presence), 
          linewidth = 0.5, 
          alpha = 0.5) +
  theme_minimal() +
  geom_errorbar(data = general_mean_SM, 
                aes(x = time.factor, y = mean_SM, 
                    ymin = mean_SM - se_SM, 
                    ymax = mean_SM + se_SM, 
                    color = SF_presence), width = 0.1) +
  labs(title = "Sperm Motility Over Time by Age Group and SF Presence",
       x = "Time Points (min)",
       y = "Sperm Motility") +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position = "top")
