
#Load tidyverse and other important libraries
library(tidyverse)
library(wesanderson)

#Import data 
my_data <- read.csv('01 Raw data /sf and age effect on sperm motility block one.csv')

#View data structure and summary
head(my_data)
str(my_data)
summary(my_data)

#Plot data to visualize relationships
ggplot(my_data, aes(x = Apps, y = Enroll, color = Private)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter plot of Apps vs Enrolment",
       x = "Sperm motility",
       y = "Sperm length")

#Make a box plot to compare sperm length across species
ggplot(my_data, aes(x = specie, y = sperm_length, fill = temperature_levels)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Box plot of Sperm length by Species",
       x = "Species",
       y = "Sperm length")

#fit the model
model <- lm(avg_sperm_motilty ~ sperm_length * specie + temperature_levels, data = my_data)

#fit a mixed effect model
library(lme4)
lmm <- lmer(avg_sperm_motilty ~ sperm_length * specie + temperature_levels + (1+ experiment_id), data = my_data)

#View model summary
summary(lmm)

#Check model diagnostics
par(mfrow = c(2, 2))
plot(lmm)
par(mfrow = c(1, 1))

#load DHARMa for residual diagnostics
library(DHARMa)
model_sim <- simulateResiduals(lmm)
plot(model_sim)

#check for variance-covariance issues
correlations <- vcov(lmm)
library(car)

#run anova to check significance of predictors
anova(lmm)

#get the estimated marginal means
library(emmeans)
emm <- emmeans(lmm, ~ sperm_length | specie)

#plot the estimated marginal means
plot(emm)

#save emmean as dataframe
emm_df <- as.data.frame(emm)

#pairwise comparison between treatment groups 
simp <- pairs(emm)

#perform joint tests to test the significance of the difference between predictors 
joint_tests(emm)

