#Analysis of the age effect on sperm motility in Drosophila melanogaster
#Oliver Otti
#10.05.22

#BEFORE WE START SOME THINGS TO REMEMBER
# always comment code
# indent and spacing, use control+i to tell R to indent your code
# keep yourself to 80 characters per line, if possible: split comments and 
# function calls if necessary
# never save the environment!

# Loading packages and data -----------------------------------------------

#clear R's brain
rm(list=ls())

#load additional packages to have all the necessary functions for graphs and
# available
library(tidyverse)#this package contains all we need, is this not nice
library(wesanderson)

#load data into R
my_data<-read.csv("01 raw data/sf and age effect on sperm motility block one.csv",
                  header=T)

#verify if your data loaded and if all variables are correctly assigned
str(my_data)

# Sperm motility plots ---------------------------------------------------

#add "min" to the measurement timepoint to make it look nices in the figure
my_data$time<-paste(my_data$measurement.timepoint.min,"min")

#order the levels
my_data$time <- ordered(my_data$time, levels = c("0 min","2 min", "4 min","6 min"))
my_data$age.factor<-as.factor(my_data$male.age)

#calculate the difference for each measurement timepoint of "with seminal fluid"
#minus "without seminal fluid"
withSF<-filter(my_data,treatment=="with seminal fluid")
withoutSF<-filter(my_data,treatment=="without seminal fluid")
motility.difference<-left_join(withSF,withoutSF,by = c("maleID","male.age",
                                                       "measurement.timepoint.min","time"))
motility.difference$motility.dif<-motility.difference$mean.sperm.motility.x-
  motility.difference$mean.sperm.motility.y

slopes<-my_data %>% 
  group_by(maleID,treatment,male.age,age.factor) %>%
  summarise(across(starts_with("mean.sperm.motility"),
                   list(slope = ~lm(. ~ measurement.timepoint.min)$coef[2])))

# Mixed model analysis ----------------------------------------------------

# load packages for the stats and assumption testing
library(lme4)
library(lmerTest)
library(DHARMa)

#full model with maleID as a random effect with complete data set
my_data$OLRE <- seq_len(nrow(my_data))
m1<-lmer(mean.sperm.motility~seminal.vesicle+time+age.factor+treatment+
          (1|maleID:treatment)+time:age.factor+
           time:treatment+age.factor:treatment+
           time:age.factor:treatment,data=my_data)

#simulate residuals to check if model assumptions are ok
sim_m1<-simulateResiduals(m1)
plot(sim_m1)#looks okish, lower quantile line red!

#only call summary() and anova() once the model assumptions are ok
summary(m1)
anova(m1,test="F")


m1.1<-lmer(mean.sperm.motility~seminal.vesicle+age.factor+treatment+time+
           (1|maleID:treatment)+age.factor:time,
           data=my_data)

#simulate residuals to check if model assumptions are ok
sim_m1.1<-simulateResiduals(m1.1)
plot(sim_m1.1)#all looks ok

#only call summary() and anova() once the model assumptions are ok
summary(m1.1)
anova(m1.1,test="F")

#mean sperm motility
mean.motility<-my_data %>% group_by(maleID,treatment,male.age,age.factor) %>% 
  summarise(mean.sperm.motility=mean(mean.sperm.motility,na.rm=T))

#full model with maleID as a random effect with means per male and treatment level
m2<-lmer(mean.sperm.motility~age.factor+treatment+(1|maleID)+age.factor:treatment, 
         data=mean.motility)
#model singular!!!! but why?

#simulate residuals to check if model assumptions are ok
sim_m2<-simulateResiduals(m2)
plot(sim_m2)#all ok

#only call summary() and anova() once the model assumptions are ok
summary(m2)
anova(m2,test="F")

#model with only the main effects, maleID as a random effect with means per male and treatment level
m3<-lmer(mean.sperm.motility~age.factor+treatment+(1|maleID), 
         data=mean.motility)
#model singular! why?

#simulate residuals to check if model assumptions are ok
sim_m3<-simulateResiduals(m3)
plot(sim_m3)#all ok

#only call summary() and anova() once the model assumptions are ok
summary(m3)
anova(m3,test="F",type=2)

#analysis of slope differences between age groups and treatments
library(lme4)
library(lmerTest)
library(emmeans)

# Assuming your model is something like: Motility ~ Time * Treatment + (1 | Individual)

m1<-lmer(mean.sperm.motility~time+age.factor+treatment+
           (1|maleID:treatment)+time:age.factor+
           time:treatment+age.factor:treatment+
           time:age.factor:treatment,data=my_data)

summary(m1)
anova(m1)

write.csv(anova(m1),file="02 clean data/glmm sperm motility.csv")

emm <- emmeans(m1, specs = ~ age.factor*treatment*time)
emm_plot <- as.data.frame(emm)

levels(emm_plot$time) <- c("0", "2", "4","6")

pdf("04 R plots/estimated marginal means.pdf")
# Plot the EMMs
ggplot(emm_plot, aes(x = time, y = emmean, group = treatment, color = treatment)) +
  geom_line() +
  geom_point() +
  facet_grid(~age.factor)+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  labs(x = "Time in minutes", y = "Estimated marginal mean",color="Treatment",
       title="Male age")+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12))
dev.off()

# Assuming you have already fitted the linear mixed-effects model 'model' as shown earlier

# Perform post-hoc pairwise comparisons using Tukey's HSD test
posthoc_male_age <- emmeans(emm, specs = ~ age.factor)

# Conduct Tukey's HSD test
Tukey_results_male_age <- pairs(posthoc_male_age, adjust = "tukey")
Tukey_results_male_age

# Perform post-hoc pairwise comparisons using Tukey's HSD test
posthoc_time <- emmeans(emm, specs = ~ time)

# Conduct Tukey's HSD test
Tukey_results_time <- pairs(posthoc_time, adjust = "tukey")
Tukey_results_time

# Assuming you have 'Tukey_results' object from the Tukey's HSD test
# and you have 'ggplot2' package installed and loaded
# Visualize the Tukey's HSD results
plot(Tukey_results_male_age)
plot(Tukey_results_time)

