#age effect on sperm motility in Drosophila melanogaster block one
#Oliver Otti
#10.05.22

#BEFORE WE START SOME THINGS TO REMEMBER
# always comment code
# indent and spacing, use control+i to tell R to indent your code
# keep yourself to 80 characters per line, if possible: split comments and 
# function calls if necessary
# never save the environment!

# Load packages ----------------------------------------------------------

#clear R's brain
rm(list=ls())

#load additional packages to have all the necessary functions for graphs and
# available
library(tidyverse)#this package contains all we need, is this not nice
library(wesanderson)

# Load data block one----------------------------------------------------------

my_data_block1<-read.csv("01 raw data/sf and age effect on sperm motility block one.csv",
                   header=T)

#verify if your data loaded and if all variables are correctly assigned
str(my_data_block1)

# Sperm motility plots block one ----------------------------------------------

#we look at sperm motility over 6 min
#add "min" to the measurement timepoint to make it look nicer in the figure
my_data_block1$time<-paste(my_data_block1$measurement.timepoint.min,"min")

#order the levels
my_data_block1$time <- ordered(my_data_block1$time, levels = c("0 min", 
                              "2 min", "4 min","6 min"))
#convert to factors
my_data_block1$age.factor<-as.factor(my_data_block1$male.age)
my_data_block1$time.factor<-as.factor(my_data_block1$time)
my_data_block1$vesicle <- as.factor(my_data_block1$seminal.vesicle)

#calculate the difference for each measurement timepoint of "with seminal fluid"
#minus "without seminal fluid"
withSF<-filter(my_data_block1,treatment=="with seminal fluid")
withoutSF<-filter(my_data_block1,treatment=="without seminal fluid")
motility.difference1<-left_join(withSF,withoutSF,by = c("maleID","male.age",
                                          "measurement.timepoint.min","time"))
motility.difference1$motility.dif<-motility.difference1$mean.sperm.motility.x-
  motility.difference1$mean.sperm.motility.y
    
#mean sperm motility
mean.motility_block1<-my_data_block1 %>% group_by(maleID,treatment,male.age,age.factor) %>% 
  summarise(mean.sperm.motility=mean(mean.sperm.motility,na.rm=T))

pdf("04 R plots/sperm motility block one/sf and age effect on sperm motility block 1.pdf")

#using stat_summary means of means
ggplot(mean.motility_block1, aes(x=age.factor,y=mean.sperm.motility))+
  geom_point(aes(colour=treatment),alpha=0.3,position=position_jitter(width=.2),)+
  stat_summary(aes(colour=treatment))+
  xlab("Male age in days")+
  ylab(expression(paste("Sperm motility (temporal noise ",sigma,")")))+
  labs(colour="Treatment")+
  scale_y_continuous(breaks=c(1:16))+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  guides(alpha="none")+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

#using geom_smooth now this plot is made from the mean of sperm motility for
#each male across the time point
ggplot(mean.motility_block1, aes(x=male.age,y=mean.sperm.motility))+
  geom_smooth(aes(colour=treatment),method="loess",formula="y~x")+
  xlab("Male age in days")+
  ylab(expression(paste("Mean sperm motility (temporal noise ",sigma,")")))+
  labs(colour="Treatment")+
  scale_y_continuous(breaks=c(1:16))+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

# Recode factor levels by name
levels(my_data_block1$vesicle) <- list(first  = "1", second = "2")

#vesicle processing effect
ggplot(my_data_block1, aes(x=measurement.timepoint.min,y=mean.sperm.motility))+
  geom_point(aes(colour=vesicle),alpha=0.4,position=position_jitter(width=.2))+
  geom_smooth(aes(colour=vesicle),method="lm", formula="y~x")+
  #facet_wrap(~male.age,ncol=10)+
  facet_grid(cols=vars(male.age),rows=vars(treatment))+
  xlab("Measurement time in min")+
  ylab(expression(paste("Sperm motility (temporal noise ",sigma,")")))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20))+
  scale_x_continuous(breaks=c(0,2,4,6))+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  labs(colour="Seminal vesicle processing")+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

ggplot(my_data_block1, aes(x=measurement.timepoint.min,y=mean.sperm.motility))+
  geom_smooth(aes(colour=treatment),method="loess",formula="y~x")+
  geom_point(aes(colour=treatment),alpha=0.4,position=position_jitter(width=.2),)+
  facet_wrap(~male.age,ncol=10)+
  xlab("Measurement time in min")+
  ylab(expression(paste("Sperm motility (temporal noise ",sigma,")")))+
  labs(colour="Treatment")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20))+
  scale_x_continuous(breaks=c(0,2,4,6))+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  guides(alpha="none")+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

dev.off()

# Alternative plots block one --------------------------------------------------

pdf("04 R plots/sperm motility block one/alternative plot types for sf and age effect on sperm motility block 1.pdf")

ggplot(my_data_block1, aes(x=measurement.timepoint.min,y=mean.sperm.motility))+
  geom_point(aes(colour=treatment),alpha=0.4,position=position_jitter(width=.2))+
  geom_smooth(aes(colour=treatment),method="lm", formula="y~x")+
  facet_wrap(~male.age,ncol=10)+
  xlab("Measurement time in min")+
  ylab(expression(paste("Sperm motility (temporal noise ",sigma,")")))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20))+
  scale_x_continuous(breaks=c(0,2,4,6))+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  labs(colour="Treatment")+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

# Sperm motility difference with SF vs without SF block one --------------------

#plot of the differences in sperm motility within male and over the timepoints.
#not a very useful plot
motility.difference1$male<-as.factor(motility.difference1$maleID)
ggplot(motility.difference1, aes(x=measurement.timepoint.min,y=motility.dif))+
  geom_line(aes(group=male),alpha=0.4,position=position_jitter(width=.2),
            col="grey10")+
  geom_smooth(method="lm",formula="y~x")+
  facet_wrap(~male.age,ncol=10)+
  xlab("Measurement time in min")+
  ylab(expression(paste("Difference in sperm motility (temporal noise ",sigma,")")))+
  scale_y_continuous(breaks=c(-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12))+
  scale_x_continuous(breaks=c(0,2,4,6))+
  labs(colour="Treatment")+
  theme_classic()+
  theme(text=element_text(size=18))

#overall means for the difference in sperm motility
ggplot(motility.difference1, aes(x=male.age,y=motility.dif))+
  geom_point(alpha=0.4,position=position_jitter(width=.2))+
  geom_smooth(method="loess",formula="y~x")+
  xlab("Male age in days")+
  ylab(expression(paste("Difference in sperm motility (temporal noise ",sigma,")")))+
  labs(colour="Treatment")+
  theme_classic()+
  theme(text=element_text(size=18))

dev.off()

# Estimated marginal plots sperm motility block one-----------------------------

#estimated marginal means plot
#analysis of slope differences between age groups and treatments
library(lme4)
library(lmerTest)
library(emmeans)

# Assuming your model is something like: Motility ~ Time * Treatment + (1 | Individual)

m1<-lmer(mean.sperm.motility~vesicle+time+age.factor+treatment+
           (1|maleID:treatment)+time:age.factor+
           time:treatment+age.factor:treatment+
           time:age.factor:treatment,data=my_data_block1)

library(DHARMa)
sim_m1<-simulateResiduals(m1)
plot(sim_m1)

emmip(m1, vesicle ~ treatment)
emmip(m1, vesicle ~ time)
emmip(m1, vesicle ~ age.factor)
emmip(m1, age.factor ~ time)
emmip(m1, time ~ treatment)
emmip(m1, age.factor ~ treatment)
emmip(m1, treatment ~ age.factor)

emmip(m1, vesicle ~ time | treatment | age.factor)
emmip(m1, treatment ~ time | age.factor)
emmip(m1, treatment ~ age.factor | time)
emmip(m1, time ~ treatment | age.factor)

emm_s.t <- emmeans(m1, pairwise ~ time | treatment | age.factor)
emm_s.t

summary(m1)

library(car)
Anova(m1)

write.csv(Anova(m1),
  file="02 clean data/glmm sf and age effect on sperm motility block one.csv")

emm <- emmeans(m1, specs = ~ time*age.factor*treatment)
emm
joint_tests(emm)

pdf("04 R plots/sperm motility block one/estimated marginal means sperm motility age and SF block 1.pdf")
# Plot the EMMs

emmip(emm, ~ time | age.factor, CIs = TRUE, type = "response") +
  geom_point(aes(x = time, y = mean.sperm.motility,colour=treatment), 
             data = my_data_block1, pch = 2)+
  facet_grid(treatment~age.factor)+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=8),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

emm <- emmeans(m1, specs = ~ age.factor*treatment*time)
emm_plot <- as.data.frame(emm)

levels(emm_plot$time) <- c("0", "2", "4","6")

levels(my_data_block1$time) <- c("0", "2", "4","6")

ggplot()+
  geom_line(data=emm_plot, aes(x = time, y = emmean, group = treatment, color = 
                            treatment),position=position_dodge(.5))+
  geom_point(data=emm_plot, aes(x = time, y = emmean, group = treatment, color = 
                                 treatment),position=position_dodge(.5))+
  geom_errorbar(data=emm_plot, aes(x = time, y = emmean, group = treatment, 
                                   color = treatment, ymin  =  emmean-SE,
                                   ymax  =   emmean+SE), width =  0.1, 
                                   linewidth  =  0.5,position=position_dodge(.5))+
  geom_point(data=my_data_block1, aes(x = time, y = mean.sperm.motility, 
                               group = treatment, color = 
                          treatment),alpha=0.1,position=position_dodge(.5))+
  facet_grid(~age.factor)+
  labs(x = "Time in minutes", 
       y = expression(paste("Sperm motility (temporal noise ",sigma,")")),
       color="Treatment",
       title="Male age in days")+
  scale_color_manual(values=c("darkred", "#56B4E9"))+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

dev.off()

my_data_t0<-my_data_block1 %>% filter(measurement.timepoint.min=="0")

pdf("04 R plots/sperm motility block one/max sperm motility block 1.pdf")
ggplot(data=my_data_t0, aes(x = age.factor, y = mean.sperm.motility,
                              fill = treatment))+
  geom_boxplot(alpha=0.8)+
  geom_point(position=position_dodge(0.75),colour="grey40")+
  labs(x = "Male age in days", 
       y = expression(paste("Sperm motility at t0 (temporal noise ",sigma,")")),
       fill = "Treatment")+
  scale_fill_manual(values=c("darkred", "#56B4E9"))+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")

my_data_max<-my_data_block1 %>% group_by(maleID,male.age,age.factor,treatment) %>%
  summarise(max.sperm.motility=max(mean.sperm.motility,na.rm=T))

ggplot(data=my_data_max, aes(x = age.factor, y = max.sperm.motility,
                            fill = treatment))+
  geom_boxplot(alpha=0.8)+
  geom_point(position=position_dodge(0.75),colour="grey40")+
  labs(x = "Male age in days", 
       y = expression(paste("Max sperm motility (temporal noise ",sigma,")")),
       fill = "Treatment")+
  scale_fill_manual(values=c("darkred", "#56B4E9"))+
  theme_classic()+
  theme(text=element_text(size=20),plot.title=element_text(size=18,hjust=0.5),
        axis.text.x = element_text(size=10),legend.title=element_text(size=16),
        legend.text=element_text(size=12),legend.position="bottom")
dev.off()

#glmm for max sperm motility
m2<-lmer(max.sperm.motility~age.factor+treatment+
           (1|maleID)+age.factor:treatment,data=my_data_max)

library(DHARMa)
sim_m2<-simulateResiduals(m2)
plot(sim_m2)#okish!

summary(m2)
Anova(m2)#interaction not significant, but main effects

write.csv(Anova(m2),
          file="02 clean data/glmm sf and age effect on max sperm motility block one.csv")