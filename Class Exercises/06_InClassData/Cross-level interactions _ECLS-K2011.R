#################Cross-Level Interactions###############
#####Intercepts and slopes as outcome##############
rm(list = ls())
library(dplyr)
library(lme4)
library(lmerTest)

#inpute data
data <- readRDS("/Users/aishuhan/Desktop/EDUC 231D Multilevel Analysis/Class Exercises/06_InClassData/06_ecls_ex_xlevel.rds")
head(data)

#create famsesc which centered on school mean ses
data <- data %>%
  mutate(famsesc = famses - schses, na.rm = TRUE)

#model with random slopes
m1 <- lmer(g1rscore ~ 1 + famsesc + (1 + famsesc | schid), data = data)
summary(m1)

#model with cross-level interactions
m2 <- lmer(g1rscore ~ 1 + famsesc + sector + sector*famsesc 
           + (1 + famsesc | schid), data = data)
summary(m2)

#calculate grand mean ses
grand_mean_ses <- mean(data$schses, na.rm = TRUE)
grand_mean_ses

#compute schsesgdm
data <- data %>%
  mutate(schsesdgm = schses - grand_mean_ses)

#model with cross-level interactions, expanding the intercept and slope models
m3 <- lmer(g1rscore ~ 1 + sector + schsesdgm
           + famsesc + sector*famsesc + schsesdgm*famsesc +
             + (1 + famsesc | schid), data = data)

summary(m3)

