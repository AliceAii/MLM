#######3#Two-level models with Random Intercept and Random Slopes#########
#ECLS-K:2011 as dataset
rm(list=ls())

library(lme4)
library(lmerTest)
library(dplyr)

data <- readRDS("/Users/aishuhan/Desktop/EDUC 231D Multilevel Analysis/Class Exercises/05_InClassData/05_ecls_ex_ris.rds")

#create school mean ses
data <- data %>%
  group_by(schid) %>%
  mutate(schoolmeanses = mean(famses, na.rm = TRUE))

#create famsesc which centered on school mean ses
data <- data %>%
  mutate(famsesc = famses - schoolmeanses, na.rm = TRUE)

#lmer linear mixed effect regression, model with random slopes
m1 <- lmer(g1rscore ~ 1 + famsesc + (1 + famsesc | schid), data = data)
summary(m1)

#model without random slopes
m2 <- lmer(g1rscore ~ 1 + famsesc + (1 | schid), data = data)
summary(m2)

#test model fit 
#AIC BIC, likelihood raition test
#m1 have lower LRT
anova(m2, m1, test = "LRT")