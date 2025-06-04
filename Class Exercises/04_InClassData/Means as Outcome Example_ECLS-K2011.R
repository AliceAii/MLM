#########class example: Means as Outcomes###########
#Example's motivating question: How much dose early grade reading achievement differ between private and public U.S. schools

#clean the environment
rm(list=ls())
library(lmerTest) #use lmerTest instead of lmer to get the df and pvalue
library(skimr)
library(tidyverse)
library(flextable)

#load the data
data <- readRDS("/Users/aishuhan/Desktop/EDUC 231D Multilevel Analysis/Class Exercises/04_InClassData/04_ecls_ex_meansasoutcomes.rds")
#ECLS-K:2011 Kindergarten — Fifth Grade, the data include 11.091 first grade students, 742 schools, 9% private schools

#######Descriptive Analysis#############
#Aggregate data at the school level
school_level_data <- data %>%
  group_by(schid, sector) %>%
  summarise(
    mean_reading = mean(schmeanreadg1, na.rm = TRUE),  # School mean reading score
    mean_ses = mean(schmeanses, na.rm = TRUE),        # School mean SES
    .groups = "drop"
  )

# Compute descriptive statistics for public and private schools
descriptive_stats <- school_level_data %>%
  group_by(sector) %>%
  summarise(
    count = n(),  # Number of schools
    mean_reading = mean(mean_reading, na.rm = TRUE),
    sd_reading = sd(mean_reading, na.rm = TRUE),
    median_reading = median(mean_reading, na.rm = TRUE),
    min_reading = min(mean_reading, na.rm = TRUE),
    max_reading = max(mean_reading, na.rm = TRUE),
    mean_ses = mean(mean_ses, na.rm = TRUE),
    sd_ses = sd(mean_ses, na.rm = TRUE),
    median_ses = median(mean_ses, na.rm = TRUE),
    min_ses = min(mean_ses, na.rm = TRUE),
    max_ses = max(mean_ses, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Sector = ifelse(sector == 0, "Public Schools", "Private Schools")) %>%
  select(-sector)

# Compute overall statistics (all schools)
overall_stats <- school_level_data %>%
  summarise(
    count = n(),
    mean_reading = mean(mean_reading, na.rm = TRUE),
    sd_reading = sd(mean_reading, na.rm = TRUE),
    median_reading = median(mean_reading, na.rm = TRUE),
    min_reading = min(mean_reading, na.rm = TRUE),
    max_reading = max(mean_reading, na.rm = TRUE),
    mean_ses = mean(mean_ses, na.rm = TRUE),
    sd_ses = sd(mean_ses, na.rm = TRUE),
    median_ses = median(mean_ses, na.rm = TRUE),
    min_ses = min(mean_ses, na.rm = TRUE),
    max_ses = max(mean_ses, na.rm = TRUE)
  ) %>%
  mutate(Sector = "Overall")

# Combine public, private, and overall stats
descriptive_table <- bind_rows(descriptive_stats, overall_stats)

# Print the descriptive table
print(descriptive_table)

#############Naive approach1: student-level analysis#############
# Fit the naive OLS regression model
naive_ols_student <- lm(g1rscore ~ sector, data = data)

# Summary of the model
summary(naive_ols_student)
#print(as_flextable(naive_ols_student), preview = "docx") #or preview "pptx" "pdf"
as_flextable(naive_ols_student)
#standard errors are too small- will lead to invalid inferences; df are inflated - each student observation is assumed to be independent
#The predicted average reading score for students in public schools (sector=0) is 94.847.The predicted difference in average reading scores between students in private schools (sector=1) and public schools is 6.553 points. the predicted average reading score for private schools is: Private Schools=𝛽0+𝛽1=94.847+6.553=101.400)

#############Naive approach2: school-level analysis#############

#fit the naive ols school level model
naive_ols_school <- lm(mean_reading ~ sector, data = school_level_data)
summary(naive_ols_school)
#print(as_flextable(naive_ols_student), preview = "docx")
as_flextable(naive_ols_school)

#############Multilevel analysis#############
multilevel_model <- lmer(g1rscore ~ sector + (1 | schid), data = data)
summary(multilevel_model)
as_flextable(multilevel_model)
#the predicted school mean readin score for public schools is 94.657; the difference in reading mean between private and public schools is 6.5, so the predicted reading score for private school is 94.657 + 6.512 = 101.169
#the between school variance tau_00 is 49.645, the residual variance (within school) variance, theta sq, is 258.686

##Multilevel analysis sector, and ses
m_rscore <- lmer(g1rscore ~ sector + schmeansesc + (1 | schid), data = data)
summary(m_rscore)
as_flextable(m_rscore)

##Plot the reading difference
# Create the plot
ggplot(school_level_data, aes(x = mean_ses, y = mean_reading)) +
  geom_point(color = "steelblue2", alpha = 0.6) +  # Add points
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Add regression line
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, 0.5)) +  # Set x-axis scale
  scale_y_continuous(limits = c(60, 120), breaks = seq(60, 120, 10)) +  # Set y-axis scale
  facet_wrap(~sector, labeller = as_labeller(c(`0` = "Public Schools", `1` = "Private Schools"))) +
  labs(
    title = "Relationship Between School SES and Reading Achievement",
    x = "Mean SES (schmeansesc)",
    y = "Mean Reading Achievement (\u03B2\u2080\u1D62)"  # Unicode for β₀ⱼ
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
