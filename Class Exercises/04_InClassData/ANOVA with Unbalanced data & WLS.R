#########class example: one-way ANOVA with unbalanced data & WLS###########
#Example's motivating question: What was the average math score for Grade 8 students in the United States in 2019? How much did math scores differ across schools?

#clean the environment
rm(list=ls())
library(lmerTest) #use lmerTest instead of lmer to get the df and pvalue
library(skimr)
library(tidyverse)
library(flextable)
library(brms) #Bayesian Regression Models using 'Stan'

#load Trends in International Mathematics and Science Study (TIMSS) data
data <- readRDS("/Users/aishuhan/Desktop/EDUC 231D Multilevel Analysis/Class Exercises/04_InClassData/04_timss_ex_unbalanced.rds")

#naive option1: calculate the mean math score across all students (N=661)
mean_math_st <- mean(data$bsmmatxx, na.rm = TRUE)
mean_math_st

#naive option2: calculate the mean math score per school (y hat_j), then average acrooss school (N=20)
mean_math_sch_j <- data %>%
  group_by(idschool) %>%
  summarise(mean_sch_j = mean(bsmmatxx, na.rm = TRUE), .groups = 'drop')
mean_schools_overall <- mean(mean_math_sch_j$mean_sch_j)
mean(data$schmath_j)

math_sch_var <- var(mean_math_sch_j$mean_sch_j, na.rm = TRUE)
math_sch_var

# Generate the variance plot
ggplot(mean_math_sch_j, aes(x = 1, y = mean_sch_j)) + 
  geom_point(color = "blue", size = 2) +  # Add points for each school's mean
  geom_hline(yintercept = mean_schools_overall, linetype = "solid", color = "black") +  # Add grand mean line
  annotate("text", x = 1.2, y = mean_schools_overall, label = round(mean_schools_overall, 2), color = "black", hjust = 0) +  # Annotate the grand mean
  scale_x_continuous(breaks = NULL) +  # Remove x-axis labels
  labs(y = "Average school math score", x = NULL) +  # Label y-axis
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.text.x = element_blank(),  # Remove x-axis text
        panel.grid.major.x = element_blank(),  # Remove vertical gridlines
        panel.grid.minor.x = element_blank()) +
  coord_cartesian(clip = "off")   # Prevent clipping of text annotations

#######model-based approach#######
#estimate the hierarchical model in R: Y_ij = gamma_00 + mu_0j + r_ij
m1 <- lmer(bsmmatxx ~ 1 + (1 | idschool), data = data)
print(as_flextable(m1), preview = "pptx")
summary(m1)

# Compute Level-1 error variance
level1_variance <- data %>%
  summarise(
    error_var = sum((bsmmatxx - schmath_j)^2, na.rm = TRUE) / (nrow(data) - n_distinct(idschool))
  )  %>% pull(error_var)  # Extract as a scalar value
print(level1_variance) #hat sigma^ 2 = 5440

# Estimate between-school variance (random effect variance) from the model
random_effect_variance <- as.data.frame(VarCorr(m1)) %>%
  filter(grp == "idschool") %>%
  pull(vcov)  # Extract as a scalar value
print(random_effect_variance)  # Should output 3558

# Calculate total variance for each school mean
# Create the school-level dataset
school_data <- data %>%
  group_by(idschool) %>%
  summarise(
    j = cur_group_id(), #assign a sequential number for each school,
    schn = n(), #the number of students per school
    schmath_j = mean(bsmmatxx, na.rm = TRUE),
    .groups = "drop"
  )

#add V_j and SE_j to the school-level dataset
school_data <- school_data %>%
  mutate(
    V_j = level1_variance / schn, #error variance for each school
    SE_j = sqrt(V_j) #standard error for each school
  )


#in the unbalanced case, the error terms are not identically distributed, using the balanced approach to estimate hat tau_00 and hat gamam_00 when the groups are unbalanced will produce biased results.

#so, a soluation is to use WLS (weighted least squares)
tau_00 <- random_effect_variance
sigma_sq <- level1_variance

print(tau_00)
print(sigma_sq) 
#Step1: Compute total variance (D_j) delta_j
school_data <- school_data %>%
  mutate(
    D_j = tau_00 + (sigma_sq / schn)  # Total variance for each school
  )

#Step2: ompute weights and their proportions (wgtpct)
school_data <- school_data %>%
  mutate(
    weight = 1 / D_j, #weight shows the precision of each school's contribution based on its variance.
    wgtpct = (weight / sum(weight)) *100 
  )  #Larger schools with smaller total variance (D_j) contribute more to the grand mean. In other word, for school j6, its schmath_j (619.35) account for 5% of the grand mean.
print(school_data)

#After getting the weight, there are several analysis could do: 1. compute the precision-weighted grand mean (hat gamma_00); 
# 2.compute empirical Bayes Estimates (hat beta*_0j); 3. Investigate Between-school variablity; 4. Explore predictors of school means

#Step3: Firstly, compute the precision-weighted grand mean (minimum variance unbiased estimator of gamma_00)
precision_weighted_grand_mean <- sum(school_data$weight * school_data$schmath_j) / sum(school_data$weight)
precision_weighted_grand_mean #it's a value, 482.757

#Step4: Then, compute the empirical Bayes estimates (shrinkage estimator)
school_data <- school_data %>%
  mutate(
    lambda_j = tau_00 / D_j, #reliability (lambda_j)
    EB_j = lambda_j * schmath_j + (1 - lambda_j) * precision_weighted_grand_mean #EB_j is Empirical Bayes estimate, or called shrinkage estimator, or called BLUP (best linear unbiased predictor)
  )
view(school_data)

#Then, viasulaize the results, scatter plot comparing schmath_j and EB_j
#create the CI
school_data <- school_data %>%
  mutate(
    lower_CI_EB_j = EB_j - 1.96 * SE_j,  # Lower bound of the confidence interval
    upper_CI_EB_j = EB_j + 1.96 * SE_j   # Upper bound of the confidence interval
  )
# create the plot
#reorder means the y-axis (y) display school IDs (idschool) in ascending order of their EB.
ggplot(school_data, aes(y = reorder(idschool, EB_j))) +
  #add EB estimates as steelblue2
  geom_point(aes(x = EB_j), size = 3, color = "steelblue2") +
  #add original school means as black hollow circles
  geom_point(aes(x = schmath_j), size = 3, shape = 1, color = "black") +
  #add confidence intervals for EB estimator
  geom_errorbarh(aes(xmin = lower_CI_EB_j, xmax = upper_CI_EB_j), height = 0.2, color = "royalblue3") +
  #add a vertical dashed line for the grand mean
  geom_vline(xintercept = precision_weighted_grand_mean, linetype = "dashed", color = "black") +
  #labels and title
  labs(
    x = "Grade 8 Math Score",
    y = "School ID",
    title = "EB Estimate vs. Original School Mean Math Scores (β*0j and β0j)") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
    panel.grid.minor = element_blank(),    # Remove minor gridlines
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

#compute ICC (interclass correlation coefficient)
ICC <- tau_00 / (tau_00 + sigma_sq)
print(ICC) # 39.54% Proportion of variance explained by between-school differences

#Next, we can use the weights computed from the school_data dataset in a hierarchical linear regression model using the data (student-level dataset). These weights can be used to account for the precision of the school-level contributions in the model.
data <- data %>%
  left_join(select(school_data, idschool, weight), by = "idschool")
view(data)

#then fit a weighted hierarchical model
hlm_weighted <- lmer(bsmmatxx ~ homeses + (1 | idschool), data = data, weights = weight)
summary(hlm_weighted)

#compare with unweighted
hlm_unweighted <- lmer(bsmmatxx ~ homeses + (1 | idschool), data = data)
summary(hlm_unweighted)

#Misunderstanding of the weight, the weight thing is already included in the HLM analysis, no need to specify the weights = weight, the difference about the two model is because the hlm_weighted is double weight, the hlm_unweighted is already weighted in the analysis (behind the screen). And the precision_weighted_grand_mean just equals the grand mean! So, in the actual analysis, there is no need to imply weights = weight.  

