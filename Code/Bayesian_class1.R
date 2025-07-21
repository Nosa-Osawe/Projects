# Simulated data
set.seed(123)  # for reproducibility
study_hours <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
exam_score <- 40 + 5 * study_hours + rnorm(10, mean = 0, sd = 3)  # linear with some noise

# Combine into a data frame
data <- data.frame(study_hours, exam_score)

# View the data
print(data)
 
library(rstanarm)

# Fit Bayesian linear regression
fit <- stan_glm(exam_score ~ study_hours, data = data,
                prior = normal(0, 10),         # prior on slope
                prior_intercept = normal(0, 10),  # prior on intercept
                chains = 4, iter = 2000, seed = 123)

summary(fit)

freq_model <- lm(exam_score ~ study_hours, data = data)
summary(freq_model)



