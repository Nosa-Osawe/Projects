library(tidyverse)
library(haven)
library(MASS)
library(dplyr)



becky_sec1 <- read.csv("C:\\Users\\HP\\Desktop\\GS\\section1_becky.csv")
attach(becky_sec1)
view(becky_sec1)
str(becky_sec1)

becky_sec1$ADHG <- factor(becky_sec1$ADHG , levels = c("Low", "Medium", "High"))
becky_sec1$Age <- as.numeric(becky_sec1$Age)



attach(becky_sec1)

Demo_model <- polr(ADHG ~ . -ADH,
                    data = becky_sec1 ,
                    Hess = TRUE)
summary(Demo_model)



# Fit the ordinal logistic regression model
Demo_model <- polr(ADHG ~ . -ADH,
                   data = becky_sec1 ,
                   Hess = TRUE)

# Extract coefficient estimates and their standard errors
coef_summary <- summary(Demo_model)$coefficients

# Calculate Wald test statistics and p-values
wald_stats <- coef_summary[, "Value"] / coef_summary[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(wald_stats)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary <- cbind(coef_summary, wald_stats, p_values)

# Print the updated summary
print(coef_summary)
