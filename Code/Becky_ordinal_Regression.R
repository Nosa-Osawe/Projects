library(tidyverse)
library(haven)
library(MASS)


becky_sec1 <- read.csv("C:\\Users\\HP\\Desktop\\GS\\section1_becky.csv")
attach(becky_sec1)
view(becky_sec1)
str(becky_sec1)

becky_sec1$ADHG <- factor(becky_sec1$ADHG , levels = c("Low", "Medium", "High"))
becky_sec1$Age <- as.numeric(becky_sec1$Age)

unique(becky_sec1$income)
becky_sec1$income <- factor(becky_sec1$income, 
                            levels = c("Less than #20,000",
                                        "#21,000 - #50,000",
                                        "#51,000- #100,000",
                                       "above #100,000"))

attach(becky_sec1)

# Fit the ordinal logistic regression model
Demo_model <- polr(ADHG ~ . -ADH -Occupation,
                   data = becky_sec1 ,
                   Hess = TRUE)
summary(Demo_model)
# Extract coefficient estimates and their standard errors
coef_summary_demo <- summary(Demo_model)$coefficients

# Calculate Wald test statistics and p-values
wald_stats_demo <- coef_summary_demo[, "Value"] / coef_summary_demo[, "Std. Error"]
p_values_demo <- 2 * (1 - pnorm(abs(wald_stats_demo)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_d <- cbind(coef_summary_demo, wald_stats_demo, p_values_demo)

# Print the updated summary
print(coef_summary_d)
view(coef_summary_d)

write.csv(coef_summary_d,  file = "C:\\Users\\HP\\Desktop\\GS\\demo_ord_output.csv")

################################################################################################################


sectionB <- read.csv("C:\\Users\\HP\\Desktop\\GS\\sectionB_becky.csv")

view(sectionB)
attach(sectionB)

sectionB$ADHG <- factor(sectionB$ADHG, levels = c("Low", "Medium", "High"))

str(sectionB)
sectionB$Which_eye <-factor(sectionB$Which_eye, levels = c("Both", "Left", "Right"))
sectionB$Accept_other_management. <- factor(sectionB$Accept_other_management. , levels = c("No",
                                                                                           "maybe", "Yes"))

sectionB_model <- polr(ADHG ~ first_diagnosed+How_were_you_diagnosed+
                         surgical_eyes+blind_fam+first_diagnosed+How_were_you_diagnosed+are_drugs_expensive.+
                         Accept_other_management.+eye_pressure.+ symptoms.when.your.eye.pressure.is.high.+
                         normal_eye_pressure+Which_eye
                         a
                       -ADH,
                   data = sectionB ,
                   Hess = TRUE)
summary(sectionB_model)

# Extract coefficient estimates and their standard errors
coef_B <- summary(sectionB_model)$coefficients

# Calculate Wald test statistics and p-values
wald_stats_B <- coef_B[, "Value"] / coef_B[, "Std. Error"]
p_values_B <- 2 * (1 - pnorm(abs(wald_stats_B)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_B<- cbind(coef_B, wald_stats_B, p_values_B)

# Print the updated summary
print(coef_summary_B)
view(coef_summary_B)

write.csv(coef_summary_B,  file = "C:\\Users\\HP\\Desktop\\GS\\sectionB_ord_output.csv")

################################################################################################################

sectionC <- read.csv("C:\\Users\\HP\\Desktop\\GS\\Section C.csv")
view(sectionC)

attach(sectionC)

likert <- c("1","2","3","4","5")
group <- c("Strongly Disagree",
           "Disagree",
           "Neutral",
           "Agree",
           "Strongly agree")

sectionC$Know13 <- factor(sectionC$Know13, levels = group,labels = likert)
sectionC$Know13  <- as.integer(as.character(sectionC$Know13)) 

sectionC$Know5 <- factor(sectionC$Know5, levels = group,labels = likert)
sectionC$Know5  <- as.integer(as.character(sectionC$Know5)) 

sectionC$Know8 <- factor(sectionC$Know8, levels = group,labels = likert)
sectionC$Know8 <- as.integer(as.character(sectionC$Know8)) 

sectionC$Know16 <- factor(sectionC$Know16, levels = group,labels = likert)
sectionC$Know16 <- as.integer(as.character(sectionC$Know16)) 

sectionC$Know20 <- factor(sectionC$Know20, levels = group,labels = likert)
sectionC$Know20 <- as.integer(as.character(sectionC$Know20)) 

sectionC$Know21 <- factor(sectionC$Know21, levels = group,labels = likert)
sectionC$Know21 <- as.integer(as.character(sectionC$Know21)) 

sectionC$Know22 <- factor(sectionC$Know22, levels = group,labels = likert)
sectionC$Know22 <- as.integer(as.character(sectionC$Know22))


sum(is.na(sectionC))

sectionC <- na.omit(sectionC)
sectionC$ADHG <- factor(sectionC$ADHG, levels = c("Low", "Medium", "High"))

c_model <- polr(ADHG ~.-ADH,
                         data = sectionC,
                         Hess = TRUE)
summary(c_model)


# Extract coefficient estimates and their standard errors
coef_c <- summary(c_model)$coefficients

# Calculate Wald test statistics and p-values
wald_stats_c <- coef_c[, "Value"] / coef_c[, "Std. Error"]
p_values_c <- 2 * (1 - pnorm(abs(wald_stats_c)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_c<- cbind(coef_c, wald_stats_c, p_values_c)

# Print the updated summary
print(coef_summary_c)
view(coef_summary_c)

write.csv(coef_summary_c,  file = "C:\\Users\\HP\\Desktop\\GS\\sectionc_ord_output.csv")


##################################################################################

added <- read.csv("C:\\Users\\HP\\Desktop\\GS\\added_table_becky.csv")
view(added)

sum(is.na(added))
attach(added)


likert <- c("1","2","3","4","5")
group <- c("Strongly Disagree",
           "Disagree",
           "Neutral",
           "Agree",
           "Strongly agree")

added$Know1 <- factor(added$Know1, levels = group,labels = likert)
added$Know1  <- as.integer(as.character(added$Know1)) 

added$Know10 <- factor(added$Know10, levels = group,labels = likert)
added$Know10  <- as.integer(as.character(added$Know10)) 

added$Know17 <- factor(added$Know17, levels = group,labels = likert)
added$Know17  <- as.integer(as.character(added$Know17)) 

added$Know18 <- factor(added$Know18, levels = group,labels = likert)
added$Know18  <- as.integer(as.character(added$Know18))

added$Know2 <- factor(added$Know2, levels = group,labels = likert)
added$Know2  <- as.integer(as.character(added$Know2))

added$Know23 <- factor(added$Know23, levels = group,labels = likert)
added$Know23  <- as.integer(as.character(added$Know23))

added$Know24 <- factor(added$Know24, levels = group,labels = likert)
added$Know24  <- as.integer(as.character(added$Know24))

added$Know25 <- factor(added$Know25, levels = group,labels = likert)
added$Know25  <- as.integer(as.character(added$Know25))

added$Know3 <- factor(added$Know3, levels = group,labels = likert)
added$Know3  <- as.integer(as.character(added$Know3))

added$Know4 <- factor(added$Know4, levels = group,labels = likert)
added$Know4  <- as.integer(as.character(added$Know4))

added$Know6 <- factor(added$Know6, levels = group,labels = likert)
added$Know6  <- as.integer(as.character(added$Know6))

added$ADHG <- factor(added$ADHG, levels = c("Low", "Medium", "High"))
a_model <- polr(ADHG~., data = added, Hess = TRUE)
summary(a_model)


# Extract coefficient estimates and their standard errors
coef_a <- summary(a_model)$coefficients

# Calculate Wald test statistics and p-values
wald_stats_a <- coef_a[, "Value"] / coef_a[, "Std. Error"]
p_values_a <- 2 * (1 - pnorm(abs(wald_stats_a)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_a<- cbind(coef_a, wald_stats_a, p_values_a)

# Print the updated summary
print(coef_summary_a)
view(coef_summary_a)

write.csv(coef_summary_a,  file = "C:\\Users\\HP\\Desktop\\GS\\added_ord_output.csv")







