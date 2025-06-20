library(tidyverse)
library(readxl)

blue_lights <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Blue lights.xlsx",
                          sheet = 'Sheet1')

set.seed(123)

Dry_eyes <- sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.3, 0.7))
Blurred_Vision <- sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.21, 0.79))
Eye_Strain <- sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.245, 0.755))
Headache <- sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.35, 0.65))
Shoulder <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.48, 0.52))
Irritation_burning <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.345, 0.655))
Red_eyes <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.245, 0.755))
foreign_body <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.25, 0.75))
prickling_sensation <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.445, 0.555))
Photophobia <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.3, 0.7))
Watery_eyes <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.20, 0.80))
Diplopia <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.42, 0.58))
Halo <-  sample(c("No","Yes"), size = 100, replace = TRUE, prob = c(0.34, 0.66))


eye_symptoms_visual_parameters <- data.frame(
  Dry_eyes,
  Blurred_Vision,
  Eye_Strain,
  Headache,
  Shoulder,
  Irritation_burning,
  Red_eyes,
  foreign_body,
  prickling_sensation,
  Photophobia,
  Watery_eyes,
  Diplopia,
  Halo
)

write.csv(eye_symptoms_visual_parameters, file= "C:\\Users\\DELL\\Desktop\\visual.csv")


not_go_to_sleep_within_30_min <-  sample(c("Not during the past month",
                                           "Less than once a week",
                                           "Once or twice a week",
                                           "Three or more times a week"
                                           ), size = 100,
                                         replace = TRUE, 
                                         prob = c(0.30, 0.35, 0.15, 0.20))

wake_up_middle_of_night_or_early_morning <-  sample(c("Not during the past month",
                                                      "Less than once a week",
                                                      "Once or twice a week", 
                                                      "Three or more times a week"
                                                      ), 
                                                    size = 100, replace = TRUE, 
                                         prob = c(0.31, 0.19, 0.20, 0.30))

trouble_staying_awake_at_drivingEatingSocial <-  sample(c("Not during the past month",
                                                          "Less than once a week",
                                                          "Once or twice a week",
                                                          "Three or more times a week"), 
                                                    size = 100, replace = TRUE, 
                                                    prob = c(0.25, 0.36, 0.20, 19))


enthusiasm_to_get_things_done <-  sample(c("Not during the past month",
                                           "Less than once a week",
                                           "Once or twice a week",
                                           "Three or more times a week"), 
                                         size = 100, replace = TRUE, 
                                         prob = c(0.40, 0.20, 0.24, 0.16))

overall_sleep_quality <-  sample(c("Very good",
                                   "Fairly good",
                                   "Fairly bad",
                                   "Very bad"), 
                                 size = 100, replace = TRUE, 
                                 prob = c(0.19, 0.20, 0.45, 0.16))

 

non_visual_data <- data.frame(
  not_go_to_sleep_within_30_min,
  wake_up_middle_of_night_or_early_morning,
  trouble_staying_awake_at_drivingEatingSocial,
  enthusiasm_to_get_things_done,
  overall_sleep_quality
)

write.csv(non_visual_data, file= "C:\\Users\\DELL\\Desktop\\non_visual.csv")


#----------------------------------------------

sum(is.na(blue_lights))
names(which(colSums(is.na(blue_lights)) > 0))  # all good



c_blue_lights <- blue_lights %>% 
  rename("user" = 1,
         "see_without_glass" = 2,
         "How_long" = 3,
         "Gender" = 4,
         "Age" = 5,
         "Dry_eyes" = 6,
         "Blurred_vis" = 7,
         "Eye_stain" = 8,
         "Headache" = 9,
         "shoulder_neck_pain"= 10,
         "irritation_burn" = 11,
         "Red_eyes" = 12,
         "Foreign_body_eye" = 13,
         "prickle_sense" = 14,
         "Photophobia" = 15,
         "watery_eye" = 16,
         "Diplopia" = 17,
         "Halo" = 18,
         "no_sleep_30" = 19,
         "wake_up_midnight_earlymorn" = 20,
         "trouble_staying_awake" = 21,
         "enthusiasm" = 22,
         "overall_sleep_Qlty" = 23) %>% 
  mutate( 
         see_without_glass = ifelse(see_without_glass == "Yes", 1, 0),
         "Gender" = ifelse(Gender == "Yes", 1, 0),
         "Dry_eyes" = ifelse(Dry_eyes == "Yes", 1, 0),
         "Blurred_vis" = ifelse(Blurred_vis == "Yes", 1, 0),
         "Eye_stain" = ifelse(Eye_stain == "Yes", 1, 0),
         "Headache" = ifelse(Headache == "Yes", 1, 0),
         "shoulder_neck_pain"= ifelse(shoulder_neck_pain == "Yes", 1, 0),
         "irritation_burn" = ifelse(irritation_burn == "Yes", 1, 0),
         "Red_eyes" = ifelse(Red_eyes == "Yes", 1, 0),
         "Foreign_body_eye" = ifelse(Foreign_body_eye == "Yes", 1, 0),
         "prickle_sense" = ifelse(prickle_sense == "Yes", 1, 0),
         "Photophobia" = ifelse(Photophobia == "Yes", 1, 0),
         "watery_eye" = ifelse(watery_eye == "Yes", 1, 0),
         "Diplopia" = ifelse(Diplopia == "Yes", 1, 0),
         "Halo" = ifelse(Halo == "Yes", 1, 0))
  
# ---- build logistic models

Dry_eyes <- glm(Dry_eyes~How_long , data = c_blue_lights, 
                          family = binomial)
summary(Dry_eyes)
as.data.frame(exp(coef(Dry_eyes)))
as.data.frame(confint(Dry_eyes))



Blurred_vis <- glm(Blurred_vis~How_long , data = c_blue_lights, 
                family = binomial)
summary(Blurred_vis)
as.data.frame(exp(coef(Blurred_vis)))
as.data.frame(confint(Blurred_vis))


Eye_stain <- glm(Eye_stain~How_long , data = c_blue_lights, 
                   family = binomial)
summary(Eye_stain)
as.data.frame(exp(coef(Eye_stain)))
as.data.frame(confint(Eye_stain))


Headache <- glm(Headache~How_long , data = c_blue_lights, 
                 family = binomial)
summary(Headache)
as.data.frame(exp(coef(Headache)))
as.data.frame(confint(Headache))


shoulder_neck_pain <- glm(shoulder_neck_pain~How_long , data = c_blue_lights, 
                family = binomial)
summary(shoulder_neck_pain)
as.data.frame(exp(coef(shoulder_neck_pain)))
as.data.frame(confint(shoulder_neck_pain))



irritation_burn <- glm(irritation_burn~How_long , data = c_blue_lights, 
                          family = binomial)
summary(irritation_burn)
as.data.frame(exp(coef(irritation_burn)))
as.data.frame(confint(irritation_burn))


Red_eyes <- glm(Red_eyes~How_long , data = c_blue_lights, 
                       family = binomial)
summary(Red_eyes)
as.data.frame(exp(coef(Red_eyes)))
as.data.frame(confint(Red_eyes))



Foreign_body_eye <- glm(Foreign_body_eye~How_long , data = c_blue_lights, 
                family = binomial)
summary(Foreign_body_eye)
as.data.frame(exp(coef(Foreign_body_eye)))
as.data.frame(confint(Foreign_body_eye))


prickle_sense <- glm(prickle_sense~How_long , data = c_blue_lights, 
                        family = binomial)
summary(prickle_sense)
as.data.frame(exp(coef(prickle_sense)))
as.data.frame(confint(prickle_sense))


Photophobia <- glm(Photophobia~How_long , data = c_blue_lights, 
                     family = binomial)
summary(Photophobia)
as.data.frame(exp(coef(Photophobia)))
as.data.frame(confint(Photophobia))


watery_eye <- glm(watery_eye~How_long , data = c_blue_lights, 
                   family = binomial)
summary(watery_eye)
as.data.frame(exp(coef(watery_eye)))
as.data.frame(confint(watery_eye))


Diplopia <- glm(Diplopia~How_long , data = c_blue_lights, 
                  family = binomial)
summary(Diplopia)
as.data.frame(exp(coef(Diplopia)))
as.data.frame(confint(Diplopia))


Halo <- glm(Halo~How_long , data = c_blue_lights, 
                family = binomial)
summary(Halo)
as.data.frame(exp(coef(Halo)))
as.data.frame(confint(Halo))

####################################################################################


unique(c_blue_lights$overall_sleep_Qlty)
levels(c_blue_lights$overall_sleep_Qlty)

library(haven)
library(MASS)


# sleep quality

overall_sleep_Qlty <- polr(overall_sleep_Qlty ~ How_long,
                   data = c_blue_lights %>% 
                     mutate(overall_sleep_Qlty = factor(overall_sleep_Qlty,
                                                        levels = c("Very bad" ,
                                                                   "Fairly bad",
                                                                   "Fairly good",
                                                                   "Very good"
                                                                   ))),
                   Hess = TRUE)

overall_sleep_Qlty_coeff <- summary(overall_sleep_Qlty)$coefficients


# Calculate Wald test statistics and p-values
wald_stats_overall_sleep_Qlty<- overall_sleep_Qlty_coeff[, "Value"] / overall_sleep_Qlty_coeff[, "Std. Error"]
p_values_overall_sleep_Qlty <- 2 * (1 - pnorm(abs(wald_stats_overall_sleep_Qlty)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_overall_sleep_Qlty <- cbind(overall_sleep_Qlty_coeff, wald_stats_overall_sleep_Qlty,
                                         p_values_overall_sleep_Qlty)


print(coef_summary_overall_sleep_Qlty)

#####--- 

wake_up_midnight_earlymorn <- polr(wake_up_midnight_earlymorn ~ How_long,
data = c_blue_lights %>% 
  mutate(wake_up_midnight_earlymorn = factor(wake_up_midnight_earlymorn,
                                     levels = c("Not during the past month"  ,
                                                "Less than once a week",
                                                "Once or twice a week",
                                                "Three or more times a week"))), Hess = TRUE)

wake_up_midnight_earlymorn_coeff <- summary(wake_up_midnight_earlymorn)$coefficients


# Calculate Wald test statistics and p-values
wald_stats_wake_up_midnight_earlymorn<- wake_up_midnight_earlymorn_coeff[, "Value"] / wake_up_midnight_earlymorn_coeff[, "Std. Error"]
p_values_wake_up_midnight_earlymorn <- 2 * (1 - pnorm(abs(wald_stats_wake_up_midnight_earlymorn)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_wake_up_midnight_earlymorn <- cbind(wake_up_midnight_earlymorn_coeff, 
                                                 wald_stats_wake_up_midnight_earlymorn,
                                                 p_values_wake_up_midnight_earlymorn)


print(coef_summary_wake_up_midnight_earlymorn)



###---------------------------------------


no_sleep_30 <- polr(no_sleep_30 ~ How_long,
                                   data = c_blue_lights %>% 
                                     mutate(no_sleep_30 = factor(no_sleep_30,
                                                          
                                                                                levels = c("Not during the past month"  ,
                                                                                           "Less than once a week",
                                                                                           "Once or twice a week",
                                                                                           "Three or more times a week"))), Hess = TRUE)

no_sleep_30_coeff <- summary(no_sleep_30)$coefficients


# Calculate Wald test statistics and p-values
wald_stats_no_sleep_30<- no_sleep_30_coeff[, "Value"] / no_sleep_30_coeff[, "Std. Error"]
p_values_no_sleep_30  <- 2 * (1 - pnorm(abs(wald_stats_no_sleep_30)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_no_sleep_30 <- cbind(no_sleep_30_coeff, 
                                                 wald_stats_no_sleep_30,
                                                 p_values_no_sleep_30)

print(coef_summary_no_sleep_30)


###-----------------------------------------------------------------


wake_up_midnight_earlymorn <- polr(wake_up_midnight_earlymorn ~ How_long,
                                   data = c_blue_lights %>% 
                                     mutate(wake_up_midnight_earlymorn = factor(wake_up_midnight_earlymorn,
                                                                                levels = c("Not during the past month"  ,
                                                                                           "Less than once a week",
                                                                                           "Once or twice a week",
                                                                                           "Three or more times a week"))), Hess = TRUE)

wake_up_midnight_earlymorn_coeff <- summary(wake_up_midnight_earlymorn)$coefficients


# Calculate Wald test statistics and p-values
wald_stats_wake_up_midnight_earlymorn<- wake_up_midnight_earlymorn_coeff[, "Value"] / wake_up_midnight_earlymorn_coeff[, "Std. Error"]
p_values_wake_up_midnight_earlymorn <- 2 * (1 - pnorm(abs(wald_stats_wake_up_midnight_earlymorn)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_wake_up_midnight_earlymorn <- cbind(wake_up_midnight_earlymorn_coeff, 
                                                 wald_stats_wake_up_midnight_earlymorn,
                                                 p_values_wake_up_midnight_earlymorn)


print(coef_summary_wake_up_midnight_earlymorn)



# ---------------------- Trouble staying awake during physical actibities like driving



trouble_staying_awake <- polr(trouble_staying_awake ~ How_long,
                                   data = c_blue_lights %>% 
                                     mutate(trouble_staying_awake = factor(trouble_staying_awake,
                                                                                levels = c("Not during the past month"  ,
                                                                                           "Less than once a week",
                                                                                           "Once or twice a week",
                                                                                           "Three or more times a week"))), Hess = TRUE)

trouble_staying_awake_coeff <- summary(trouble_staying_awake)$coefficients


# Calculate Wald test statistics and p-values
wald_stats_trouble_staying_awake<- trouble_staying_awake_coeff[, "Value"] / trouble_staying_awake_coeff[, "Std. Error"]
p_values_trouble_staying_awake <- 2 * (1 - pnorm(abs(wald_stats_trouble_staying_awake)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_trouble_staying_awake <- cbind(trouble_staying_awake_coeff, 
                                                 wald_stats_trouble_staying_awake,
                                                 p_values_trouble_staying_awake)


print(coef_summary_trouble_staying_awake)



### -- ------------------------------------

enthusiasm <- polr(enthusiasm ~ How_long,
                              data = c_blue_lights %>% 
                                mutate(enthusiasm = factor(enthusiasm,
                                                                      levels = c("Not during the past month"  ,
                                                                                 "Less than once a week",
                                                                                 "Once or twice a week",
                                                                                 "Three or more times a week"))), Hess = TRUE)

enthusiasm_coeff <- summary(enthusiasm)$coefficients


# Calculate Wald test statistics and p-values
wald_stats_enthusiasm<- enthusiasm_coeff[, "Value"] / enthusiasm_coeff[, "Std. Error"]
p_values_enthusiasm <- 2 * (1 - pnorm(abs(wald_stats_enthusiasm)))

# Combine coefficients, standard errors, Wald stats, and p-values into a data frame
coef_summary_enthusiasm <- cbind(enthusiasm_coeff, 
                                 wald_stats_enthusiasm,
                                 p_values_enthusiasm)


print(coef_summary_enthusiasm)



summary(c_blue_lights$Age)
