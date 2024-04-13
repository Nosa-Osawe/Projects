library(tidyverse)

melanomafor <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\melanomafor_stat1.csv")
view(melanomafor)

str(melanomafor)
attach(melanomafor)

###########################        Summary statistics:  summary per Ulcer

melanomafor$status <- factor(melanomafor$status, 
                             levels = c(1, 3, 2), 
                             labels = c("died from melanoma", 
                                        "died from others",
                                        "still alive"))

melanomafor$ulcer <- factor(melanomafor$ulcer, 
                             levels = c(0, 1), 
                             labels = c("absent", 
                                        "present"))

melanomafor$sex <- factor(melanomafor$sex, 
                            levels = c(0, 1), 
                            labels = c("Female", 
                                       "Male"))

time_summary_status<- aggregate(melanomafor$time,
                        by = list(as.factor(melanomafor$status)),
                        FUN = function(x) c(median = median(x), 
                                            mean = mean(x),
                                            sd = sd(x), 
                                            Min = min(x),
                                            Max = max(x),
                                            se = sqrt(var(x)/length(x))))
time_summary_status

age_summary_status<- aggregate(melanomafor$age,
                                by = list(as.factor(melanomafor$status)),
                                FUN = function(x) c(median = median(x), 
                                                    mean = mean(x),
                                                    sd = sd(x), 
                                                    Min = min(x),
                                                    Max = max(x),
                                                    se = sqrt(var(x)/length(x))))
age_summary_status


thickness_summary_status<- aggregate(melanomafor$thickness,
                               by = list(as.factor(melanomafor$status)),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x),
                                                   sd = sd(x), 
                                                   Min = min(x),
                                                   Max = max(x),
                                                   se = sqrt(var(x)/length(x))))
thickness_summary_status

year_summary_status<- aggregate(melanomafor$year,
                                     by = list(as.factor(melanomafor$status)),
                                     FUN = function(x) c(round(length(x)) ,
                                                         median = median(x), 
                                                         mean = mean(x),
                                                         sd = sd(x), 
                                                         Min = min(x),
                                                         Max = max(x),
                                                         se = sqrt(var(x)/length(x))))
year_summary_status

#######################    summary per Ulcer ####################################################


time_summary_ulcer<- aggregate(melanomafor$time,
                                by = list(as.factor(melanomafor$ulcer)),
                                FUN = function(x) c(median = median(x), 
                                                    mean = mean(x),
                                                    sd = sd(x), 
                                                    Min = min(x),
                                                    Max = max(x),
                                                    se = sqrt(var(x)/length(x))))
time_summary_ulcer
 c 
age_summary_ulcer<- aggregate(melanomafor$age,
                               by = list(as.factor(melanomafor$ulcer)),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x),
                                                   sd = sd(x), 
                                                   Min = min(x),
                                                   Max = max(x),
                                                   se = sqrt(var(x)/length(x))))
age_summary_ulcer


thickness_summary_ulcer<- aggregate(melanomafor$thickness,
                                     by = list(as.factor(melanomafor$ulcer)),
                                     FUN = function(x) c(median = median(x), 
                                                         mean = mean(x),
                                                         sd = sd(x), 
                                                         Min = min(x),
                                                         Max = max(x),
                                                         se = sqrt(var(x)/length(x))))
thickness_summary_ulcer

year_summary_ulcer<- aggregate(melanomafor$year,
                                by = list(as.factor(melanomafor$ulcer)),
                                FUN = function(x) c(round(length(x)) ,
                                                    median = median(x), 
                                                    mean = mean(x),
                                                    sd = sd(x), 
                                                    Min = min(x),
                                                    Max = max(x),
                                                    se = sqrt(var(x)/length(x))))
year_summary_ulcer
#######################    summary per sex ####################################################


time_summary_sex<- aggregate(melanomafor$time,
                               by = list(as.factor(melanomafor$sex)),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x),
                                                   sd = sd(x), 
                                                   Min = min(x),
                                                   Max = max(x),
                                                   se = sqrt(var(x)/length(x))))
time_summary_sex

age_summary_sex<- aggregate(melanomafor$age,
                              by = list(as.factor(melanomafor$sex)),
                              FUN = function(x) c(median = median(x), 
                                                  mean = mean(x),
                                                  sd = sd(x), 
                                                  Min = min(x),
                                                  Max = max(x),
                                                  se = sqrt(var(x)/length(x))))
age_summary_sex


thickness_summary_sex<- aggregate(melanomafor$thickness,
                                    by = list(as.factor(melanomafor$sex)),
                                    FUN = function(x) c(median = median(x), 
                                                        mean = mean(x),
                                                        sd = sd(x), 
                                                        Min = min(x),
                                                        Max = max(x),
                                                        se = sqrt(var(x)/length(x))))
thickness_summary_sex

year_summary_sex<- aggregate(melanomafor$year,
                               by = list(as.factor(melanomafor$sex)),
                               FUN = function(x) c(round(length(x)) ,
                                                   median = median(x), 
                                                   mean = mean(x),
                                                   sd = sd(x), 
                                                   Min = min(x),
                                                   Max = max(x),
                                                   se = sqrt(var(x)/length(x))))
year_summary_sex

####################################################################################

summary_age <- melanomafor %>%
  summarise(
    Mean = mean(age),
    Median = median(age),
    SD = sd(age),
    se = sd(age)/sqrt(length(age)),
    Min = min(age),
    Max = max(age),
    Q1 = quantile(age, 0.25),
    Q3 = quantile(age, 0.75)
  )

summary_time <- melanomafor %>%
  summarise(
    Mean = mean(time),
    Median = median(time),
    SD = sd(time),
    se = sqrt(var(time)/length(time)),
    Min = min(time),
    Max = max(time),
    Q1 = quantile(time, 0.25),
    Q3 = quantile(time, 0.75)
  )

hist(melanomafor$age)
length(time)

summary_thickness <- melanomafor %>%
  summarise(
    Mean = mean(thickness),
    Median = median(thickness),
    SD = sd(thickness),
    se = sqrt(var(thickness)/length(thickness)),
    Min = min(thickness),
    Max = max(thickness),
    Q1 = quantile(thickness, 0.25),
    Q3 = quantile(thickness, 0.75)
  )

summary_year <- melanomafor %>%
  summarise(
    Mean = mean(year),
    Median = median(year),
    SD = sd(year),
    se = sqrt(var(year)/length(year)),
    Min = min(year),
    Max = max(year),
    Q1 = quantile(year, 0.25),
    Q3 = quantile(year, 0.75)
  )

overall_summary <- rbind(summary_age, summary_time,
                 summary_thickness,summary_year)

overall_summary <- as.data.frame(overall_summary)

write.csv(overall_summary, "C:\\Users\\HP\\Desktop\\overall_summary.csv")
summary_sex <- melanomafor %>%
  group_by(as.factor(sex)) %>%
  summarise(
    Count = n())

summary_ulcer <- melanomafor %>%
  group_by(as.factor(ulcer)) %>%
  summarise(
    Count = n())

summary_status<- melanomafor %>%
  group_by(status) %>%
  summarise(
    Count = n())

hist(age)
hist(time)
hist(thickness)
hist(year)


ggplot(melanomafor, aes(x = as.factor(sex))) +
  geom_bar() +
  labs(title = "Distribution of Sex",
       x = "Sex",
       y = "Count")

ggplot(melanomafor, aes(x = as.factor(ulcer))) +
  geom_bar() +
  labs(title = "Distribution of Ulcer",
       x = "Ulcer",
       y = "Count")

ggplot(melanomafor, aes(x = as.factor(status))) +
  geom_bar() +
  labs(title = "Distribution of Status",
       x = "Status",
       y = "Count")
###################################################################################
Monthly_boxplot_Shannon_H <- C_diversity %>% 
  select(Month, Shannon_H) %>%
  ggplot(aes(x=Month, y = Shannon_H, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Shannon_H))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Shannon_H Diversity index")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4),    
                       labels = c( "January", "February", "March", "April"))
Monthly_boxplot_Shannon_H



#####  regression analysis and appropriate correlation computations


ggplot(melanomafor, aes(x = age, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Scatter Plot (time ~ age)",
       x = "Age",
       y = "Time")+ theme_bw()

cor.test(time, age, method = "pearson") # Negative correlation

time_age_model <- lm(time ~ age, data = melanomafor)
# Summary of the regression model
summary(time_age_model)

qqnorm(melanomafor$thickness)
qqline(melanomafor$thickness)   ## thickness variable is not normally distributed

qqnorm(melanomafor$time)
qqline(melanomafor$time)    ### Normally distributed time residuals

qqnorm(melanomafor$age)
qqline(melanomafor$age)   #### Normally distributed   age residuals


t.test(age~sex, data = melanomafor)
t.test(time~sex, data = melanomafor)
wilcox.test(thickness~sex, data = melanomafor)


ggplot(melanomafor, aes(x = thickness, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(title = "Scatter Plot (time ~ thickness)",
       x = "Thickness",
       y = "Time")+ theme_bw()

cor.test(thickness, time, method = "spearman") 
time_thickness_model <- lm(time ~ thickness, data = melanomafor)
# Summary of the regression model
summary(time_thickness_model)


ggplot(melanomafor, aes(x = age, y = thickness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Scatter Plot (thickness ~ age)",
       x = "age",
       y = "Thickness")+ theme_bw()

cor.test(age, thickness, method = "spearman") 
age_thickness_model <- lm(thickness ~ age, data = melanomafor)
# Summary of the regression model
summary(age_thickness_model)


age.summary <-melanomafor %>%
  group_by(sex)%>%
  summarise(
    mean_age = mean(age),
    median_age = median(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )
  
qqnorm(diversity$Taxa_S)
qqline(diversity$Taxa_S)

Thickness.summary <-melanomafor %>%
  group_by(sex)%>%
  summarise(
    mean_thickness = mean(thickness),
    median_Thickness = median(thickness),
    min_Thickness = min(thickness),
    max_Thickness = max(thickness),
    sd_Thickness = sd(thickness)
  )

Time.summary <-melanomafor %>%
  group_by(sex)%>%
  summarise(
    mean_Time = mean(time),
    median_Time = median(time),
    min_Time = min(time),
    max_Time = max(time),
    sd_Time = sd(time)
  )

#####       QQ plots -------------------------------------------------------------
age_sex_0_female <- melanomafor %>% 
  select(age, sex) %>%
  filter(sex == "Female") 
qqnorm(age_sex_0_female$age,  main = "Female-age")
qqline(age_sex_0_female$age)    #### Normally distributed residuals

age_sex_1_male <- melanomafor %>% 
  select(age, sex) %>%
  filter(sex == "Male")
qqnorm(age_sex_1_male$age,  main = "Male-age")
qqline(age_sex_1_male$age)    #### Normally distributed residuals

Time_sex_0_female <- melanomafor %>% 
  select(time, sex) %>%
  filter(sex == "Female") 
qqnorm(Time_sex_0_female$time,  main = "Female-time")
qqline(Time_sex_0_female$time)  #### Normally distributed residuals

Time_sex_1_male <- melanomafor %>% 
  select(time, sex) %>%
  filter(sex == "Male") 
qqnorm(Time_sex_1_male$time,  main = "Male-time")
qqline(Time_sex_1_male$time)  #### Normally distributed residuals

Thickness_sex_0_female <- melanomafor %>% 
  select(thickness, sex) %>%
  filter(sex == "Female") 
qqnorm(Thickness_sex_0_female$thickness,  main = "Female-thickness")
qqline(Thickness_sex_0_female$thickness)  #### The residuals are NOT normally distributed



Thickness_sex_1_male <- melanomafor %>% 
  select(thickness, sex) %>%
  filter(sex == "Male") 
qqnorm(Thickness_sex_1_male$thickness,  main = "Male-thickness")
qqline(Thickness_sex_1_male$thickness)  #### The residuals are NOT normally distributed



ggplot(melanomafor, aes(x = thickness, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(title = "Scatter Plot (time ~ thickness)",
       x = "Thickness",
       y = "time")+ theme_bw()
  


ulcer_summary_sex<- melanomafor %>%
  select(sex, ulcer) %>% 
  group_by(as.factor(sex), ulcer) %>%
  summarise(Count = n())
