#####  -------------------indices ~ Day 

library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)

digg <- read.csv("C:\\Users\\HP\\Desktop\\DIE MODELLING\\Digg.csv")
digg <- read_csv("Data/Digg.csv")
view(digg)
attach(digg)

fdigg <- digg %>% 
  filter(`Simpson_1-D` >= 0.0001)
attach(fdigg)

ind_glmm_day <- glmer(Individuals ~ Day + (1 | Pitfall),
                      data = digg, family = poisson(link = "log"))

summary(ind_glmm_day)

even_glmm_day <- glmer(Evenness_e.H.S ~ Day +   (1|Pitfall),
                    data = digg, family = Gamma(link = "log"))
summary(even_glmm_day)

domi_glmm_day <- glmer(Dominance_D ~ Day +   (1|Pitfall),
                       data = digg, family = Gamma(link = "log"))
summary(domi_glmm_day)

simp_glmm_day <- glmer(Simpson_1.D ~ Day +   (1|Pitfall),
                    data = fdigg, family = Gamma(link = "log"))
summary(simp_glmm_day)

shan_glmm_day <- glmer(Shannon_H ~ Day +   (1|Pitfall),
                       data = fdigg, family = Gamma(link = "log"))
summary(shan_glmm_day)

################################################################################

abun_glmm_per <- glmer(Individuals ~ Period +  (1 | Pitfall),
                   data = digg, family = poisson(link = "log"))

summary(abun_glmm_per)

even_glmm_per <- glmer(`Evenness_e^H/S` ~ Period +   (1|Pitfall),
                    data = digg, family = Gamma(link = "log"))
summary(even_glmm_per)

domi_glmm_per <- glmer(Dominance_D ~ Period +   (1|Pitfall),
                       data = digg, family = Gamma(link = "log"))
summary(domi_glmm_per)

simp_glmm_per <- glmer(`Simpson_1-D` ~ Period +   (1|Pitfall),
                       data = fdigg, family = Gamma(link = "log"))
summary(simp_glmm_per)

shan_glmm_per <- glmer(Shannon_H ~ Period +   (1|Pitfall),
                       data = fdigg, family = Gamma(link = "log"))
summary(shan_glmm_per)

##################################################################################  

c_digg <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Projects\\Data\\Digging_in.csv")

c_digg <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\Digging_in.csv")
view(c_digg)
attach(c_digg)

hist(Individuals)
abun_glmm_per <- glm(Individuals ~ Period ,
                       data = c_digg,  family = poisson(link = "log"))
summary(abun_glmm_per)

even_glmm_per <- glm(Evenness_e.H.S~ Period ,
                       data = c_digg, family = Gamma(link = "log"))
summary(even_glmm_per)

domi_glmm_per <- glm(Dominance_D ~ Period,
                       data = c_digg, family = Gamma(link = "log"))
summary(domi_glmm_per)

simp_glmm_per <- glm(Simpson_1.D~ Period,
                       data = c_digg, family = Gamma(link = "log"))
summary(simp_glmm_per)

shan_glmm_per <- glm(Shannon_H ~ Period,
                       data = c_digg, family = Gamma(link = "log"))
summary(shan_glmm_per)


c_digg$Period <-factor(c_digg$Period, levels =  c("S2", "S3", "S1")) ## careful with releveling
c_digg$Period <-factor(c_digg$Period, levels =  c("S1", "S2", "S3", ))