library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)

digg <- read.csv("C:\\Users\\HP\\Desktop\\DIE MODELLING\\Digg.csv")
digg <- read_csv("Data/Digg.csv")
view(digg)

mean(digg$Individuals)
var(digg$Individuals)  ##### The data is clearly over dispersed

hist(digg$Individuals)
attach(digg)

ind_pred <- glm(Individuals ~ Day +  Period,
                data = digg,
                family = quasipoisson(link = "log"))
summary(ind_pred)

coef(ind_pred)

ind_pred2 <- glm.nb(Individuals ~ Day +  Period,
                    data = digg,
                    link = log)
summary(ind_pred2)

ind_pred3 <- glm.nb(Individuals ~ Day ,
                    data = digg,
                    link = log)
summary(ind_pred3)


digg %>%
  filter(Period == "S1") %>%
  summarise(mean = mean(Individuals),
            median  = median(Individuals)) 

########################################################################################3

ind_pred3 <- lmer(Shannon_H ~ Day +  (1|Pitfall),
                    data = digg
                    )
summary(ind_pred3)

ind_pred4 <- lmer(Shannon_H ~ Day + Period + ( 1 | Pitfall),
                  data = digg)
summary(ind_pred4)