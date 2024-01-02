library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)

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

############################################################################################

qqplot(digg$Shannon_H)
qqnorm(digg$Shannon_H)
hist(digg$Shannon_H)
shapiro.test(digg$Shannon_H)


ind_pred3 <- lmer(Shannon_H ~ Day +  (1|Pitfall),
                    data = digg)
summary(ind_pred3)



ind_glmm_day <- glmer(Individuals ~ Day + (1 | Pitfall),
                   data = digg, family = poisson(link = "log"))

summary(ind_glmm_day)

ind_pred5 <- glmer(Individuals ~ Day +  (1 | Pitfall) + (1|Period),
                   data = digg, family = poisson(link = "log"))

summary(ind_pred5)
anova(ind_glmm_day, ind_pred5)

digg$Period <- factor(digg$Period, levels = c("S1", "S2", "S3"))

ind_pred6 <- glmer(Individuals ~ Period +  (1 | Pitfall),
                   data = digg, family = poisson(link = "log"))

summary(ind_pred6)


shannon_pred1 <- lmer(Shannon_H ~ Day +  (1|Pitfall),
                  data = digg)
summary(shannon_pred1)

shannon_pred2 <- lmer(Shannon_H ~ Period +  (1|Pitfall),
                      data = digg)
summary(shannon_pred2)

Dom_pred1 <- lmer(Dominance_D ~ Day +  (1|Pitfall),
                      data = digg)
summary(Dom_pred1)

Dom_pred2 <- lmer(Dominance_D ~ Period +  (1|Pitfall),
                  data = digg)
summary(Dom_pred2)

even_pred1 <- lmer(`Evenness_e^H/S` ~ Day +  (1|Pitfall),
                  data = digg)
summary(even_pred1)


simp_pred2 <- lmer(`Simpson_1-D` ~ Day +   (1|Pitfall),
                   data = digg)
summary(simp_pred2)


simp_pred3 <- lmer(Individuals ~ Period +   (1|Pitfall),
                   data = digg)
summary(simp_pred3)



simp_pred4 <- lmer(Simpson_1.D ~ Period +   (1|Pitfall),
                   data = digg)
summary(simp_pred4)

simp_pred5 <- lmer(Simpson_1.D ~ Day +   (1|Pitfall) +  (1|Period),
                   data = digg)
summary(simp_pred5)

anova(simp_pred4, simp_pred5)

AIC(simp_pred3)
AIC(simp_pred4)
AIC(simp_pred5)

BIC(simp_pred3)
BIC(simp_pred4)
BIC(simp_pred5)    ## model with only day as fixed effect is best


