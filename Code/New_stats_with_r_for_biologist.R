library(arm)
library(tidyverse)
library(lme4)

oneway <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\biomass_data.csv")

midm <- function(x) mean(x) # function to calculate means
p2 <- qplot(FL, Biomass.m2, data= oneway,
              xlab= "Light & Fertilisation treatments",
              ylab= "Aboveground biomass (g m–2)")+
  theme_bw()+
  geom_hline(yintercept=480, colour="red")

p2 + stat_summary( aes(colour= "Mean"), 
                   geom = "point",
                   fun.y= midm)
#### linear model: mass as a function of FL
mod1 <- lm(Biomass.m2~FL, data= oneway)
plot(mod1)

summary(mod1)
display(mod1)
confint(mod1)

mod3 <- lm(Biomass.m2~Light+Fert+Light:Fert, data= oneway)
display(mod3) 
summary(mod3)

mod4 <- lm(Biomass.m2~Light+Fert, data= oneway)
anova(mod3, mod4)   # compare models

par(mfrow= c(2,1))
with(oneway, interaction.plot(Light, Fert, Biomass.m2) )
with(oneway, interaction.plot(Fert, Light, Biomass.m2, xlab= "Fertilizer") )

#########
 #      Panel plots
case1402<- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\case1402.csv")
xlabel <- expression(paste("Ozone (", mu,"L L"^"-1",")" ))
ylabel <- expression(paste("Log Yield (kg ha"^"-1",") "))
qplot(O3, log(William), data= case1402, facets=.~Stress,
geom= c("point", "smooth"), method= "lm",
xlab= xlabel, ylab= ylabel, main= "Soya bean variety:
'William'") +theme_bw()

case1402$Stress<- factor(case1402$Stress, levels=c("Well-watered","Stressed"))
w1 <- lm(log(William)~O3*Stress, data= case1402)
summary(w1)
display(w1)
confint(w1)

summary( lm(log(William)~SO2*Stress, data= case1402 ) )
anova( lm(log(William)~SO2*Stress, data= case1402 ) )
anova( lm(log(William)~SO2+Stress, data= case1402 ) )

summary( lm(log(William)~O3*SO2*Stress, data= case1402 ) )
anova( lm(log(William)~O3*SO2*Stress, data= case1402 ) )


###########################################################3

pdigg <- digg
pdigg$Day <- as.numeric(pdigg$Day)
attach(pdigg)
digg_predict <- lmer(Shannon_H ~ Day + (1+Day|Pitfall), data = pdigg)

digg_predict <- lmer(Shannon_H ~ Day+ (1|Pitfall), data = digg)
summary(digg_predict)

C_diversity <- read_csv("Data/C_Ant_diversity_data.csv")
attach(C_diversity)

nosa_div <- lmer(Shannon_H ~ Period + (-1+ Period|Pitfall), data = C_diversity) 
summary(nosa_div)
anova(nosa_div)
