#library(lme4)
#library(lmerTest)
library(tidyverse)
library(MASS)
#library(ggfortify)
library(corrplot)  # used to load a corr matrix!!!!
library(multcomp)
library(emmeans)
#library(car)
# in addition: 
require(performance)
library(effectsize)  # (partial) variane expdlained



length(colnames(omo_Data))  
omo<- omo_Data[,1:24]    

omo <- omo_Data %>% 
  select(1:24)  
view(omo)

length(omo)


omo_clean <- omo %>% 
  dplyr::select(-c(1:3), -Anopheles, -Aedes, -Cules) %>% 
  as.data.frame()

omo_cor <- cor(omo_clean)


# make the correlation matrix
corrplot(omo_cor, method = c("color"),  
         addCoef.col='black', 
         number.cex = 0.7,  tl.cex = 0.8, tl.col = 'black', 
         type= "upper")


lm1 <- lm(Total.Solid~Suspended.Solid, data = omo_clean)    
summary(lm1)       

lm1.2 <- lm(Total.Solid~DO , data = omo_clean) 
summary(lm1.2)

lm2 <- lm(Total.Solid~Suspended.Solid+ DO , data = omo_clean)  
summary(lm2)

lm3 <- lm(Total.Solid~Depth , data = omo_clean) 
summary(lm3)
         
model_performance(lm1)
model_performance(lm1.2)


check_normality(lm1)
check_model(lm1)


check_model(lm3)

plot(Total.Solid~Depth)

#######################################################################################
attach(omo_clean)

lmp1 <- lm(Phosphate~Colour, data = omo_clean)
summary(lmp1)
check_model(lmp1)

# Favour foward selection
lmp1 <- lm(Phosphate~Colour, data = omo_clean)
summary(lmp1)
check_model(lmp1)


lmp2 <- lm(Phosphate~ Colour+Nitrate, data = omo_clean)
summary(lmp2)
check_model(lmp2)

model_performance(lmp1)
model_performance(lmp2)

anova(lmp1, lmp2)


# we go with the simpler model: lmp1

lmp3 <- lm(Phosphate~ Colour * Chloride, data = omo_clean)
summary(lmp3)
check_model(lmp3)

eta_squared(lmp3)

omega_squared(lmp3)




