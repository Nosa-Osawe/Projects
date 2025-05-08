library(tidyverse)
library(lme4)
library(performance)
library(bbmle)

dat.tf <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\banta_totalfruits.csv")


# Before we go any further, we need to select an error
# distribution. This choice will be informed by the
# structure of our data.  Our response variable is count
# data which suggests we need a Poisson distribution (i.e.
# the variance is equal to the mean).

hist(dat.tf$total.fruits, breaks = 50, col = "blue", main = "",
     xlab = "Total fruits", ylab = "Count")

# Let's explore the variance within our data Create new
# variables that represent every combination of variables
dat.tf <- within(dat.tf, {
  # genotype x nutrient x clipping
  gna <- interaction(gen, nutrient, amd)
  gna <- reorder(gna, total.fruits, mean)
  # population x nutrient x clipping
  pna <- interaction(popu, nutrient, amd)
  pna <- reorder(pna, total.fruits, mean)
})


# Boxplot of total fruits vs genotype x nutrient x clipping interaction
ggplot(data = dat.tf, aes(factor(x = gna), y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21,
               outlier.colour = "skyblue2") +
  ylab("log (Total fruits)\n") + # \n creates a space after the title
  xlab("\nGenotype x nutrient x clipping") + # space before the title
  theme_bw() + theme(axis.text.x = element_blank()) +
  stat_summary(fun = mean, geom = "point", colour = "red")

view(dat.tf)


# Poisson GLMM Given the mean-variance relationship, we
# will most likely need a model with over-dispersion.  To
# understand why, let's start with a Poisson model.
mp1 <- glmer(total.fruits ~ nutrient * amd + rack + status +
               (1 | popu) + (1 | gen), data = dat.tf, family = "poisson")

summary(mp1)
model_performance(mp1) #Shows many performance metric
hist(resid(mp1), breaks = 50)

check_overdispersion(mp1) 


# Negative binomial GLMM using the function glmer.nb()
mnb1 <- glmer.nb(total.fruits ~ nutrient * amd + rack + status +
                   (1 | popu) + (1 | gen),
                 data = dat.tf, 
                 control = glmerControl(optimizer = "bobyqa"))
# Control argument specifies the way we optimize the
# parameter values

check_overdispersion(mnb1)

# Account for over-dispersion
mpl1 <- glmer(total.fruits ~ nutrient * amd + rack + status +
                (1 | X) + (1 | popu) + (1 | gen), data = dat.tf, 
              family = "poisson",
              control = glmerControl(optimizer = "bobyqa"))

allFit(mpl1)
# popu only
mpl1.popu <- glmer(total.fruits ~ nutrient * amd + rack + status +
                     (1 | X) + (1 | popu), data = dat.tf, family = "poisson",
                   control = glmerControl(optimizer = "bobyqa"))

# gen only
mpl1.gen <- glmer(total.fruits ~ nutrient * amd + rack + status +
                    (1 | X) + (1 | gen), data = dat.tf, 
                  family = "poisson", 
                  control = glmerControl(optimizer = "bobyqa"))

# IC approach using AICc
ICtab(mpl1, mpl1.popu, mpl1.gen, type = c("AICc"))

library(coefplot2)

# Variance terms
coefplot2(mpl1, ptype = "vcov", 
          intercept = TRUE, 
          main = "Random effect variance")

# Fixed effects
coefplot2(mpl1, intercept = TRUE,
          main = "Fixed effect coefficient")

summary(mpl1)

data("sleepstudy")
view(sleepstudy)


#########################################################################################

library(MASS)

hist(lice$Menacanthis_straminus)
hist(lice$Menopon_galinae)

summary(lice$Menacanthis_straminus)
boxplot(lice$Menacanthis_straminus)

qqnorm(lice$Menacanthis_straminus)
qqline((lice$Menacanthis_straminus))

shapiro.test(lice$Menacanthis_straminus)


#        

attach(lice)
boxplot (Menacanthis_straminus ~ Location)

model.1 <- lm(Menacanthis_straminus ~ Location, data = lice)
summary(model.1)
# Multiple R-squared:  0.0513,	Adjusted R-squared:  0.03678 # AIC: 624.194

model_performance(model.1)


model.1.1 <- glm(Menacanthis_straminus ~ Location, data = lice, 
                 gaussian(link = "identity"))
model_performance(model.1.1)


check_overdispersion(model.1.1)
check_homogeneity(model.1)

model.2 <- glm(Menacanthis_straminus ~ Location, data = lice, 
               family =  poisson(link = "log"))
model_performance(model.2)


model.3 <- glm(Menacanthis_straminus ~ Location, data = lice, 
               family =  quasipoisson(link = "log"))
model_performance(model.3)



########################################################################################

require(tidyverse)
require(readxl)
library(emmeans)
library(performance)
#library(MASS)

hist(lice$Menopon_galinae)

qqnorm(Menopon_galinae)
qqline(Menopon_galinae)



model1 <- lm(Menopon_galinae~Location, data = lice)
summary(model1)
model_performance(model1)
check_model(model1)

model2 <- glm(Menopon_galinae~Location, data = lice,
              family = poisson(link = "log"))
model_performance(model2)

check_overdispersion(model2)


model3 <- glm(Menopon_galinae~Location, data = lice,
              family = quasipoisson(link = "log"))
model_performance(model3)
check_model(model3)

check_overdimodel3check_overdispersion(model3)


model4 <- glm.nb(Menopon_galinae~Location, data = lice)
check_model(model4)
check_overdispersion(model4)
model_performance(model4)
