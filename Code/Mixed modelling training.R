library(tidyverse)
library(lme4)

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

install.packages("performance")
library(performance)
# Poisson GLMM Given the mean-variance relationship, we
# will most likely need a model with over-dispersion.  To
# understand why, let's start with a Poisson model.
mp1 <- glmer(total.fruits ~ nutrient * amd + rack + status +
               (1 | popu) + (1 | gen), data = dat.tf, family = "poisson")

summary(mp1)
model_performance(mp1) #Shows many performance metric
hist(resid(mp1), breaks = 50)

check_overdispersion(mp1) 
