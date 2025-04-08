require(tidyverse)
require(agricolae)
require(effectsize)
library(vegan)

melanoma <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\melanomafor_stat1.csv") %>% 
  mutate(status = factor(status, levels = c("1","2","3"), 
                         labels = c("Poor", "Middle-class", "Rich")),
         ulcer = factor(ulcer, levels = c("0","1"),
                        labels = c("Negative","Positive"))) %>% 
  as.data.frame()

aov1<- aov(thickness~status, data = melanoma)
summary(aov1)


lm1 <- lm(thickness~ status*age, data = melanoma)
summary(lm1)
eta_squared(lm1, partial = TRUE)
omega_squared(lm1)


