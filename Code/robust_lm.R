library(tidyverse)
library(readxl)

PhD.pah <- read_xlsx("C:\\Users\\DELL\\Desktop\\PhD_PAH.xlsx", sheet = 'Sheet1')

pah.phd <- PhD.pah[,c(1,4,21:26,17,19)]
head(pah.phd)

 
pah.phd$Season <- factor(pah.phd$Season)
pah.phd %>% 
  select(LGA, BaP, DBahA) %>% 
  group_by(LGA) %>% 
  summarise(means_Bap = mean(BaP),
            mean_DBahA = mean(DBahA))

LGA1 = pah.phd %>% 
  filter(LGA==1)

# write.csv(LGA1, file = "C:\\Users\\DELL\\Desktop\\LGA1.csv")
LGA1.edit <- read.csv("C:\\Users\\DELL\\Desktop\\LGA1.csv")
view(LGA1.edit)
LGA1.edit$Season <- factor(LGA1.edit$Season)

lga1_lmBaP <- lm(BaP~., data =LGA1.edit )
summary(lga1_lmBaP)

lga1_lmDBahA <- lm(DBahA~., data =LGA1.edit )
summary(lga1_lmDBahA)
#-------------------------------------------------------
install.packages("robustbase")
require(robustbase)

lga1_BaP_robust <- lmrob(BaP~.-LGA-Bap_pred-DBahA_pred-DBahA, data =LGA1.edit )
summary(lga1_BaP_robust)

lga1_DBahA_robust <- lmrob(DBahA~.-LGA-DBahA_pred-BaP-Bap_pred, data =LGA1.edit )
summary(lga1_DBahA_robust)

#---------------------------------------------------------
library(Metrics)
lmPred1Bap <- as.numeric(predict(lga1_lmBaP, newdata = LGA1.edit))

mse(LGA1.edit$BaP, lmPred1Bap)
rmse(LGA1.edit$BaP, lmPred1Bap)
mae(LGA1.edit$BaP, lmPred1Bap)

R.squared.plot <- as.data.frame(cbind(LGA1.edit$BaP, lmPred1Bap))

lmpred1DBahA <-as.numeric(predict(lga1_lmDBahA, newdata = LGA1.edit))

mse(LGA1.edit$DBahA, lmpred1DBahA)
rmse(LGA1.edit$DBahA, lmpred1DBahA)
mae(LGA1.edit$DBahA, lmpred1DBahA)


R.squared.plot2 <- as.data.frame(cbind(LGA1.edit$DBahA, lmpred1DBahA))

#--------------------------------------------------------------------------------------

LGA2 = pah.phd %>% 
  filter(LGA==2)

 write.csv(LGA2, file = "C:\\Users\\DELL\\Desktop\\LGA2.csv")
LGA2.edit <- read.csv("C:\\Users\\DELL\\Desktop\\LGA2.csv")
 
#----------------------------------------------------------------------------------

LGA5 = pah.phd %>% 
  filter(LGA==5)

write.csv(LGA5, file = "C:\\Users\\DELL\\Desktop\\LGA5.csv")
LGA5.edit <- read.csv("C:\\Users\\DELL\\Desktop\\LGA5.csv")

#---------------------------------------------------------------------------------

LGA3 = pah.phd %>% 
  filter(LGA==3)

write.csv(LGA3, file = "C:\\Users\\DELL\\Desktop\\LGA3.csv")
LGA3.edit <- read.csv("C:\\Users\\DELL\\Desktop\\LGA3.csv")

#---------------------------------------------------------------------------------
LGA4 = pah.phd %>% 
  filter(LGA==4)

write.csv(LGA4, file = "C:\\Users\\DELL\\Desktop\\LGA4.csv")
LGA4.edit <- read.csv("C:\\Users\\DELL\\Desktop\\LGA4.csv")








