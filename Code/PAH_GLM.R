
library(tidyverse)

ppp <- read.csv("C:\\Users\\DELL\\Desktop\\ddddddd.csv")

view(ppp)
attach(ppp)
colnames(ppp)
#------------- 

PAH <- ppp %>% 
  select(c(1:9,"Benzo..a..pyrene","Dibenzo..a.h..anthracene")) %>% 
  rename("BaP"= "Benzo..a..pyrene",
         "DBahA"= "Dibenzo..a.h..anthracene",
         "PM2.5"= "Avg..PM2.5")
head(PAH)

PAH$Pressure <-as.numeric(PAH$Pressure)
unique(PAH$L.G.A)
PAH <- na.omit(PAH)
IKPOBA_OKHA <-PAH %>% 
  filter(L.G.A== "IKPOBA-OKHA")

lm_IKPOBA_OKHA <- lm(BaP ~ scale(Temperature)+
                       scale(Relative.Humidity)+
                       scale(Pressure)+
                       scale(Wind.Speed)+
                       scale(Dew.Point)+
                       scale(PM2.5)+
                       Season, data = IKPOBA_OKHA)
summary(lm_IKPOBA_OKHA)

lm2_IKPOBA_OKHA <- lm(DBahA ~ scale(Temperature)+
                       scale(Relative.Humidity)+
                       scale(Pressure)+
                       scale(Wind.Speed)+
                       scale(Dew.Point)+
                       scale(PM2.5)+
                       Season, data = IKPOBA_OKHA)
summary(lm2_IKPOBA_OKHA)



EGOR <-PAH %>% 
  filter(L.G.A== "EGOR")

lm_EGOR <- lm(BaP ~ scale(Temperature)+
                       scale(Relative.Humidity)+
                       scale(Pressure)+
                       scale(Wind.Speed)+
                       scale(Dew.Point)+
                       scale(PM2.5)+
                       Season, data = EGOR)
summary(lm_EGOR)

lm2_EGOR <- lm(DBahA ~ scale(Temperature)+
                scale(Relative.Humidity)+
                scale(Pressure)+
                scale(Wind.Speed)+
                scale(Dew.Point)+
                scale(PM2.5)+
                Season, data = EGOR)
summary(lm2_EGOR)


UHUNMWONDE <-PAH %>% 
  filter(L.G.A== "UHUNMWONDE")

lm_UHUNMWONDE <- lm(BaP ~ scale(Temperature)+
                scale(Relative.Humidity)+
                scale(Pressure)+
                scale(Wind.Speed)+
                scale(Dew.Point)+
                scale(PM2.5)+
                Season, data = UHUNMWONDE)
summary(lm_UHUNMWONDE)

lm2_UHUNMWONDE <- lm(DBahA ~ scale(Temperature)+
                      scale(Relative.Humidity)+
                      scale(Pressure)+
                      scale(Wind.Speed)+
                      scale(Dew.Point)+
                      scale(PM2.5)+
                      Season, data = UHUNMWONDE)
summary(lm2_UHUNMWONDE)

OVIA_NE <-PAH %>% 
  filter(L.G.A== "OVIA N.E.")

lm_OVIA_NE <- lm(BaP ~ scale(Temperature)+
                scale(Relative.Humidity)+
                scale(Pressure)+
                scale(Wind.Speed)+
                scale(Dew.Point)+
                scale(PM2.5)+
                Season, data = OVIA_NE)
summary(lm_OVIA_NE)

lm2_OVIA_NE <- lm(DBahA ~ scale(Temperature)+
                   scale(Relative.Humidity)+
                   scale(Pressure)+
                   scale(Wind.Speed)+
                   scale(Dew.Point)+
                   scale(PM2.5)+
                   Season, data = OVIA_NE)
summary(lm2_OVIA_NE)

OREDO<-PAH %>% 
  filter(L.G.A== "OREDO")

lm_OREDO <- lm(BaP ~ scale(Temperature)+
                   scale(Relative.Humidity)+
                   scale(Pressure)+
                   scale(Wind.Speed)+
                   scale(Dew.Point)+
                   scale(PM2.5)+
                   Season, data = OREDO)
summary(lm_OREDO)


lm2_OREDO <- lm(DBahA ~ scale(Temperature)+
                 scale(Relative.Humidity)+
                 scale(Pressure)+
                 scale(Wind.Speed)+
                 scale(Dew.Point)+
                 scale(PM2.5)+
                 Season, data = OREDO)
summary(lm2_OREDO)




