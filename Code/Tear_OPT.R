library(tidyverse)
library(readxl)
library(agricolae)


Tear <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Tear_Data.xlsx",
                   sheet = 'working')

view(Tear)

Tear %>% 
  mutate(Period = factor(Period, levels = c("Before", "During", "After"))) %>% 
  filter(SEX== "M") %>% # Male
  ggplot(aes(x= Period, y = Tear_vol))+
  stat_summary(geom = "bar",fun = mean, fill = "blue") +
  
  stat_summary(geom = "errorbar", fun.data = function(x) {
    data.frame(y = mean(x), 
               ymin = mean(x) - sd(x),
               ymax = mean(x) + sd(x))
  },  position = position_dodge(width = 0.5),
  width = 0.2, colour = "black", size = 0.9) +
  labs(y = "Tear Volume (mm)")+
  theme_classic()

Tear %>% 
  mutate(Period = factor(Period, levels = c("Before", "During", "After"))) %>% 
  filter(SEX== "F") %>% # Female
  ggplot(aes(x= Period, y = Tear_vol))+
  stat_summary(geom = "bar",fun = mean, fill = "red") +
  
  stat_summary(geom = "errorbar", fun.data = function(x) {
    data.frame(y = mean(x), 
               ymin = mean(x) - sd(x),
               ymax = mean(x) + sd(x))
  },  position = position_dodge(width = 0.5),
  width = 0.2, colour = "black", size = 0.9) +
  labs(y = "Tear Volume (mm)")+
  theme_classic()


Tear1 <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Tear_Data.xlsx",
                   sheet = 'Sheet1') %>% 
  rename(Before = "TEAR VOLUME(BEFORE)",
         During = "TEAR VOLUME(DURING)",
         After  = "TEAR VOLUME(AFTER)")


view(Tear1)

t.test(Before~Participant, data = Tear1)
t.test(During~Participant, data =Tear1)
t.test(After~Participant, data= Tear1)


TearL <- Tear1 %>%
  select(-NAME) %>% 
  pivot_longer(
    cols = -c(1:3),
    names_to =  'Period',
    values_to = 'Tear_vol'
  )


TearL %>% 
  mutate(Period = factor(Period, levels = c("Before", "During", "After"))) %>% 
  filter(Participant== "Athlete") %>% 
ggplot(aes(x= Period, y = Tear_vol))+
  stat_summary(geom = "bar",fun = mean, fill = "green") +
  
  stat_summary(geom = "errorbar", fun.data = function(x) {
    data.frame(y = mean(x), 
               ymin = mean(x) - sd(x),
               ymax = mean(x) + sd(x))
  },  position = position_dodge(width = 0.5),
  width = 0.2, colour = "black", size = 0.9) +
  labs(y = "Tear Volume (mm)", title = "Athlete")+
  theme_classic()


TearL %>% 
  mutate(Period = factor(Period, levels = c("Before", "During", "After"))) %>% 
  filter(Participant== "Non-Athlete") %>% 
  ggplot(aes(x= Period, y = Tear_vol))+
  stat_summary(geom = "bar",fun = mean, fill = "orange") +
  
  stat_summary(geom = "errorbar", fun.data = function(x) {
    data.frame(y = mean(x), 
               ymin = mean(x) - sd(x),
               ymax = mean(x) + sd(x))
  },  position = position_dodge(width = 0.5),
  width = 0.2, colour = "black", size = 0.9) +
  labs(y = "Tear Volume (mm)", title = "Non-athlete")+
  theme_classic()


ano_A <- aov(Tear_vol~Period, data =  TearL %>% 
               filter(Participant=="Athlete"))
summary(ano_A)

hsd_a <- agricolae::HSD.test(y = ano_A, trt = "Period",
                    alpha = 0.05, group = TRUE, 
                    unbalanced=TRUE) 
hsd_a$means
hsd_a$groups



ano_nA <- aov(Tear_vol~Period, data =  TearL %>% 
               filter(Participant=="Non-Athlete"))
summary(ano_nA)

hsd_na <- agricolae::HSD.test(y = ano_nA, trt = "Period",
                             alpha = 0.05, group = TRUE, 
                             unbalanced=TRUE) 
hsd_na$means
hsd_na$groups


write.csv(TearL, "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\TearL.csv")
