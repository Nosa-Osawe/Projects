library(tidyverse)
library(readxl)
library(agricolae)

ebube <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Ebube data.xlsx",
                    sheet = "Worksheet B")
view(ebube)
ebube<-as.data.frame(ebube)
attach(ebube)
glimpse(ebube)

lm1<- lm(Population~Days+Conc, data = ebube)

summary(lm1)

ebube_conc_As_Fac <- ebube
ebube_conc_As_Fac$Conc <- factor(ebube_conc_As_Fac$Conc)

levels(ebube_conc_As_Fac$Conc)# just to confirm the levels

lm2 <- aov(Population~ Conc, data = ebube_conc_As_Fac)
summary(lm2)

TukeyHSD(lm2)

HSD.test(lm2, 
         trt = c("Conc"), 
         group = TRUE)$groups


ebu_summary <- ebube_conc_As_Fac %>% 
  select(-Group) %>% 
  group_by(Days, Conc) %>%
  summarise(mean_pop = mean(Population)) %>% 
  arrange(Conc) %>% 
  filter(Conc != "0") %>% 
  pivot_wider(names_from = Conc, 
              values_from = mean_pop) %>% 
  as.data.frame()

write.csv(ebu_summary, file = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Ebube_summ.csv")


