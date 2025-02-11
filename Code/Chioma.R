require(tidyverse)
require(agricolae)
require(readxl)


aforma <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Chioma EMT.xlsx",
                     sheet = "Afowa")
view(aforma)

My_colour <- c("black", "#4daf4a", "#377eb8","#ff7f00","purple","#e31a1c","yellow")
aforma.1 <- aforma %>% 
  rename(Prot="Prot g/dL",
         SOD="SOD U/g Prot",
         CAT="CAT U/g Prot",
         Gpx = "GPx U/g Prot",
         MDA = "MDA mol/g Prot",
         GSH ="GSH ug/mL",
         GST_Activity= "GST Activity umol/min/g Prot",
         H2O2="H2O2 ug/mL",
         Nitric_Oxide="Nitric Oxide ug/mL") %>% 
  mutate(SAMPLE= factor(SAMPLE,
                        levels =c("Control",
                        "0.025 g/ml",
                        "0.05 g/ml",
                        "0.1 g/ml"))) %>% 
  as.data.frame()
  
attach(aforma.1)
aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= Prot, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = " ",
       x= "Treatments",
       y= "Prot g/dL")+
  guides(fill="none")+
  theme_classic()

prot.aov <- aov(Prot~SAMPLE, data = aforma.1)
prot.lm<-lm(Prot~SAMPLE, data = aforma.1)
summary(prot.lm)
 
summary(prot.aov)
prot<-agricolae::HSD.test(prot.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)
prot$groups

# SOD
aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= SOD, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "SOD",
       x= "Treatments",
       y= "SOD U/g Prot")+
  guides(fill="none")+
  theme_classic()

SOD.aov <- aov(SOD~SAMPLE, data = aforma.1)
summary(SOD.aov)
agricolae::HSD.test(SOD.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)$groups

# CAT U/g Prot
aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= CAT, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "CAT",
       x= "Treatments",
       y= "CAT U/g Prot")+
  guides(fill="none")+
  theme_classic()

CAT.aov <- aov(CAT~SAMPLE, data = aforma.1)
summary(CAT.aov)
agricolae::HSD.test(CAT.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)$groups


# "GPx U/g Prot" 

aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= Gpx, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "Gpx",
       x= "Treatments",
       y= "GPx U/g Prot" )+
  guides(fill="none")+
  theme_classic()

Gpx.aov <- aov(Gpx~SAMPLE, data = aforma.1)
summary(Gpx.aov)
agricolae::HSD.test(Gpx.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)$groups


# "MDA mol/g Prot"
aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= MDA, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "MDA",
       x= "Treatments",
       y= "MDA mol/g Prot" )+
  guides(fill="none")+
  theme_classic()

MDA.aov <- aov(MDA~SAMPLE, data = aforma.1)
summary(MDA.aov)
agricolae::HSD.test(MDA.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)$groups

# "GSH ug/mL"  
aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= GSH, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "GSH",
       x= "Treatments",
       y=  "GSH ug/mL")+
  guides(fill="none")+
  theme_classic()

GSH.aov <- aov(GSH~SAMPLE, data = aforma.1)
summary(GSH.aov)
agricolae::HSD.test(GSH.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)$groups


# GST_Activity

aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= GST_Activity, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "GST_Activity",
       x= "Treatments",
       y=  "GST Activity umol/min/g Prot")+
  guides(fill="none")+
  theme_classic()

GST_Activity.aov <- aov(GST_Activity~SAMPLE, data = aforma.1)
summary(GST_Activity.aov)
agricolae::HSD.test(GST_Activity.aov, trt = c("SAMPLE"),
                    alpha = 0.5, group = TRUE)$groups

#   "H2O2 ug/mL"
aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= H2O2, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "H2O2",
       x= "Treatments",
       y=  "H2O2 ug/mL")+
  guides(fill="none")+
  theme_classic()

H2O2.aov <- aov(H2O2~SAMPLE, data = aforma.1)
summary(H2O2.aov)
agricolae::HSD.test(H2O2.aov, trt = c("SAMPLE"),
                    alpha = 0.3, group = TRUE)$groups


# "Nitric Oxide ug/mL" 

aforma.1 %>% 
  ggplot(aes(x= SAMPLE, y= Nitric_Oxide, fill = SAMPLE))+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.4, color= "black")+
  stat_summary(geom = "bar",fun = mean, width = 0.7)+
  scale_fill_manual(values = My_colour)+
  labs(title = "Nitric_Oxide",
       x= "Treatments",
       y=  "Nitric Oxide ug/mL" )+
  guides(fill="none")+
  theme_classic()

Nitric_Oxide.aov <- aov(Nitric_Oxide~SAMPLE, data = aforma.1)
summary(Nitric_Oxide.aov)
agricolae::HSD.test(Nitric_Oxide.aov, trt = c("SAMPLE"),
                    alpha = 0.5, group = TRUE)$groups

# Summary Statistics

aforma.1 %>% 
  pivot_longer(cols = -1,
               names_to = "activity",
               values_to = "Values"
  ) %>% 
  group_by(activity, SAMPLE) %>%
  summarise(mean= mean(Values),
            S.E.M= sd(Values)/sqrt(length(Values))) %>% 
  as.data.frame()
