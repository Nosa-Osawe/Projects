library(tidyverse)
library(readxl)
library(agricolae)
library(emmeans)

iop <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\IOP.xlsx",
                  sheet = "Sheet1") %>% 
  pivot_longer(cols = -(1:2),
               names_to = "smoke_time",
               values_to = "IOP") %>% 
  mutate(smoke_time = factor(smoke_time, 
                             levels = c("0 min",
                                        "5 min",
                                        "30 min",
                                        "60 min",
                                        "90 min"))) %>% 
  as.data.frame()



iop %>% 
  filter(Eye == "Right") %>% 
  ggplot(aes(x = smoke_time, y = IOP, ))+
  stat_summary(geom = "bar", fun = mean, color = "red", fill = "orange", width = .75)+
  stat_summary(geom = "errorbar", fun.data = mean_se, linewidth = 1, width = .4 )+
  labs(x= "Smoke time", y = "IOP (mmHg)")+
  theme_light()


iop %>% 
  filter(Eye == "Left") %>% 
  ggplot(aes(x = smoke_time, y = IOP))+
  stat_summary(geom = "bar", fun = mean,  fill = "lightblue", color = "blue", width = .75)+
  stat_summary(geom = "errorbar", fun.data = mean_se, linewidth = 1, width = .4 )+
  #stat_summary(geom = "line", fun = mean, 
              # aes(group = 1), # manages the categorical X-axis
              # color = "red", linewidth = 1,linetype = "dashed" ) +
  labs(x= "Smoke time", y = "IOP (mmHg)")+
  theme_light()

 
 # Right eye
right.iop <- aov(IOP~smoke_time, data = iop %>% 
                   filter(Eye == "Right"))
hsd.right <- agricolae::HSD.test(right.iop, trt = c("smoke_time"), group = TRUE)

hsd.right$means


# left eye
left.iop <- aov(IOP~smoke_time, data = iop %>% 
                   filter(Eye == "Left"))
hsd.left <- agricolae::HSD.test(left.iop, trt = c("smoke_time"), group = TRUE)

hsd.left$means


lm1 <- lm(IOP~smoke_time+ Eye, data = iop)
summary(lm1)


lm1.right <- lm(IOP~smoke_time ,data = iop %>% 
                  filter(Eye == "Right"))
summary(lm1.right)

emm <- emmeans(lm1.right, ~ smoke_time)
pairs(emm,  adjust = "none")


# LEFT

lm1.left <- lm(IOP~smoke_time ,data = iop %>% 
                  filter(Eye == "Left"))
summary(lm1.left)

emm2 <- emmeans(lm1.left, ~ smoke_time)
pairs(emm2,  adjust = "none")

