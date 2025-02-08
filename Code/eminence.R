require(tidyverse)
require(agricolae)
require(readxl)

survive <-read_excel("C:\\Users\\DELL\\Desktop\\Eminence.xlsx", sheet = "survival")
survive <-read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Eminence.xlsx")

view(survive)

My_colour_choice <- c("#4daf4a", "#377eb8","#ff7f00","purple","#e31a1c","yellow")


  
survive %>% 
  filter(Clay == "Ubiaja") %>% 
  select(c(2:3, 12:19)) %>% 
  group_by(Treatments) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatments,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatments= factor(Treatments, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatments, fill = Treatments)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +  # alpha controls the transparency of the shading
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  labs(title = "Ubiaja",
       y= "No. of Survivors")+
  scale_x_continuous(breaks = 0:7) +
  theme_classic()


survive %>% 
  filter(Clay == "Aforma") %>% 
  select(c(2:3, 12:19)) %>% 
  group_by(Treatments) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatments,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatments= factor(Treatments, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatments, fill = Treatments)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +   
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  labs(title = "Aforma",
       y= "No. of Survivors")+
  scale_x_continuous(breaks = 0:7) +
  theme_classic()


s_Ubiaja <- survive %>% 
  select(Clay, Treatments, D7) %>% 
  as.data.frame() %>% 
  mutate(Treatments= factor(Treatments, 
  levels = c("Control","0.025g/ml", "0.05g/ml",
             "0.1g/ml" ))) %>% 
  filter(Clay=="Ubiaja") %>% 
  as.data.frame()

aov_Ubiaja <- aov(D7~Treatments, data = s_Ubiaja)
summary(aov_Ubiaja)
HSD.test(aov_Ubiaja, trt = c("Treatments"), group = TRUE)$groups

s.Aforma <- survive %>% 
  select(Clay, Treatments, D7) %>% 
  as.data.frame() %>% 
  mutate(Treatments= factor(Treatments, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  filter(Clay=="Aforma") %>% 
  as.data.frame()


# TWO-way anova
s.data <- survive %>% 
  select(Clay, Treatments, D7) %>% 
  as.data.frame() %>% 
  mutate(Treatments= factor(Treatments, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  as.data.frame()

aov.s <- aov(D7~Clay*Treatments, data = s.data)
summary(aov.s)
HSD.test(aov.s, trt = c("Clay","Treatments"), group = TRUE)$groups

# Se
s.data %>% 
  filter(Clay=="Ubiaja") %>% 
  group_by(Treatments) %>% 
  summarise(mean= mean(D7),
    S.E= sd(D7)/(sqrt(length(D7))))
s.data %>% 
  filter(Clay=="Aforma") %>% 
  group_by(Treatments) %>% 
  summarise(mean= mean(D7),
            S.E= sd(D7)/(sqrt(length(D7))))
 
# 

survive %>% 
  select(c(1:2, 19)) %>% 
  mutate(Treatments= factor(Treatments, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Clay, y = D7, fill = Treatments)) +
  stat_summary(
    geom = "bar", 
    fun = mean, 
    width = 0.7, 
    position = position_dodge(0.75)
  ) +
  stat_summary(
    geom = "errorbar", 
    fun.data = mean_se, 
    linewidth = 1, 
    width = 0.4, 
    position = position_dodge(0.7),
    color = "black"
  ) +
  scale_fill_manual(values = My_colour_choice)+
  labs(x="Clay type",
       y= "No. of survivors")+
  theme_classic()


##################################################################################
###################################################################################
neg_geo <-read_excel("C:\\Users\\DELL\\Desktop\\Eminence.xlsx", sheet = "neg_geotasis(2)")
view(neg_geo)

neg_geo <- neg_geo %>%
  mutate(Treatment= factor(Treatment, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  rename(Geotaxis="Neg_Geo(%)") %>% 
  as.data.frame()
  

ggplot(aes(x = Clay, y = Geotaxis, fill = Treatment), data= neg_geo) +
  stat_summary(
    geom = "bar", 
    fun = mean, 
    width = 0.7, 
    position = position_dodge(0.75)
  ) +
  stat_summary(
    geom = "errorbar", 
    fun.data = mean_se, 
    linewidth = 1, 
    width = 0.4, 
    position = position_dodge(0.7),
    color = "black"
  ) +
  scale_fill_manual(values = My_colour_choice)+
  labs(x="Clay type",
       y= "Negative Geotaxism (%)")+
  theme_classic()

ng.Aforma <- neg_geo %>% 
  filter(Clay=="Aforma") %>% 
  as.data.frame()
aov.aforma <- aov(Geotaxis~Treatment, data = ng.Aforma)
summary(aov.aforma)
HSD.test(aov.aforma, trt = c("Treatment"), group = TRUE)$groups

ng.Ubiaja <- neg_geo %>% 
  filter(Clay=="Ubiaja") %>% 
  as.data.frame()
aov.Ubiaja <- aov(Geotaxis~Treatment, data = ng.Ubiaja)
summary(aov.Ubiaja)
HSD.test(aov.Ubiaja, trt = c("Treatment"), group = TRUE)$groups

ng.Aforma %>% 
  group_by(Clay, Treatment) %>% 
  summarise(mean= mean(Geotaxis),
                 SE= sd(Geotaxis)/sqrt(length(Geotaxis)))
ng.Ubiaja %>% 
  group_by(Clay, Treatment) %>% 
  summarise(mean= mean(Geotaxis),
            SE= sd(Geotaxis)/sqrt(length(Geotaxis)))

#################################################################################
#################################################################################
toxicity <-read_excel("C:\\Users\\DELL\\Desktop\\Eminence.xlsx", sheet = "toxicity_test")
view(toxicity)

toxicity %>% 
  filter(Clay == "Ubiaja") %>% 
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatment,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatment= factor(Treatment, 
                            levels = c("0.1g/ml", "0.5g/ml" ,"1g/ml"))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +   
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  scale_x_continuous(breaks = 0:10) +
  geom_vline(xintercept= 7, linetype = 2)+
  labs(x= "Days", 
       y= "No. of Deaths",
       title = "Ubiaja")+
  theme_light()



toxicity %>% 
  filter(Clay == "Aforma") %>% 
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatment,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatment= factor(Treatment, 
                           levels = c("0.1g/ml", "0.5g/ml" ,"1g/ml"))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +   
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  scale_x_continuous(breaks = 0:10) +
  geom_vline(xintercept= 8, linetype = 2)+
  labs(x= "Days", 
       y= "No. of Deaths",
       title = "Aforma")+
  theme_light()


####################################################################################
suv.2 <- read_excel("C:\\Users\\DELL\\Desktop\\Eminence.xlsx", sheet = "survival2")
view(suv.2)


suv.2 %>% 
  filter(Clay == "Ubiaja") %>% 
  group_by(Treatments) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatments,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatments= factor(Treatments, 
                           levels = c("Control", "0.025g/ml", "0.05g/ml","0.1g/ml"))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatments, fill = Treatments)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +   
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  scale_x_continuous(breaks = 0:14) +
  geom_vline(xintercept= 7, linetype = 2)+
  labs(x= "Days", 
       y= "No. of Deaths",
       title = "Ubiaja")+
  theme_light()



suv.2 %>% 
  filter(Clay == "Aforma") %>% 
  group_by(Treatments) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatments,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatments= factor(Treatments, 
                            levels = c("Control", "0.025g/ml", "0.05g/ml","0.1g/ml"))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatments, fill = Treatments)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +   
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  scale_x_continuous(breaks = 0:14) +
 # geom_vline(xintercept= 7, linetype = 2)+
  labs(x= "Days", 
       y= "No. of Deaths",
       title = "Aforma")+
  theme_light()

