
library(dunn.test)
library(tidyverse)
library(patchwork)


digg2 <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Digging_in.csv")
View(digg2)

digg2$Day <- factor(digg2$Day, levels = c( "Days (2-4)",  "Days (6-8)", "Days (9-11)")) 

## Taxa richness
kruskal.test(digg2$Taxa_S ~ digg2$Day)  
dunn.test(digg2$Taxa_S, digg2$Day, method = "bonferroni")

Digg2_Taxa_S <- aggregate(digg2$Taxa_S,
                          by = list(digg2$Day),
                          FUN = function(x) c(median = median(x), 
                                              mean = mean(x), sd = sd(x)))
Digg2_Taxa_S

## Species abundance
anova_Digg2_Individuals <- aov(digg2$Individuals ~ digg2$Day, data = digg2)
summary(anova_Digg2_Individuals)
TukeyHSD(anova_Digg2_Individuals)

Digg2_Individuals <- aggregate(digg2$Individuals,
                               by = list(digg2$Day),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x), sd = sd(x)))
Digg2_Individuals

######     Simpson_1.D Diversity

anova_Digg2_Simpson_1.D <- aov(digg2$Simpson_1.D ~ digg2$Day, data = digg2)
summary(anova_Digg2_Simpson_1.D)
TukeyHSD(anova_Digg2_Simpson_1.D)

Digg2_Simpson_1.D <- aggregate(digg2$Simpson_1.D,
                               by = list(digg2$Day),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x), sd = sd(x)))
Digg2_Simpson_1.D



#####   
anova_Digg2_Shannon_H <- aov(digg2$Shannon_H ~ digg2$Day, data = digg2)
summary(anova_Digg2_Shannon_H)
TukeyHSD(anova_Digg2_Shannon_H)

Digg2_Shannon_H <- aggregate(digg2$Shannon_H,
                             by = list(digg2$Day),
                             FUN = function(x) c(median = median(x), 
                                                 mean = mean(x), sd = sd(x)))
Digg2_Shannon_H


##### Dominance
anova_Digg2_Dominance_D <- aov(digg2$Dominance_D ~ digg2$Day, data = digg2)
summary(anova_Digg2_Dominance_D)
TukeyHSD(anova_Digg2_Dominance_D)

Digg2_Dominance_D <- aggregate(digg2$Dominance_D,
                               by = list(digg2$Day),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x), sd = sd(x)))
Digg2_Dominance_D

###### Evenness
anova_Digg2_Evenness_e.H.S <- aov(digg2$Evenness_e.H.S ~ digg2$Day, data = digg2)
summary(anova_Digg2_Evenness_e.H.S)
TukeyHSD(anova_Digg2_Evenness_e.H.S)

Digg2_Evenness_e.H.S <- aggregate(digg2$Evenness_e.H.S,
                                  by = list(digg2$Day),
                                  FUN = function(x) c(median = median(x), 
                                                      mean = mean(x), sd = sd(x)))
Digg2_Evenness_e.H.S





#  figures

digg4<- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Digging_in.csv")

attach(digg4)

individual_digg4 <- digg4 %>% 
  ggplot(aes(x=Day, y = Individuals, group = Day )) +
  geom_boxplot(
    outlier.fill = "transparent",
    outlier.shape = NA,
    fill = "#F0F0F0")+
  geom_point(aes(colour = Day),
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.22),
             size = 5,  alpha = 0.5) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  labs(x= " ",
       y= "Species abundance")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))+
  guides(
    color = "none",   
    fill = "none"   
  )
individual_digg4

Shannon_digg4 <- digg4 %>% 
  ggplot(aes(x=Day, y = Shannon_H, group = Day )) +
  geom_boxplot(
    outlier.fill = "transparent",
    outlier.shape = NA,
    fill = "#F0F0F0")+
  geom_point(aes(colour = Day),
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.22),
             size = 5,  alpha = 0.5) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  labs(x= "Periods",
       y= "Shannon_H diversity")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))+
  guides(
    color = "none",   
    fill = "none"   
  )
Shannon_digg4

Simpson_digg4 <- digg4 %>% 
  ggplot(aes(x=Day, y = Simpson_1.D, group = Day )) +
  geom_boxplot(
    outlier.fill = "transparent",
    outlier.shape = NA,
    fill = "#F0F0F0")+
  geom_point(aes(colour = Day),
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.22),
             size = 5,  alpha = 0.5) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  labs(x= "Periods",
       y= "Simpson's diversity")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))+
  guides(
    color = "none",   
    fill = "none"   
  )
Simpson_digg4

Evenness_digg4 <- digg4 %>% 
  ggplot(aes(x=Day, y = Evenness_e.H.S, group = Day )) +
  geom_boxplot(
    outlier.fill = "transparent",
    outlier.shape = NA,
    fill = "#F0F0F0")+
  geom_point(aes(colour = Day),
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.22),
             size = 5,  alpha = 0.5) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  labs(x= "Periods",
       y= "Evenness")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))+
  guides(
    color = "none",   
    fill = "none"   
  )
Evenness_digg4

Dominance_digg4 <- digg4 %>% 
  ggplot(aes(x=Day, y = Dominance_D, group = Day )) +
  geom_boxplot(
    outlier.fill = "transparent",
    outlier.shape = NA,
    fill = "#F0F0F0")+
  geom_point(aes(colour = Day),
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.22),
             size = 5,  alpha = 0.5) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  labs(x= " ",
       y= "Dominance")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))+
  guides(
    color = "none",   
    fill = "none"   
  )
Dominance_digg4



# combined charts

combined_chart <- individual_digg4 +
  Dominance_digg4 + Evenness_digg4 + Shannon_digg4 +
  Simpson_digg4 +
  plot_layout(nrow = 2, ncol = 3)
combined_chart
