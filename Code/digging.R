
library(dunn.test)
library(FSA)
library(tidyverse)

digg <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Digging_in data.csv")
View(digg)
digg<- read_csv("Data/Digging_in data.csv")
digg$Day <- factor(digg$Day, levels = c("2", "3", "4", "6", "7",
                                        "8", "9", "10", "11"))
#digg$Day<- as.numeric(digg$Day)
digg_Taxa_S <-digg %>% 
  ggplot(aes(x=Day, y = Taxa_S, fill = Sampling.period )) +
 geom_boxplot(outlier.fill = "transparent", outlier.shape = NA)+
  theme_classic()+
  labs(x= "Days",
       y= "Species richness")+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.2) +
  scale_color_viridis_d() +
  theme_classic() +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = 1),
               col = "black", size = 1.2) +
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )
digg_Taxa_S



digg_Individuals <-digg %>% 
  ggplot(aes(x=Day, y = Individuals, fill = Sampling.period )) +
  geom_boxplot(outlier.fill = "transparent", outlier.shape = NA)+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Days",
       y= "Species abundance")+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                             dodge.width = 0.22),
             size = 4,  alpha = 0.4) +
  scale_color_viridis_d() +
  theme_classic() +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = 1),
               col = "black", size = 1.2) +
  theme(
    text = element_text(size = 14)  # Set the font size to 14 (adjust as needed)
  )
digg_Individuals

digg_Simpson_1.D <-digg %>% 
  ggplot(aes(x=Day, y = Simpson_1.D, fill = Sampling.period)) +
  geom_boxplot(outlier.fill = "transparent", outlier.shape = NA)+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Days",
       y= "Simpsons' diversity")+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.4) +
  theme_classic() +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = 1),
               col = "black", size = 1.3)
digg_Simpson_1.D

digg_Shannon_H <-digg %>% 
  ggplot(aes(x=Day, y = Shannon_H, fill = Sampling.period )) +
  geom_boxplot(outlier.fill = "transparent", outlier.shape = NA)+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Days",
       y= "Shannon diversity index")+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.4) +
  theme_classic() +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = 1),
               col = "black", size = 1.3) +
  theme(
    text = element_text(size = 14)  # Set the font size to 14 (adjust as needed)
  )
digg_Shannon_H


digg_Dominance_D <-digg %>% 
  ggplot(aes(x=Day, y = Dominance_D, fill = Sampling.period )) +
  geom_boxplot(outlier.fill = "transparent", outlier.shape = NA)+
  theme_classic()+
  labs(x= "Days",
       y= "Dominance_D index")+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.4) +
  scale_color_viridis_d() +
  theme_classic() +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = 1),
               col = "black", size = 1.2)
digg_Dominance_D

digg_Evenness_e.H.S<-digg %>% 
  ggplot(aes(x=Day, y = Evenness_e.H.S, fill = Sampling.period )) +
  geom_boxplot(outlier.fill = "transparent", outlier.shape = NA)+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Days",
       y= "Evenness")+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22), 
             size = 4,  alpha = 0.4) +
  scale_color_viridis_d() +
  theme_classic() +
  stat_summary(fun = mean,
               geom = "line",
               aes(group = 1),
               col = "black", size = 1.2)
digg_Evenness_e.H.S


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






digg$Individuals
digg$Simpson_1.D
digg$Shannon_H
digg$Dominance_D
digg$Evenness_e.H.S
digg$Chao.1

#################################################################################
digg3 <-read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Digging_in data.csv")
digg3 <- digg
digg3$Day<- as.numeric(digg3$Day)
digg3$Dayofsampling <- factor(digg3$Dayofsampling)

individual_digg3 <- digg3 %>% 
  ggplot(aes(x=Day, y = Individuals )) +
  geom_boxplot(aes(group = Dayofsampling),
               outlier.fill = "transparent",
               outlier.shape = NA,
               fill = "#F0F0F0")+
  geom_point(aes(colour = Sampling.period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.5) +
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.2 )+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),    
                       labels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))+
  labs(x= "Days",
       y= "Species abundance")

individual_digg3


Taxa_S_digg3 <- digg3 %>% 
  ggplot(aes(x=Day, y = Taxa_S )) +
  geom_boxplot(aes(group = Dayofsampling),
               outlier.fill = "transparent",
               outlier.shape = NA,
               fill = "#F0F0F0")+
  geom_point(aes(colour = Sampling.period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.5) +
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.2 )+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),    
                       labels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))+
  labs(x= "Days",
       y= "Species richness")
Taxa_S_digg3


Simpson_1.D_digg3 <- digg3 %>% 
  ggplot(aes(x=Day, y = Simpson_1.D )) +
  geom_boxplot(aes(group = Dayofsampling),
               outlier.fill = "transparent",
               outlier.shape = NA,
               fill = "#F0F0F0")+
  geom_point(aes(colour = Sampling.period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.5) +
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.2 )+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),    
                       labels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))+
  labs(x= "Days",
       y= "Simpson's diversity index")
Simpson_1.D_digg3


Shannon_H_digg3 <- digg3 %>% 
  ggplot(aes(x=Day, y = Shannon_H )) +
  geom_boxplot(aes(group = Dayofsampling),
               outlier.fill = "transparent",
               outlier.shape = NA,
               fill = "#F0F0F0")+
  geom_point(aes(colour = Sampling.period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.5) +
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.2 )+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),    
                       labels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))+
  labs(x= "Days",
       y= "Shannon_H diversity index")
Shannon_H_digg3


Evenness_e.H.S_digg3 <- digg3 %>% 
  ggplot(aes(x=Day, y = Evenness_e.H.S )) +
  geom_boxplot(aes(group = Dayofsampling),
               outlier.fill = "transparent",
               outlier.shape = NA,
               fill = "#F0F0F0")+
  geom_point(aes(colour = Sampling.period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.5) +
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.2 )+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),    
                       labels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))+
  labs(x= "Days",
       y= "Evenness")
Evenness_e.H.S_digg3


Dominance_D_digg3 <- digg3 %>% 
  ggplot(aes(x=Day, y = Dominance_D )) +
  geom_boxplot(aes(group = Dayofsampling),
               outlier.fill = "transparent",
               outlier.shape = NA,
               fill = "#F0F0F0")+
  geom_point(aes(colour = sampling),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.22),
             size = 4,  alpha = 0.5) +
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.2 )+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+ scale_x_continuous(  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),    
                       labels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))+
  labs(x= "Days",
       y= "Dominance")
Dominance_D_digg3

individual_digg3
Taxa_S_digg3
Simpson_1.D_digg3
Shannon_H_digg3
Evenness_e.H.S_digg3
Dominance_D_digg3


library(ggplot2)

digg_cor <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\cor_dig.csv")
View(digg_cor)

indices_colour <- c( "red", "darkgreen", "red", "darkgreen", "darkgreen")

attach(digg_cor)
ggplot(digg_cor, aes(x=Indices, y = Mean.Cor., color= Indices))+
  geom_pointrange(aes(ymin = Mean.Cor.- Sd, ymax = Mean.Cor.+ Sd ))+
  scale_color_manual(values = indices_colour)+
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.line.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.line.x = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(
    text = element_text(family = "Times New Roman", size = 15))+
  labs(y= "Pearson correlation", legend = FALSE)+
  scale_x_discrete( labels = c( "Dominance", "Evenness",
                                "Abundance", "Shannon_H", 
                                "Simpson_D"))+
  geom_point(size = 4)

 
digg4<- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Digging_in data.csv")
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
  labs(x= "Periods",
       y= "Species abundance")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))
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
                                "3"))
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
                                "3"))
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
                                "3"))
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
  labs(x= "Periods",
       y= "Dominance")+
  scale_x_discrete( labels = c( "1", "2",
                                "3"))
Dominance_digg4