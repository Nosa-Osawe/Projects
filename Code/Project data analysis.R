library(tidyr)    # load library
library(ggplot2)
library(dplyr)
library(dunn.test)
library(FSA)


ant <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\ants4T.csv") # load data
View(ant)
head(ant)
    #convert wide data to long data
antT <- gather(ant, Pitfall, NumOfCatch, P1:P12,factor_key=TRUE ) 
View(antT)

antTW <- spread(antT, key = Pitfall, value = NumOfCatch) # back to wide data 
View(antTW)

    # re-level factor (Month) to coerce it to appear in correct order in chart
antT$Month <- factor(antT$Month, levels = c("January", "February", "March", "April"))


#######################################################################################################
# all objects are named according to the respective ant species for each chart

Paltothrens_tarsatus <-antT %>% 
  filter(Species== "Paltothrens tarsatus") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Paltothrens_tarsatus


Paratrechina_species_cf._albibes <-antT %>% 
  filter(Species== "Paratrechina species cf. albibes") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Paratrechina_species_cf._albibes

Odontomachus_troglodytes <-antT %>% 
  filter(Species== "Odontomachus troglodytes") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Odontomachus_troglodytes

Lepisiota_capensis <-antT %>% 
  filter(Species== "Lepisiota capensis") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Lepisiota_capensis


pheidole_species <-antT %>% 
  filter(Species== "pheidole species") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
pheidole_species


Camponotus_vividus <-antT %>% 
  filter(Species== "Camponotus vividus") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Camponotus_vividus

Morphospecies_M <- antT %>% 
  filter(Species== "Morphospecies M") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Morphospecies_M

pheidole_welgelegenensis<- antT %>% 
  filter(Species== "pheidole welgelegenensis") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
pheidole_welgelegenensis

Odontomachus_troglodytes<- antT %>% 
  filter(Species== "Odontomachus troglodytes") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Odontomachus_troglodytes

Morphospecies_E <- antT %>% 
  filter(Species== "Morphospecies E") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Morphospecies_E

Monomorium_species <- antT %>% 
  filter(Species== "Monomorium species") %>%
  ggplot(aes(x=Month, y = NumOfCatch, fill = Day )) +
  geom_boxplot(aes(x=Month, y = NumOfCatch, fill = Day ))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Monomorium_species



####################################################################################################

#stacked area chart of percentage plant cover

plant <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\Plant_long.csv")

plant$Date <- as.Date(plant$Date, format = "%d/%m/%Y")
ggplot(plant, aes(x = Date , y = Percentage.cover, fill = Species)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = custom_colors) +  # Set custom colors using scale_fill_manual
  theme_classic()
 
# use ' + coord_flip()' to flip the axis


plantareachart <-ggplot(plant, aes(x= Date,y= Percentage.cover,fill = Species)) +
  geom_area(position = "stack", linejoin = "round")+
  labs(x = "Month", y = "Plant cover (%)", fill = "Species")+
  scale_fill_manual(values = custom_colors) +  # Set custom colors using scale_fill_manual
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
plantareachart


# Define your custom color palette
custom_colors <- c("#8c564b", "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", 
                    "#FF00FF", "#33312E")  # Replace with your desired colors

# Modify your ggplot code
plantareachart <- ggplot(plant, aes(x = Date, y = Percentage.cover, fill = Species)) +
  geom_area(position = "stack", linejoin = "round") +
  labs(x = "Month", y = "Plant cover (%)", fill = "Species") +
  scale_fill_manual(values = custom_colors) +  # Set custom colors using scale_fill_manual
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"))+
  theme(
    text = element_text(family = "Times New Roman", size = 18)  # Set font to Times New Roman and font size to 14
  )

plantareachart



# Modify your ggplot code to italicize legend text
plantareachart <- ggplot(plant, aes(x = Date, y = Percentage.cover, fill = Species)) +
  geom_area(position = "stack", linejoin = "round") +
  labs(x = "Month", y = "Plant cover (%)", fill = "Species") +
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.text = element_text(face = "italic")  # Italicize legend text
  )+
  theme(text = element_text(family = "Times New Roman",size = 18 ))

plantareachart

####################################################################################################

# Ant data bar plots

Morphospecies_E_bar <- antT %>% 
  filter(Species == "Morphospecies E") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Morphospecies_E_bar

Paltothrens_tarsatus_bar <- antT %>% 
  filter(Species == "Paltothrens tarsatus") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Paltothrens_tarsatus_bar


Camponotus_vividus_bar <- antT %>% 
  filter(Species == "Paltothrens tarsatus") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Camponotus_vividus_bar

Odontomachus_troglodytes_bar <- antT %>% 
  filter(Species == "Odontomachus troglodytes") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Odontomachus_troglodytes_bar

Monomorium_species_bar <- antT %>% 
  filter(Species == "Monomorium species") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Monomorium_species_bar

pheidole_species_bar <- antT %>% 
  filter(Species == "pheidole species") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
pheidole_species_bar

pheidole_welgelegenensis  <- antT %>% 
  filter(Species == "pheidole welgelegenensis") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
pheidole_welgelegenensis

Paratrechina_species_cf._albibes <- antT %>% 
  filter(Species == "Paratrechina species cf. albibes") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Paratrechina_species_cf._albibes


Lepisiota_capensis <- antT %>% 
  filter(Species == "Lepisiota capensis") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Lepisiota_capensis


Camponotus_maculatus <- antT %>% 
  filter(Species == "Camponotus maculatus") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Camponotus_maculatus


Morphospecies_M <- antT %>% 
  filter(Species == "Morphospecies M") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Morphospecies_M


Morphospecies_N <- antT %>% 
  filter(Species == "Morphospecies N") %>%
  ggplot(aes(x = Month, y = NumOfCatch, fill = Day)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Month", y = "Average number of individuals")
Morphospecies_N


##############################################################################################
# species diversity indices

diversity <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\Ant_diversity_data.csv")

View(diversity)

# Normality test
qqnorm(diversity$Shannon_H)
qqline(diversity$Shannon_H)
shapiro.test(diversity$Shannon_H)

qqnorm(diversity$Fisher_alpha)
qqline(diversity$Fisher_alpha)
shapiro.test(diversity$Fisher_alpha)

qqnorm(diversity$Taxa_S)
qqline(diversity$Taxa_S)
shapiro.test(diversity$Taxa_S)

qqnorm(diversity$Margalef)
qqline(diversity$Margalef)
shapiro.test(diversity$Margalef)

qqnorm(diversity$Individuals)
qqline(diversity$Individuals)
shapiro.test(diversity$Individuals)

qqnorm(diversity$Evenness_e.H.S)
qqline(diversity$Evenness_e.H.S)
shapiro.test(diversity$Evenness_e.H.S)

qqnorm(diversity$Dominance_D)
qqline(diversity$Dominance_D)
shapiro.test(diversity$Dominance_D)

## All are not normally distributed

attach(diversity)


diversity$Shannon_H <- as.integer(diversity$Shannon_H)
diversity$Month <- as.factor(diversity$Month)
diversity$Day <- as.factor(diversity$Day)



result <- kruskal.test(Shannon_H ~ Day + Month, data = diversity)

diversity %>% filter(Month == "January") %>%
qqnorm(diversity$Shannon_H)
qqline(diversity$Shannon_H)


January <- all_of(diversity) %>% 
  select(Month, Day, Pitfall, Shannon_H) %>%
  filter(Month == "January") %>% 
  spread(Day, Shannon_H) %>% # back to wide data
  select(-Month, - Pitfall)
  
kruskal.test(diversity$Taxa_S ~ diversity$Month)  

  
diversity$Month <- factor(diversity$Month, levels = c("January", "February", "March", "April"))


#    Taxa_S
kruskal.test(diversity$Taxa_S ~ diversity$Month)  
dunn.test(diversity$Taxa_S, diversity$Month, method = "bonferroni")



Monthly_boxplot_Taxa_S <- diversity %>% 
  select(Month, Taxa_S) %>%
  ggplot(aes(x=Month, y = Taxa_S, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Taxa_S))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= " Species richness")
Monthly_boxplot_Taxa_S

Day_boxplot_Taxa_S <- diversity %>% 
  select(Month, Day, Taxa_S) %>%
  ggplot(aes(x=Month, y = Taxa_S, fill = Day)) +
  geom_boxplot(aes(x=Month, y = Taxa_S, fill = Day))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Species richness")
Day_boxplot_Taxa_S


#    Individuals
kruskal.test(diversity$Individuals ~ diversity$Month)  
dunn.test(diversity$Individuals, diversity$Month, method = "bonferroni")

Monthly_boxplot_Individuals <- diversity %>% 
  select(Month, Individuals) %>%
  ggplot(aes(x=Month, y = Individuals)) +
  geom_boxplot(aes(x=Month, y = Individuals, fill = Month))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of individuals")
Monthly_boxplot_Individuals


Day_boxplot_Individuals <- diversity %>% 
  select(Month, Day, Individuals) %>%
  ggplot(aes(x=Month, y = Individuals, fill = Day)) +
  geom_boxplot(aes(x=Month, y = Individuals, fill = Day))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of Individuals")
Day_boxplot_Individuals

#    Evenness_e.H.S

kruskal.test(diversity$Evenness_e.H.S ~ diversity$Month)  
dunn.test(diversity$Evenness_e.H.S, diversity$Month, method = "bonferroni")

Monthly_boxplot_Evenness_e.H.S <- diversity %>% 
  select(Month, Evenness_e.H.S) %>%
  ggplot(aes(x=Month, y = Evenness_e.H.S, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Evenness_e.H.S))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Evenness")
Monthly_boxplot_Evenness_e.H.S

Evenness_e.H.S_D_summary <- aggregate(diversity$Evenness_e.H.S, by = list(diversity$Month),
                                 FUN = function(x) c(median = median(x), mean = mean(x), sd = sd(x)))
Evenness_e.H.S_D_summary

Day_boxplot_Evenness_e.H.S <- diversity %>% 
  select(Month, Day, Evenness_e.H.S) %>%
  ggplot(aes(x=Month, y = Evenness_e.H.S, fill = Day)) +
  geom_boxplot(aes(x=Month, y = Evenness_e.H.S, fill = Day))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Number of Individuals")
Day_boxplot_Evenness_e.H.S

# dominance

kruskal.test(diversity$Dominance_D ~ diversity$Month)  
dunn.test(diversity$Dominance_D, diversity$Month, method = "bonferroni")

Monthly_boxplot_Dominance_D <- diversity %>% 
  select(Month, Dominance_D) %>%
  ggplot(aes(x=Month, y = Dominance_D, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Dominance_D))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Dominance")
Monthly_boxplot_Dominance_D

Day_boxplot_Dominance_D <- diversity %>% 
  select(Month, Day, Dominance_D) %>%
  ggplot(aes(x=Month, y = Dominance_D, fill = Day)) +
  geom_boxplot(aes(x=Month, y = Dominance_D, fill = Day))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Dominance")
Day_boxplot_Dominance_D


Dominance_D_summary <- aggregate(diversity$Dominance_D, by = list(diversity$Month),
                                 FUN = function(x) c(median = median(x), mean = mean(x), sd = sd(x)))

Taxa_S_summary <- aggregate(diversity$Taxa_S, by = list(diversity$Month),
                                 FUN = function(x) c(median = median(x), mean = mean(x), sd = sd(x)))
Taxa_S_summary

Individuals_summary <- aggregate(diversity$Individuals, by = list(diversity$Month),
                            FUN = function(x) c(median = median(x), mean = mean(x), sd = sd(x)))
Individuals_summary


#################################################################################################

# combined days in the month 
C_diversity <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\C_Ant_diversity_data.csv")
View(C_diversity)
                    # re-level
C_diversity$Month <- factor(C_diversity$Month, levels = c("January", "February", "March", "April"))

attach(C_diversity)
C_diversity$Shannon_H <- as.integer(C_diversity$Shannon_H)
C_diversity$Month <- as.factor(C_diversity$Month)

    
# Shannon_h has been tested for normality using SPSS ~details in thesis notes
# Shannon_h = Normally distributed with few outliers 

anova_Shannon_H <- aov(C_diversity$Shannon_H ~ C_diversity$Month, data = C_diversity)
summary(anova_Shannon_H)
TukeyHSD(anova_Shannon_H)

Shannon_H_summary <- aggregate(C_diversity$Shannon_H,
                               by = list(C_diversity$Month),
                               FUN = function(x) c(median = median(x), 
                                                   mean = mean(x), sd = sd(x)))
Shannon_H_summary


Monthly_boxplot_Shannon_H <- C_diversity %>% 
  select(Month, Shannon_H) %>%
  ggplot(aes(x=Month, y = Shannon_H, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Shannon_H))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Shannon_H Diversity index")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_continuous(  breaks = c(1, 2, 3, 4),    
                       labels = c( "January", "February", "March", "April"))
Monthly_boxplot_Shannon_H



# Dominance_D has been tested for normality using SPSS ~details in thesis notes
# Dominance_D = Normally distributed with few outliers 

anova_Dominance_D <- aov(C_diversity$Dominance_D ~ C_diversity$Month, data = C_diversity)
summary(anova_Dominance_D)
TukeyHSD(anova_Dominance_D)

Dominance_D_summary <- aggregate(C_diversity$Dominance_D,
                                 by = list(C_diversity$Month),
                                 FUN = function(x) c(median = median(x), 
                                                     mean = mean(x), sd = sd(x)))
Dominance_D_summary


Monthly_boxplot_Dominance_D <- C_diversity %>% 
  select(Month, Dominance_D) %>%
  ggplot(aes(x=Month, y = Dominance_D, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Dominance_D))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Dominance_D Index")
Monthly_boxplot_Dominance_D



# Evenness_e.H.S has been tested for normality using SPSS ~details in thesis notes
# Evenness_e.H.S = Normally distributed with few outliers 

anova_Evenness_e.H.S <- aov(C_diversity$Evenness_e.H.S ~ C_diversity$Month, data = C_diversity)
summary(anova_Evenness_e.H.S)

TukeyHSD(anova_Evenness_e.H.S)

Evenness_e.H.S_D_summary <- aggregate(C_diversity$Evenness_e.H.S,
                                 by = list(C_diversity$Month),
                                 FUN = function(x) c(median = median(x), 
                                                     mean = mean(x), sd = sd(x)))
Evenness_e.H.S_D_summary

Monthly_boxplot_Evenness_e.H.S <- C_diversity %>% 
  select(Month, Evenness_e.H.S) %>%
  ggplot(aes(x=Month, y = Evenness_e.H.S, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Evenness_e.H.S))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Evenness_e.H.S")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )
Monthly_boxplot_Evenness_e.H.S
# Individuals has been tested for normality using SPSS ~details in thesis notes
# Individuals = not Normally distributed with few outliers 

kruskal.test(C_diversity$Individuals ~ C_diversity$Month)  
dunn.test(C_diversity$Individuals, C_diversity$Month, method = "bonferroni")
Individuals_summary <- aggregate(C_diversity$Individuals,
                                 by = list(C_diversity$Month),
                                 FUN = function(x) c(median = median(x), 
                                                     mean = mean(x), sd = sd(x)))
Individuals_summary


Monthly_boxplot_Individuals <- C_diversity %>% 
  select(Month, Individuals) %>%
  ggplot(aes(x=Month, y = Individuals, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Individuals))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Species Abundance")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )
Monthly_boxplot_Individuals




# Chao.1 has been tested for normality using SPSS ~details in thesis notes
# Chao.1 = not Normally distributed with few outliers 

kruskal.test(C_diversity$Chao.1 ~ C_diversity$Month)  
dunn.test(C_diversity$Chao.1, C_diversity$Month, method = "bonferroni")

Individuals_Chao.1 <- aggregate(C_diversity$Chao.1,
                                by = list(C_diversity$Month),
                                FUN = function(x) c(median = median(x), 
                                                    mean = mean(x), sd = sd(x)))
Individuals_Chao.1

Monthly_boxplot_Chao.1 <- C_diversity %>% 
  select(Month, Chao.1) %>%
  ggplot(aes(x=Month, y = Chao.1, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Chao.1))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Chao 1 estimate")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )
Monthly_boxplot_Chao.1



# Taxa_S has been tested for normality using SPSS ~details in thesis notes
# Taxa_S = not Normally distributed with few outliers 


kruskal.test(C_diversity$Taxa_S ~ C_diversity$Month)  
dunn.test(C_diversity$Taxa_S, C_diversity$Month, method = "bonferroni")

Individuals_Taxa_S <- aggregate(C_diversity$Taxa_S,
                                by = list(C_diversity$Month),
                                FUN = function(x) c(median = median(x), 
                                                    mean = mean(x), sd = sd(x)))
Individuals_Taxa_S



Monthly_boxplot_Taxa_S <- C_diversity %>% 
  select(Month, Taxa_S) %>%
  ggplot(aes(x=Month, y = Taxa_S, fill = Month)) +
  geom_boxplot(aes(x=Month, y = Taxa_S))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x= "Month",
       y= "Species richness")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )
Monthly_boxplot_Taxa_S

C_diversity$Month <- factor(C_diversity$Month, levels = c("January", "February", "March", "April"))
C_diversity$Period <- as.numeric(C_diversity$Period)
Monthly_colors <- c("#8c564b", "#1f77b4", "#ff7f0e", "#2ca02c")

spline_boxplot_Shannon_H <- C_diversity %>% 
  select(Month, Shannon_H, Period) %>%
  ggplot(aes(x= Period, y = Shannon_H)) +
  geom_boxplot(aes(x= Month, y = Shannon_H), fill = Month)+
  scale_color_viridis_d()+
  theme_classic()+
  geom_smooth(method = "loess", se = TRUE,
              colour = "black",
              size = 1.0 )+
  labs(x= "Month",
       y= "Shannon_H Diversity index")+
  theme(
    text = element_text(family = "Times New Roman", size = 14)  # Set font to Times New Roman and font size to 14
  )+
  scale_x_discrete(labels = c( "January", "February", "March", "April"))
  spline_boxplot_Shannon_H
