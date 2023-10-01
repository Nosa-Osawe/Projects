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
    text = element_text(family = "Times New Roman", size = 16), 
    # Set font to Times New Roman and font size to 18
    plot.title = element_text(face = "italic"),  # Italicize plot title
    legend.text = element_text(face = "italic"),  # Italicize legend text
    plot.caption = element_text(face = "italic")
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

  
  
  #############################################################################################
  
  # combined days in the month 
  C_diversity2 <- read.csv("C:\\Users\\user\\Desktop\\ant_div.csv")
  View(C_diversity2)
  
  C_diversity2$Month <- factor(C_diversity2$Month, levels = c("December", "January", "February", "March", "April"))
  
  attach(C_diversity2)
  #C_diversity$Shannon_H <- as.integer(C_diversity$Shannon_H)
  C_diversity2$Month <- factor(C_diversity2$Month)
  C_diversity2$Period<- as.numeric( C_diversity2$Period)
  spray_period <- c( "#BABABA", "#BABABA", "#F3F3F3", "#F3F3F3", "#F3F3F3")
  Monthly_colors <-c("#FFFF0C", "orange", "#353535", "#A0662C", "brown")
  
  Shannon_H2 <- C_diversity2 %>% 
    select(Period, Shannon_H) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Shannon_H),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Shannon_H, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Shannon_H),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Shannon_H diversity index", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                         labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Shannon_H2
  
  
  Taxa_S2 <- C_diversity2 %>% 
    select(Period, Taxa_S) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Taxa_S),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Taxa_S, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Taxa_S),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Species richness", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                       labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Taxa_S2
  
  
  Dominance_D2 <- C_diversity2 %>% 
    select(Period, Dominance_D) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Dominance_D),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Dominance_D, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Dominance_D),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Dominance", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                       labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Dominance_D2
  
  
  Evenness_e.H.S2 <- C_diversity2 %>% 
    select(Period, Evenness_e.H.S) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Evenness_e.H.S),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Evenness_e.H.S, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Evenness_e.H.S),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Evenness", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                       labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Evenness_e.H.S2
  
  
  Simpson_1.D2 <- C_diversity2 %>% 
    select(Period, Simpson_1.D) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Simpson_1.D),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Simpson_1.D, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Simpson_1.D),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Simpson diversity index", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                       labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Simpson_1.D2
  
  
  
  Individuals2 <- C_diversity2 %>% 
    select(Period, Individuals) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Individuals),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Individuals, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Individuals),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Species abundance", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                       labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Individuals2
  
  
  Chao.12 <- C_diversity2 %>% 
    select(Period, Chao.1) %>%
    ggplot() +
    geom_boxplot(aes(x=factor(Period), y = Chao.1),
                 outlier.fill = "transparent",
                 outlier.shape = NA,
                 fill = spray_period)+
    geom_point(aes(x=factor(Period), y = Chao.1, colour = Month),
               position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4),
               size = 6,  alpha = 0.75) +
    scale_color_manual(values = Monthly_colors)+
    geom_smooth(aes(x= Period, y= Chao.1),  method = "loess", se = TRUE,formula = y ~ x,
                colour = "black",
                size = 1.0 )+
    theme_classic()+
    labs(x= "Month",
         y= "Chao.1 estimates", legend = FALSE)+
    theme(
      text = element_text(family = "Times New Roman", size = 20))+
    scale_x_discrete(  breaks = c(1, 2, 3, 4, 5),    
                       labels = c( "Dec", "Jan", "Feb", "Mar", "Apr"))
  Chao.12
  
  
##################################################################
  # hypothesis testing
  
  kruskal.test(C_diversity2$Taxa_S ~ C_diversity2$Month)  
  dunn.test(C_diversity2$Taxa_S, C_diversity2$Month, method = "bonferroni")
Taxa_S_2 <- aggregate(C_diversity2$Taxa_S,
                                  by = list(C_diversity2$Month),
                                  FUN = function(x) c(median = median(x), 
                                                      mean = mean(x), sd = sd(x)))
Taxa_S_2


kruskal.test(C_diversity2$Individuals ~ C_diversity2$Month)  
dunn.test(C_diversity2$Individuals, C_diversity2$Month, method = "bonferroni")
Individuals_2 <- aggregate(C_diversity2$Individuals,
                      by = list(C_diversity2$Month),
                      FUN = function(x) c(median = median(x), 
                                          mean = mean(x), sd = sd(x)))
Individuals_2

anova_Evenness_e.H.S2 <- aov(C_diversity2$Evenness_e.H.S ~ C_diversity2$Month, data = C_diversity2)
summary(anova_Evenness_e.H.S2)

TukeyHSD(anova_Evenness_e.H.S2)

Evenness_e.H.S_D_summary2 <- aggregate(C_diversity2$Evenness_e.H.S,
                                      by = list(C_diversity2$Month),
                                      FUN = function(x) c(median = median(x), 
                                                          mean = mean(x), sd = sd(x)))
Evenness_e.H.S_D_summary2



anova_Shannon_H2 <- aov(C_diversity2$Shannon_H ~ C_diversity2$Month, data = C_diversity2)
summary(anova_Shannon_H2)

TukeyHSD(anova_Shannon_H2)

Shannon_H_summary2 <- aggregate(C_diversity2$Shannon_H,
                                       by = list(C_diversity2$Month),
                                       FUN = function(x) c(median = median(x), 
                                                           mean = mean(x), sd = sd(x)))
Shannon_H_summary2

anova_Simpson_1.D <- aov(C_diversity2$Simpson_1.D ~ C_diversity2$Month, data = C_diversity2)
summary(anova_Simpson_1.D)

TukeyHSD(anova_Simpson_1.D)

Simpson_1.D_summary2 <- aggregate(C_diversity2$Simpson_1.D,
                                by = list(C_diversity2$Month),
                                FUN = function(x) c(median = median(x), 
                                                    mean = mean(x), sd = sd(x)))
Simpson_1.D_summary2

anova_Dominance_D <- aov(C_diversity2$Dominance_D ~ C_diversity2$Month, data = C_diversity2)
summary(anova_Dominance_D)

TukeyHSD(anova_Dominance_D)

Dominance_D_summary2 <- aggregate(C_diversity2$Dominance_D,
                                  by = list(C_diversity2$Month),
                                  FUN = function(x) c(median = median(x), 
                                                      mean = mean(x), sd = sd(x)))
Dominance_D_summary2






