rm(list = ls()) # clear environment
cat("\014") # clear console
library(tidyverse)
install.packages("vegan")
install.packages("pairwiseAdonis")
library(vegan)
library(corrplot)
PlantNMDS <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\plantNMDS.csv")
View(PlantNMDS)
#subset_plantNMDS <- PlantNMDS %>% filter(Samples == "Sample1")
#subset_plantNMDS <- PlantNMDS[PlantNMDS$Samples %in% c("Sampling1", "Sampling5"), ]
data1 <- PlantNMDS[,3:14]
data2 <- PlantNMDS[,1:2]
NMDS <- metaMDS(data1, distance = "bray", k=2)

# check stress level 
NMDS$stress # stress > 0.2 : poor fit; stress between 0.05- <0.2: good fit;
            # stress <0.05 : excellent fit # we got 0.18

NMDS_scores <- as.data.frame(scores(NMDS))

co=c("red", "green", "blue", "brown", "orange")
shape= c(18, 16, 10, 34, 42)
plot(NMDS$points, col=co[data2$Samples], pch= shape[data2$Samples],
     cex= 1, xlab="axis 1", ylab = "axis 2")
line_width <- 2

ordispider(NMDS, groups = data2$Samples, label = TRUE,  col =co, lwd = line_width)

# test for statistical difference

fit <- adonis(data1~Samples, data = data2, permutations = 10, method = "bray")
fit
# adonis2 is recommended 
fit2 <- adonis2 (data1~Samples, data = data2, permutations = 999, method = "bray")
fit2


distance_data <- vegdist(data1)
anova(betadisper(distance_data, data2$Samples))

  
NMDS$points
NMDS_df <- as.data.frame(NMDS$points)

corrplot(cor(NMDS$points, data1),
         addCoef.col='black',
         tl.cex = 1.2, tl.col = 'black', cl.ratio = 0.15
        )
#or....
corrplot(cor(NMDS$points, data1), method = c("number"),
addCoef.col='black',
tl.cex = 1.2, tl.col = 'black', cl.ratio = 0.15)



library(ggplot2)

# Create a ggplot2 plot
ggplot(PlantNMDS, aes(x = NMDS_df$MDS1, y = NMDS_df$MDS2, color = Samples)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_color_manual(values = c("S1" = "red", "S2" = "green", "S3" = "blue" ,
                                "S4" = "brown",
                                "S5" = "orange")) +  # Specify colors
  theme_minimal()+
  stat_ellipse(geom = "polygon", aes(group = Samples),
               level = 0.95, 
               size = 0.9,
               fill = "transparent")+
  theme(
    text = element_text(family = "Times New Roman", size = 16)  # Set font to Times New Roman and font size to 14
  )+
  theme_classic()




##################################################################################

data1 <- PlantNMDS[,3:14]
data2 <- PlantNMDS[,1:2]
NMDS <- metaMDS(data1, distance = "bray", k=2)

fit2 <- adonis2 (data1~Samples, data = data2, permutations = 999,
                 method = "bray")
fit2
#############################################################

S1S2 <-PlantNMDS %>% 
  filter(Samples=="S1" | Samples== "S2")
data1_S1S2 <- S1S2[,3:14]
data2_S1S2 <- S1S2[,1:2]
NMDS_S1S2 <- metaMDS(data1_S1S2, distance = "bray", k=2)

fit2_S1S2 <- adonis2 (data1_S1S2~Samples, data = data2_S1S2, permutations = 999,
                      method = "bray")
fit2_S1S2

#################################################################################
S1S3 <-PlantNMDS %>% 
  filter(Samples=="S1" | Samples== "S3")
data1_S1S3 <- S1S3[,3:14]
data2_S1S3 <- S1S3[,1:2]
NMDS_S1S3 <- metaMDS(data1_S1S3, distance = "bray", k=2)

fit2_S1S3 <- adonis2 (data1_S1S3~Samples, data = data2_S1S3, permutations = 999,
                      method = "bray")
fit2_S1S3
################################################################################

S1S4 <-PlantNMDS %>% 
  filter(Samples=="S1" | Samples== "S4")

data1_S1S4 <- S1S4[,3:14]
data2_S1S4 <- S1S4[,1:2]
NMDS_S1S4 <- metaMDS(data1_S1S4, distance = "bray", k=2)

fit2_S1S4 <- adonis2 (data1_S1S4~Samples, data = data2_S1S4, permutations = 999,
                      method = "bray")
fit2_S1S4
##################################################################################
S1S5 <-PlantNMDS %>% 
  filter(Samples=="S1" | Samples== "S5")

data1_S1S5 <- S1S5[,3:14]
data2_S1S5 <- S1S5[,1:2]
NMDS_S1S5 <- metaMDS(data1_S1S5, distance = "bray", k=2)

fit2_S1S5 <- adonis2 (data1_S1S5~Samples, data = data2_S1S5, permutations = 999,
                      method = "bray")
fit2_S1S5
#################################################################################
S2S5 <-PlantNMDS %>% 
  filter(Samples=="S1" | Samples== "S5")

data1_S1S5 <- S1S5[,3:14]
data2_S1S5 <- S1S5[,1:2]
NMDS_S1S5 <- metaMDS(data1_S1S5, distance = "bray", k=2)

fit2_S1S5 <- adonis2 (data1_S1S5~Samples, data = data2_S1S5, permutations = 999,
                      method = "bray")
fit2_S1S5
#################################################################################
S2S3 <-PlantNMDS %>% 
  filter(Samples=="S2" | Samples== "S3")

data1_S2S3 <- S2S3[,3:14]
data2_S2S3 <- S2S3[,1:2]
NMDS_S2S3 <- metaMDS(data1_S2S3, distance = "bray", k=2)

fit2_S2S3 <- adonis2 (data1_S2S3~Samples, data = data2_S1S5, permutations = 999,
                      method = "bray")
fit2_S2S3

#####################################################################################
S2S4 <-PlantNMDS %>% 
  filter(Samples=="S2" | Samples== "S4")

data1_S2S4 <- S2S4[,3:14]
data2_S2S4 <- S2S4[,1:2]
NMDS_S2S4 <- metaMDS(data1_S2S4, distance = "bray", k=2)

fit2_S2S4 <- adonis2 (data1_S2S4~Samples, data = data2_S1S4, permutations = 999,
                      method = "bray")
fit2_S2S4

################################################################################
S2S5 <-PlantNMDS %>% 
  filter(Samples=="S2" | Samples== "S5")
data1_S2S5 <- S2S5[,3:14]
data2_S2S5 <- S2S5[,1:2]
NMDS_S2S5 <- metaMDS(data1_S2S5, distance = "bray", k=2)

fit2_S2S5 <- adonis2 (data1_S2S5~Samples, data = data2_S1S5, permutations = 999,
                      method = "bray")
fit2_S2S5

##################################################################################
S3S4 <-PlantNMDS %>% 
  filter(Samples=="S3" | Samples== "S4")
data1_S3S4 <- S3S4[,3:14]
data2_S3S4 <- S3S4[,1:2]
NMDS_S3S4 <- metaMDS(data1_S3S4, distance = "bray", k=2)

fit2_S3S4 <- adonis2 (data1_S3S4~data2_S3S4$Samples, data = data2_S3S4, permutations = 999,
                      method = "bray")
fit2_S3S4

#################################################################################
S3S5 <-PlantNMDS %>% 
  filter(Samples=="S3" | Samples== "S5")
data1_S3S5 <- S3S5[,3:14]
data2_S3S5 <- S3S5[,1:2]
NMDS_S3S5 <- metaMDS(data1_S3S5, distance = "bray", k=2)

fit2_S3S5 <- adonis2 (data1_S3S5~data2_S3S5$Samples, data = data2_S3S5, permutations = 999,
                      method = "bray")
fit2_S3S5
#################################################################################

S5S4 <-PlantNMDS %>% 
  filter(Samples=="S5" | Samples== "S4")
data1_S5S4 <- S5S4[,3:14]
data2_S5S4 <- S5S4[,1:2]
NMDS_S5S4 <- metaMDS(data1_S5S4, distance = "bray", k=2)

fit2_S5S4 <- adonis2 (data1_S5S4~data2_S5S4$Samples, data = data2_S5S4, permutations = 999,
                      method = "bray")
fit2_S5S4



pvalues_plant<-c(fit2_S1S2$`Pr(>F)`,
fit2_S1S3$`Pr(>F)`,
fit2_S1S4$`Pr(>F)`,
fit2_S1S5$`Pr(>F)`,
fit2_S2S3$`Pr(>F)` , 
fit2_S2S4$`Pr(>F)` ,
fit2_S2S5$`Pr(>F)` ,
fit2_S3S5$`Pr(>F)` ,
fit2_S5S4$`Pr(>F)`)

p.adjust(pvalues_plant,method="bonferroni")

