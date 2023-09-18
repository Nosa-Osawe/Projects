rm(list = ls()) # clear environment
cat("\014") # clear console
library(tidyverse)
library(vegan)
library(corrplot)

antdiversitynmds <- read.csv("C:\\Users\\user\\Desktop\\antdiversitynmds.csv")
view(antdiversitynmds)
dim(antdiversitynmds)
#subset_plantNMDS <- PlantNMDS %>% filter(Samples == "Sample1")
#subset_plantNMDS <- PlantNMDS[PlantNMDS$Samples %in% c("Sampling1", "Sampling5"), ]
antdata1 <- antdiversitynmds[,3:8]
antdata2 <- antdiversitynmds[,1:2]
antNMDS <- metaMDS(antdata1, distance = "bray", k=2)
antNMDS$stress

antNMDS$points
antNMDS_df <- as.data.frame(antNMDS$points)

antfit <- adonis(antdata1~Month, data = antdata2, permutations = 666, method = "bray")
antfit
# adonis2 is recommended 
antfit2 <- adonis2 (antdata1~Month, data = antdata2, permutations = 666, method = "bray")
antfit2

ggplot(antdiversitynmds, aes(x = antNMDS_df$MDS1, y = antNMDS_df$MDS2, color = Month)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_color_manual(values = c("January" = "red", "February" = "green", "March" = "blue",
                                "April" = "orange")) +  # Specify colors
  theme_minimal()+
  stat_ellipse(geom = "polygon", aes(group = Month), level = 0.95, fill = "transparent")

corrplot(cor(antNMDS$points, antdata1),  method ='color',
         addCoef.col='black',
         tl.cex = 1, tl.col = 'black')


###################################################################################

anttt <- read.csv("C:\\Users\\user\\Desktop\\anttttttttttttt.csv")
view(anttt)
dim(anttt)
#subset_plantNMDS <- PlantNMDS %>% filter(Samples == "Sample1")
#subset_plantNMDS <- PlantNMDS[PlantNMDS$Samples %in% c("Sampling1", "Sampling5"), ]
ant_data1 <- anttt[,3:14]
ant_data2 <- anttt[,1:2]
ant_NMDS <- metaMDS(ant_data1, distance = "bray", k=3)
ant_NMDS$stress

ant_NMDS$points
ant_NMDS_df <- as.data.frame(ant_NMDS$points)

ant_fit2 <- adonis2 (ant_data1~Month, data = ant_data2, permutations = 666, method = "bray")
ant_fit2


ggplot(anttt, aes(x = ant_NMDS_df$MDS1, y = ant_NMDS_df$MDS2, color = Month)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_color_manual(values = c("January" = "red", "February" = "green", "March" = "blue",
                                "April" = "orange")) +  # Specify colors
  theme_minimal()+
  stat_ellipse(geom = "polygon", aes(group = Month), level = 0.95, fill = "transparent")

dissimilarity_matrix <- vegdist(ant_NMDS$points)


dim(ant_NMDS_df)

corrplot(cor(ant_NMDS$points, ant_data1),  method ='color',
         addCoef.col='black',
         tl.cex = 0.7, tl.col = 'black')
