rm(list = ls()) # clear environment
cat("\014") # clear console
library(tidyverse)
library(vegan)
library(corrplot)

antdiversitynmds <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\antdiversitynmds.csv")
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
antfit2 <- adonis2 (antdata1~Month, data = antdata2, permutations = 9999, method = "bray")
antfit2

ggplot(antdiversitynmds, aes(x = antNMDS_df$MDS1, y = antNMDS_df$MDS2, color = Month)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_color_manual(values = c("January" = "red", "February" = "green", "March" = "lightblue",
                                "April" = "purple")) +  # Specify colors
  theme_minimal()+
  stat_ellipse(geom = "polygon", aes(group = Month), level = 0.95, fill = "transparent")+
  theme(
    text = element_text(family = "Times New Roman", size = 15)  # Set font to Times New Roman and font size to 14
  )+
  theme_classic()

corrplot(cor(antNMDS$points, antdata1),  method ='color',
         addCoef.col='black',
         tl.cex = 1, tl.col = 'black')


###################################################################################

antdiversitynmds2 <- read.csv("C:\\Users\\user\\Desktop\\antNMDS2.csv")
view(antdiversitynmds2)
dim(antdiversitynmds2)
#subset_plantNMDS <- PlantNMDS %>% filter(Samples == "Sample1")
#subset_plantNMDS <- PlantNMDS[PlantNMDS$Samples %in% c("Sampling1", "Sampling5"), ]
antdata12 <- antdiversitynmds2[,3:8]
antdata22 <- antdiversitynmds2[,1:2]
antNMDS2 <- metaMDS(antdata12, distance = "bray", k=2)
antNMDS2$stress

antNMDS2$points
antNMDS_df2 <- as.data.frame(antNMDS2$points)

# adonis2 is recommended 
antfit22 <- adonis2 (antdata12~Month, data = antdata22, permutations = 9999, method = "bray")
antfit22

ggplot(antdiversitynmds2, aes(x = antNMDS_df2$MDS1, y = antNMDS_df2$MDS2, color = Month)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_color_manual(values = c("December" = "yellow","January" = "orange", 
                                "February" = "#353535", "March" = "#A0662C",
                                "April" = "brown")) +  # Specify colors
  stat_ellipse(geom = "polygon", aes(group = Month), 
               level = 0.95,
               size = 0.9,
               fill = "transparent")+
  theme(
    text = element_text(family = "Times New Roman", size = 20))+  
  theme_classic()

corrplot(cor(antNMDS2$points, antdata12),  method = c("number"),
         addCoef.col='black',
         tl.cex = 1.2, tl.col = 'black', cl.ratio = 0.15
        )

DecJan <-antdiversitynmds2 %>% 
  filter(Month=="December" | Month== "January")

data1_DecJan <- DecJan[,3:8]
data2_DecJan <- DecJan[,1:2]
NMDS_DecJan<- metaMDS(data1_DecJan, distance = "bray", k=2)

fit2_DecJan <- adonis2 (data1_DecJan~Month, data = data2_DecJan, permutations = 999,
                        method = "bray")
fit2_DecJan

DecApr <-antdiversitynmds2 %>% 
  filter(Month=="December" | Month== "April")

data1_DecApr <- DecApr[,3:8]
data2_DecApr <- DecApr[,1:2]
NMDS_DecApr<- metaMDS(data1_DecApr, distance = "bray", k=2)

fit2_DecApr <- adonis2 (data1_DecApr~Month, data = data2_DecApr, permutations = 9999,
                        method = "bray")
fit2_DecApr




JanMar <-antdiversitynmds2 %>% 
  filter(Month=="March" | Month== "January")

data1_JanMar <- JanMar[,3:8]
data2_JanMar <- JanMar[,1:2]
NMDS_JanMar<- metaMDS(data1_JanMar, distance = "bray", k=2)

fit2_JanMar <- adonis2 (data1_JanMar~Month, data = data2_JanMar, permutations = 9999,
                        method = "bray")
fit2_JanMar


MarApr <-antdiversitynmds2 %>% 
  filter(Month=="March" | Month== "April")

data1_MarApr <- MarApr[,3:8]
data2_MarApr <- MarApr[,1:2]
NMDS_MarApr<- metaMDS(data1_MarApr, distance = "bray", k=2)

fit2_MarApr <- adonis2 (data1_MarApr~Month, data = data2_MarApr, permutations = 9999,
                        method = "bray")
fit2_MarApr


FebMAr <-antdiversitynmds2 %>% 
  filter(Month=="February" | Month== "March")

data1_FebMAr <- FebMAr[,3:8]
data2_FebMAr <- FebMAr[,1:2]
NMDS_FebMAr<- metaMDS(data1_FebMAr, distance = "bray", k=2)

fit2_FebMAr <- adonis2 (data1_FebMAr~Month, data = data2_FebMAr, permutations = 9999,
                        method = "bray")
fit2_FebMAr


FebJan <-antdiversitynmds2 %>% 
  filter(Month=="February" | Month== "January")

data1_FebJan <- FebJan[,3:8]
data2_FebJan <- FebJan[,1:2]
NMDS_FebJan<- metaMDS(data1_FebJan, distance = "bray", k=2)

fit2_FebJan <- adonis2 (data1_FebJan~Month, data = data2_FebJan, permutations = 9999,
                        method = "bray")
fit2_FebJan

FebApr <-antdiversitynmds2 %>% 
  filter(Month=="February" | Month== "April")

data1_FebApr <- FebApr[,3:8]
data2_FebApr <- FebApr[,1:2]
NMDS_FebApr<- metaMDS(data1_FebApr, distance = "bray", k=2)

fit2_FebApr <- adonis2 (data1_FebApr~Month, data = data2_FebApr, permutations = 9999,
                        method = "bray")
fit2_FebApr


JanApr <-antdiversitynmds2 %>% 
  filter(Month=="January" | Month== "April")

data1_JanApr <- JanApr[,3:8]
data2_JanApr <- JanApr[,1:2]
NMDS_JanApr<- metaMDS(data1_JanApr, distance = "bray", k=2)

fit2_JanApr <- adonis2 (data1_JanApr~Month, data = data2_JanApr, permutations = 9999,
                        method = "bray")
fit2_JanApr



DecFeb <-antdiversitynmds2 %>% 
  filter(Month=="December" | Month== "February")

data1_DecFeb <- DecFeb[,3:8]
data2_DecFeb <- DecFeb[,1:2]
NMDS_DecFeb<- metaMDS(data1_DecFeb, distance = "bray", k=2)

fit2_DecFeb <- adonis2 (data1_DecFeb~Month, data = data2_DecFeb, permutations = 9999,
                        method = "bray")
fit2_DecFeb


DecMar <-antdiversitynmds2 %>% 
  filter(Month=="December" | Month== "March")

data1_DecMar <- DecMar[,3:8]
data2_DecMar <- DecMar[,1:2]
NMDS_DecMar<- metaMDS(data1_DecMar, distance = "bray", k=2)

fit2_DecMar <- adonis2 (data1_DecMar~Month, data = data2_DecMar, permutations = 9999,
                        method = "bray")
fit2_DecMar

DecJan <-antdiversitynmds2 %>% 
  filter(Month=="December" | Month== "January")
data1_DecJan <- DecJan[,3:8]
data2_DecJan <- DecJan[,1:2]
NMDS_DecJan<- metaMDS(data1_DecJan, distance = "bray", k=2)

fit2_DecJan <- adonis2 (data1_DecJan~Month, data = data2_DecJan, permutations = 9999,
                        method = "bray")
fit2_DecJan





pvalues_ant <-c(
  fit2_DecApr$`Pr(>F)`,
  fit2_JanMar$`Pr(>F)`,
  fit2_MarApr$`Pr(>F)`,
  fit2_FebMAr$`Pr(>F)`,
  fit2_FebJan$`Pr(>F)`,
  fit2_FebApr$`Pr(>F)`,
  fit2_JanApr$`Pr(>F)`,
  fit2_DecMar$`Pr(>F)`,
  fit2_DecFeb$`Pr(>F)`,
  fit2_DecJan$`Pr(>F)`)


F_statistics <-c(
  fit2_DecApr$F ,
  fit2_JanMar$F,
  fit2_MarApr$F,
  fit2_FebMAr$F,
  fit2_FebJan$F,
  fit2_FebApr$F,
  fit2_JanApr$F,
  fit2_DecMar$F,
  fit2_DecFeb$F,
  fit2_DecJan$F)

groups_ant <-c(
  "fit2_DecApr$`Pr(>F)`",
  "fit2_JanMar$`Pr(>F)`",
  "fit2_MarApr$`Pr(>F)`",
  "fit2_FebMAr$`Pr(>F)`",
  "fit2_FebJan$`Pr(>F)`",
  "fit2_FebApr$`Pr(>F)`",
  "fit2_JanApr$`Pr(>F)`",
  "fit2_DecMar$`Pr(>F)`",
  "fit2_DecFeb$`Pr(>F)`",
  "fit2_DecJan$`Pr(>F)`")

pvalues_ant <- na.omit(pvalues_ant)
adjusted_pvalue_ant <- na.omit(p.adjust(pvalues_ant,method="fdr"))
F_statistics<-na.omit(F_statistics)

adjusted_pvalue_ant_df <- data.frame(groups= groups_ant,
                                     F_statistics= F_statistics,
                                     pvalues_ant = pvalues_ant,
                                     adjusted_pvalue_ant = adjusted_pvalue_ant
)
adjusted_pvalue_ant_df
###################################################################################

anttt <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\anttttttttttttt.csv")
view(anttt)
dim(anttt)
#subset_plantNMDS <- PlantNMDS %>% filter(Samples == "Sample1")
#subset_plantNMDS <- PlantNMDS[PlantNMDS$Samples %in% c("Sampling1", "Sampling5"), ]
ant_data1 <- anttt[,3:14]
ant_data2 <- anttt[,1:2]
ant_NMDS <- metaMDS(ant_data1, distance = "bray", k=2)
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
