rm(list = ls()) # clear environment
cat("\014") # clear console
library(tidyverse)
library(vegan)
library(corrplot)

ant.R.A <- read.csv("C:\\Users\\user\\Desktop\\R.A ant.csv")
view(ant.R.A)
dim(ant.R.A)

antdata1RA <- ant.R.A[,3:17]
antdata2RA <- ant.R.A[,1:2]
antNMDSRA <- metaMDS(antdata1RA, distance = "bray", k=3)
antNMDSRA$stress

antNMDSRA$points
antNMDS_df_RA <- as.data.frame(antNMDSRA$points)

antfit2_RA <- adonis2 (antNMDS_df_RA~Month, data = antdata2RA, permutations = 9999, method = "bray")
antfit2_RA


# Define a mapping of Month values to shape codes
shape_mapping <- c(
  "December" = 10,  # Use shape code 1 for December
  "January" = 2,   # Use shape code 2 for January
  "February" = 1,  # Use shape code 3 for February
  "March" = 4,     # Use shape code 4 for March
  "April" = 5      # Use shape code 5 for April
)

# Your data and plot code
ggplot(ant.R.A, aes(x = antNMDS_df_RA$MDS1, y = antNMDS_df_RA$MDS2, shape = Month, color = Month)) +
  geom_point(size = 3, ) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_color_manual(values = c("December" = "yellow","January" = "orange", 
                                "February" = "#353535", "March" = "#A0662C",
                                "April" = "red")) +  # Specify colors
  stat_ellipse(geom = "polygon", aes(group = Month), 
               level = 0.95,
               size = 1.3,
               fill = "transparent")+
  theme(
    text = element_text(family = "Times New Roman", size = 20)) +  
  theme_classic() +
  scale_shape_manual(values = shape_mapping)


DecJan <-ant.R.A %>% 
  filter(Month=="December" | Month== "January")

data1_DecJan <- DecJan[,3:17]
data2_DecJan <- DecJan[,1:2]
NMDS_DecJan<- metaMDS(data1_DecJan, distance = "bray", k=3)

fit2_DecJan <- adonis2 (data1_DecJan~Month, data = data2_DecJan, permutations = 9999,
                        method = "bray")
fit2_DecJan

DecApr <-ant.R.A %>% 
  filter(Month=="December" | Month== "April")

data1_DecApr <- DecApr[,3:17]
data2_DecApr <- DecApr[,1:2]
NMDS_DecApr<- metaMDS(data1_DecApr, distance = "bray", k=3)

fit2_DecApr <- adonis2 (data1_DecApr~Month, data = data2_DecApr, permutations = 9999,
                        method = "bray")
fit2_DecApr




JanMar <-ant.R.A %>% 
  filter(Month=="March" | Month== "January")

data1_JanMar <- JanMar[,3:17]
data2_JanMar <- JanMar[,1:2]
NMDS_JanMar<- metaMDS(data1_JanMar, distance = "bray", k=3)

fit2_JanMar <- adonis2 (data1_JanMar~Month, data = data2_JanMar, permutations = 9999,
                        method = "bray")
fit2_JanMar


MarApr <-ant.R.A %>% 
  filter(Month=="March" | Month== "April")

data1_MarApr <- MarApr[,3:17]
data2_MarApr <- MarApr[,1:2]
NMDS_MarApr<- metaMDS(data1_MarApr, distance = "bray", k=3)

fit2_MarApr <- adonis2 (data1_MarApr~Month, data = data2_MarApr, permutations = 9999,
                        method = "bray")
fit2_MarApr


FebMAr <-ant.R.A %>% 
  filter(Month=="February" | Month== "March")

data1_FebMAr <- FebMAr[,3:17]
data2_FebMAr <- FebMAr[,1:2]
NMDS_FebMAr<- metaMDS(data1_FebMAr, distance = "bray", k=3)

fit2_FebMAr <- adonis2 (data1_FebMAr~Month, data = data2_FebMAr, permutations = 9999,
                        method = "bray")
fit2_FebMAr


FebJan <-ant.R.A %>% 
  filter(Month=="February" | Month== "January")

data1_FebJan <- FebJan[,3:17]
data2_FebJan <- FebJan[,1:2]
NMDS_FebJan<- metaMDS(data1_FebJan, distance = "bray", k=3)

fit2_FebJan <- adonis2 (data1_FebJan~Month, data = data2_FebJan, permutations = 9999,
                        method = "bray")
fit2_FebJan

FebApr <-ant.R.A %>% 
  filter(Month=="February" | Month== "April")

data1_FebApr <- FebApr[,3:17]
data2_FebApr <- FebApr[,1:2]
NMDS_FebApr<- metaMDS(data1_FebApr, distance = "bray", k=3)

fit2_FebApr <- adonis2 (data1_FebApr~Month, data = data2_FebApr, permutations = 9999,
                        method = "bray")
fit2_FebApr


JanApr <-ant.R.A %>% 
  filter(Month=="January" | Month== "April")

data1_JanApr <- JanApr[,3:17]
data2_JanApr <- JanApr[,1:2]
NMDS_JanApr<- metaMDS(data1_JanApr, distance = "bray", k=3)

fit2_JanApr <- adonis2 (data1_JanApr~Month, data = data2_JanApr, permutations = 9999,
                        method = "bray")
fit2_JanApr



DecFeb <-ant.R.A %>% 
  filter(Month=="December" | Month== "February")

data1_DecFeb <- DecFeb[,3:17]
data2_DecFeb <- DecFeb[,1:2]
NMDS_DecFeb<- metaMDS(data1_DecFeb, distance = "bray", k=3)

fit2_DecFeb <- adonis2 (data1_DecFeb~Month, data = data2_DecFeb, permutations = 9999,
                        method = "bray")
fit2_DecFeb


DecMar <-ant.R.A %>% 
  filter(Month=="December" | Month== "March")

data1_DecMar <- DecMar[,3:17]
data2_DecMar <- DecMar[,1:2]
NMDS_DecMar<- metaMDS(data1_DecMar, distance = "bray", k=3)

fit2_DecMar <- adonis2 (data1_DecMar~Month, data = data2_DecMar, permutations = 9999,
                        method = "bray")
fit2_DecMar

DecJan <-ant.R.A %>% 
  filter(Month=="December" | Month== "January")
data1_DecJan <- DecJan[,3:17]
data2_DecJan <- DecJan[,1:2]
NMDS_DecJan<- metaMDS(data1_DecJan, distance = "bray", k=3)

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
adjusted_pvalue_ant <- na.omit(p.adjust(pvalues_ant,method="bonferroni"))
view(adjusted_pvalue_ant)

adjusted_pvalue_ant_df <- data.frame(groups= groups_ant,
  pvalues_ant = pvalues_ant,
  adjusted_pvalue_ant = adjusted_pvalue_ant
)

