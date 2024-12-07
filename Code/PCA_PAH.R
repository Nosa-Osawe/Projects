library(tidyverse)
library(readxl)
library(factoextra)
library(FactoMineR)

PhD.pah <- read_xlsx("C:\\Users\\DELL\\Desktop\\PhD_PAH.xlsx", sheet = 'Sheet1')

PhD.pah$LGA <- factor(PhD.pah$LGA, levels= c("1","2","3","4","5"),
                      labels= c("Ikpoba Ohka", 
                                "Egor",
                                "Uhunmwonde",
                                "Ovia North-East",
                                "Oredo"))

PhD.pah$Season<-factor(PhD.pah$Season, labels = c("Dry","Wet"))
  
pah_outcome <-PhD.pah[,c("BaP","DBahA")]

mt.pah <- PhD.pah[,c(1,4,21:26)]
colnames(mt.pah)

pah.mt <-mt.pah %>% 
  select(-LGA,-Season)

mt_lga_season <-mt.pah %>% 
  select(LGA,Season) %>% 
  as.data.frame()

pca.mt <- PCA(pah.mt, graph = FALSE)

eigen.mt<- get_eigenvalue(pca.mt)

fviz_eig(pca.mt, 
         addlabels = TRUE, 
         ylim = c(0, 50)) # scree plot


fviz_pca_var(pca.mt, col.var = "black", repel = TRUE)

fviz_contrib(pca.mt, choice = "var", axes = 1)
fviz_contrib(pca.mt, choice = "var", axes = 2)

pca.mt$ind$coord

coord_pca.mt <- as.data.frame(pca.mt$ind$coord)
coord_coord_pca.mt_12<- as.data.frame(coord_pca.mt[,1:2]) %>% 
  rename("PC1"="Dim.1",
         "PC2"="Dim.2")

ind_pca_mt<- cbind(mt_lga_season,coord_coord_pca.mt_12,pah_outcome) %>% 
  as.data.frame()
head(ind_pca_mt)

# mt vizzzzz


mt_LGA_Bap <- ggplot() +
  geom_point(data = ind_pca_mt, 
             aes(x = PC1, y = PC2, 
                 color = LGA, , 
                 size = BaP)) + 
  stat_ellipse(data = ind_pca_mt,  
               geom = "polygon", 
               aes(x = PC1, y = PC2, group = LGA, fill = LGA),
               level = 0.95, 
               alpha = 0.09,
               show.legend = NA)+ 
  
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  scale_fill_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (53.1%)", y = "PC2 (21.1%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
mt_LGA_Bap

mt_LGA_DBahA <- ggplot() +
  geom_point(data = ind_pca_mt, 
             aes(x = PC1, y = PC2, 
                 color = LGA, , 
                 size = DBahA)) + 
  stat_ellipse(data = ind_pca_mt,  
               geom = "polygon", 
               aes(x = PC1, y = PC2, group = LGA, fill = LGA),
               level = 0.95, 
               alpha = 0.09,
               show.legend = NA)+ 
  
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  scale_fill_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (53.1%)", y = "PC2 (21.1%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
mt_LGA_DBahA


#########################################################################################


PAH_Data <- PhD.pah[,5:20]
head(PAH_Data)



pca.PAH <- PCA(PAH_Data, graph = FALSE)

eigen.PAH<- get_eigenvalue(pca.PAH)

fviz_eig(pca.PAH, 
         addlabels = TRUE, 
         ylim = c(0, 50)) # scree plot


fviz_pca_var(pca.PAH, col.var = "black", repel = TRUE)

fviz_contrib(pca.PAH, choice = "var", axes = 1)
fviz_contrib(pca.PAH, choice = "var", axes = 2)

pca.PAH$ind$coord

coord_pca.PAH <- as.data.frame(pca.PAH$ind$coord)
coord_coord_pca.PAH_12<- as.data.frame(coord_pca.PAH[,1:2]) %>% 
  rename("PC1"="Dim.1",
         "PC2"="Dim.2")

ind_pca_PAH<- cbind(mt_lga_season,coord_coord_pca.PAH_12,pah_outcome) %>% 
  as.data.frame()
head(ind_pca_PAH)


#######################################################################################



PAH_LGA_Bap <- ggplot() +
  geom_point(data = ind_pca_PAH, 
             aes(x = PC1, y = PC2, 
                 color = LGA, , 
                 size = BaP)) + 
  
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  scale_fill_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (44.4%)", y = "PC2 (11.2%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
PAH_LGA_Bap





PAH_LGA_DBahA <- ggplot() +
  geom_point(data = ind_pca_PAH, 
             aes(x = PC1, y = PC2, 
                 color = LGA, , 
                 size = DBahA)) + 
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  scale_fill_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  )+ 
  labs(x = "PC1 (44.4%)", y = "PC2 (11.2%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
PAH_LGA_DBahA


PAH_Season_Bap <- ggplot() +
  geom_point(data = ind_pca_PAH, 
             aes(x = PC1, y = PC2, 
                 color = Season, , 
                 size = BaP)) + 
  stat_ellipse(data = ind_pca_PAH, 
               aes(x = PC1, y = PC2, 
                   group = Season, 
                   color = Season), 
               geom = "path", 
               level = 0.85, 
               linewidth = 0.3,  # Adjust this value to make the line bold
               show.legend = NA) +
  
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  scale_fill_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (44.4%)", y = "PC2 (11.2%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
PAH_Season_Bap

PAH_Season_DBahA <- ggplot() +
  geom_point(data = ind_pca_PAH, 
             aes(x = PC1, y = PC2, 
                 color = Season, , 
                 size = DBahA)) + 
  stat_ellipse(data = ind_pca_PAH, 
               aes(x = PC1, y = PC2, 
                   group = Season, 
                   color = Season), 
               geom = "path", 
               level = 0.85, 
               linewidth = 0.3,  # Adjust this value to make the line bold
               show.legend = NA) +
  
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  scale_fill_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (44.4%)", y = "PC2 (11.2%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
PAH_Season_DBahA
