library(factoextra)
library(FactoMineR)

outcome_var <- PAH[,10:11]
PAH_PCA <- PAH[,4:9]
LGA_month_season <- PAH[,1:3]

PAH_LGA <- cbind(LGA, PAH_PCA)
head(PAH_LGA)

pca.pah <- PCA(PAH_PCA, graph = FALSE)

eigen.pah <- get_eigenvalue(pca.pah)

fviz_eig(pca.pah, addlabels = TRUE, ylim = c(0, 50)) # scree plot


fviz_pca_var(pca.pah, col.var = "black", repel = TRUE)

fviz_contrib(pca.pah, choice = "var", axes = 1)
fviz_contrib(pca.pah, choice = "var", axes = 2)

pca.pah$ind$coord

coord_pca_ah <- as.data.frame(pca.pah$ind$coord)
coord_pca_12<- as.data.frame(coord_pca_ah[,1:2]) %>% 
  rename("PC1"="Dim.1",
         "PC2"="Dim.2")
  
ind_pca_pah<- cbind(LGA_month_season,coord_pca_12,outcome_var) %>% 
  as.data.frame()

# vizzzzzz!!!

final_PCA_PAH_LGA <- ggplot() +
  geom_point(data = ind_pca_pah, 
             aes(x = PC1, y = PC2, 
                 color = L.G.A, , 
                size = BaP)) + 
  
  scale_fill_manual(values = c("IKPOBA-OKHA" = "orange",
                               "EGOR"  = "red",
                               "UHUNMWONDE"  = "black",
                               "OVIA N.E."= "brown",
                               "OREDO"   = "darkgreen")) +
  
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (53.1%)", y = "Dim2 (21.1%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
final_PCA_PAH_LGA

final_PCA_PAH_season <- ggplot() +
  geom_point(data = ind_pca_pah, 
             aes(x = PC1, y = PC2, 
                 color = Season, , 
                 size = BaP)) + 

  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "PC1 (53.1%)", y = "PC2 (21.1%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")

final_PCA_PAH_season

