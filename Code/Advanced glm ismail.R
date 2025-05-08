#library(lme4)
#library(lmerTest)
library(tidyverse)
library(MASS)
#library(ggfortify)
library(corrplot)  # used to load a corr matrix!!!!
library(multcomp)
library(emmeans)
#library(car)
# in addition: 
require(performance)
library(effectsize)  # (partial) variane expdlained



length(colnames(omo_Data))  
omo<- omo_Data[,1:24]    

omo <- omo_Data %>% 
  select(1:24)  
# view(omo)

length(omo)



omo_clean <- omo %>% 
  dplyr::select(-c(1:3), -Anopheles, -Aedes, -Cules) %>% 
  as.data.frame()

omo_cor <- cor(omo_clean)


# make the correlation matrix
corrplot(omo_cor, method = c("color"),  
         addCoef.col='black', 
         number.cex = 0.7,  tl.cex = 0.8, tl.col = 'black', 
         type= "upper")


lm1 <- lm(Total.Solid~Suspended.Solid, data = omo_clean)    
summary(lm1)       

lm1.2 <- lm(Total.Solid~DO , data = omo_clean) 
summary(lm1.2)

lm2 <- lm(Total.Solid~Suspended.Solid+ DO , data = omo_clean)  
summary(lm2)

lm3 <- lm(Total.Solid~Depth , data = omo_clean) 
summary(lm3)
         
model_performance(lm1)
model_performance(lm1.2)


check_normality(lm1)
check_model(lm1)


check_model(lm3)

plot(Total.Solid~Depth)

#######################################################################################
attach(omo_clean)

lmp1 <- lm(Phosphate~Colour, data = omo_clean)
summary(lmp1)
check_model(lmp1)

# Favour foward selection
lmp1 <- lm(Phosphate~Colour, data = omo_clean)
summary(lmp1)
check_model(lmp1)


lmp2 <- lm(Phosphate~ Colour+Nitrate, data = omo_clean)
summary(lmp2)
check_model(lmp2)

model_performance(lmp1)
model_performance(lmp2)

anova(lmp1, lmp2)


# we go with the simpler model: lmp1

lmp3 <- lm(Phosphate~ Colour * Chloride, data = omo_clean)
summary(lmp3)
check_model(lmp3)

eta_squared(lmp3)

omega_squared(lmp3)


#####################################################################################


lmp3 <- lm(Phosphate~ Colour * Chloride, data = omo_clean)
summary(lmp3)


scale(omo_clean$Colour)
hist(omo_clean$Colour)
hist(scale(omo_clean$Colour),  breaks = 30)

round(mean(scale(omo_clean$Colour)), 2)
sd(scale(omo_clean$Colour))

mean(scale(omo_clean$Chloride))

omo_clean %>% 
  dplyr::select(Colour, Chloride)

lmp4 <- lm(Phosphate~ scale (Colour) * scale(Chloride), data = omo_clean)
summary(lmp4)



eta_squared(lmp4)

omega_squared(lmp4)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


ismail_n <- function(y) {
  return((y - min(y))/ (max(y) - min(y)))
}

str(ismail_n)

hist(ismail_n(omo_clean$Colour), breaks = 15)



#################################################################################


#   PCA

omo_physicochemical <- omo[,4:20]
Location <- omo[,"Location"]

omo_physic_PCA <- cbind(Location, omo_physicochemical)
head(omo_physic_PCA) # well done

# rownames(omo_physic_PCA) <- omo_physic_PCA$Location

omo_physic_PCA_active <- omo_physic_PCA[,-1]
omo.pca <- PCA(omo_physic_PCA_active, graph = TRUE)

eig.val_omo <- get_eigenvalue(omo.pca)
eig.val_omo

fviz_eig(omo.pca, addlabels = TRUE, ylim = c(0, 50)) # scree plot

fviz_pca_var(omo.pca, col.var = "black", repel = TRUE)

fviz_pca_var(omo.pca, col.var = "cos2",
             gradient.cols = c("red", "#E7B800", "darkgreen"),
             repel = TRUE # Avoid text overlapping
)

fviz_pca_ind(omo.pca, col.ind = "cos2", pointsize = "cos2",
             pointshape = 21, fill = "#E7B800", 
             repel = TRUE) # Avoid text overlapping (slow if many points)

fviz_pca_var(omo.pca, col.var = "contrib",
             gradient.cols = c("red", "#E7B800", "darkgreen"),
             repel = TRUE # Avoid text overlapping
)


fviz_pca_var(omo.pca, col.var = "cos2",
             gradient.cols = c("red", "#E7B800", "darkgreen"),
             repel = TRUE # Avoid text overlapping
)

omo.pca$var$contrib

omo.pca$ind$contrib
omo.pca$ind$dist
fviz_contrib(omo.pca, choice = "ind", axes = 1)
fviz_contrib(omo.pca, choice = "ind", axes = 2)

fviz_contrib(omo.pca, choice = "var", axes = 1)
fviz_contrib(omo.pca, choice = "var", axes = 2)

fviz_contrib(omo.pca, choice = "var", axes = 2)

omo.pca$ind$cos2
omo.pca$ind$cos2

coordinate_ind <- as.data.frame(omo.pca$ind$coord)
coordinate_ind12<- coordinate_ind[,1:2]   

coordinate_ind123 <- as.data.frame(cbind((omo[,1:3]),
                                         coordinate_ind12))
head(coordinate_ind123)

quality_of_rep_ind <- as.data.frame(omo.pca$ind$cos2)
quality_of_rep_ind12<- quality_of_rep_ind[,1:2]   
quality_of_rep_ind12<- quality_of_rep_ind12 %>% 
  rename(CO2.Dim.1 = Dim.1, 
         CO2.Dim.2 = Dim.2)
ind_viz_data <- cbind(coordinate_ind123, quality_of_rep_ind12)
head(ind_viz_data)


co2_var_dataframe12 <-  as.data.frame(omo.pca$var$coord[,1:2])

co2_var<- cbind(row.names(co2_var_dataframe12),co2_var_dataframe12) 
row.names(co2_var)  <- NULL     
head(co2_var) 

co2_var <- co2_var %>% 
  rename(parameter = "row.names(co2_var_dataframe12)",
         PCA1 = Dim.1, 
         PCA2 = Dim.2)
head(co2_var) 
head(co2_ind_viz_data)
co2_var<- as.data.frame(co2_var)
omo.pca$var$cos2
omo.pca$var$coord
omo.pca$ind$contrib
omo.pca$var$contrib
length(omo$Location)





