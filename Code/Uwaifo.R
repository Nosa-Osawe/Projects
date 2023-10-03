library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)
library(tidyverse)

uwaifo <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\UWAIFO_not_standardized.csv")
colnames(uwaifo)
head(uwaifo)

row.names(uwaifo) <- uwaifo[,1]
uwaifo <- uwaifo[,-1]
View(uwaifo)


res.uwaifo <- MFA(uwaifo,
               group = c(1,1, 20, 6),
               type = c("n", "n", "s", "s"),
               name.group = c("month","station","metals","anti-biotics"),
               graph = FALSE) # n= categorical variable (no standardization)
# s= standardize the continuous variables
res.uwaifo

eig.val_uwaifo <- get_eigenvalue(res.uwaifo)
head(eig.val_uwaifo)
fviz_screeplot(res.uwaifo)

group_uwaifo <- get_mfa_var(res.uwaifo, "group")
group_uwaifo

#The different components can be accessed as follow:
# Coordinates of groups
head(group_uwaifo$coord)
# Cos2: quality of representation on the factor map
head(group_uwaifo$cos2)
# Contributions to the dimensions
head(group_uwaifo$contrib)

# plot of variables
# red color = active groups of variables
# green color = supplementary groups of variables
fviz_mfa_var(res.uwaifo, "group", repel = TRUE)

res.uwaifo$quali.var$cos2
res.uwaifo$quali.var$contrib
res.uwaifo$quali.var$coord

# Contribution to the first dimension
fviz_contrib(res.uwaifo, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.uwaifo, "group", axes = 2)
# Contribution to the second dimension
fviz_contrib(res.uwaifo, "group", axes = 3)

# extract results of quantitative variables
quanti.var_uwaifo <- get_mfa_var(res.uwaifo, "quanti.var")
quanti.var_uwaifo

# Coordinates
head(quanti.var_uwaifo$coord)
# Cos2: quality on the factore map
head(quanti.var_uwaifo$cos2)
# Contributions to the dimensions
head(quanti.var_uwaifo$contrib)

fviz_mfa_var(res.uwaifo, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE)

#change also the legend position from "right" to "bottom"
fviz_mfa_var(res.uwaifo, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE,
             geom = c("point", "text"), legend = "bottom")+
  theme_classic()

# Contributions to dimension 1
fviz_contrib(res.uwaifo, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco") # variables are coloured by groups

# Contributions to dimension 2
fviz_contrib(res.uwaifo, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")# Contributions to dimension 2

# Contributions to dimension 3
fviz_contrib(res.uwaifo, choice = "quanti.var", axes = 3, top = 20,
             palette = "jco")# Contributions to dimension 2

# most contributing variables
fviz_mfa_var(res.uwaifo, "quanti.var", col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo, choice = "quanti.var", axes = 1)
# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo, choice = "quanti.var", axes = 2)
# results for individuals
ind <- get_mfa_ind(res.uwaifo)
ind

fviz_mfa_var(res.uwaifo, "quali.var", palette = "jco",
             col.var.sup = FALSE,   repel = TRUE)+
  theme_classic()


#color individuals by their cos2 values
fviz_mfa_ind(res.uwaifo, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quali.var", # make qualitative variables invisible
             repel = TRUE)

fviz_mfa_ind(res.uwaifo,
             habillage = "stations", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE # Avoid text overlapping
)

fviz_mfa_ind(res.uwaifo,
             habillage = "stations", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE,
             geom = c("point")
)




#######################################################################################
uwaifo3 <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\Uwaifo_sediment.csv")
row.names(uwaifo3) <- uwaifo3[,1]
uwaifo3 <- uwaifo3[,-1]
View(uwaifo2)

res.uwaifo3 <- MFA(uwaifo3,
                   group = c(1,1, 8, 6),
                   type = c("n","n", "s", "s"),
                   name.group = c("month","station","metals","anti-biotics"),
                   graph = FALSE) 

eig.val_uwaifo3 <- get_eigenvalue(res.uwaifo3)
head(eig.val_uwaifo3)
fviz_screeplot(res.uwaifo3)

group_uwaifo3 <- get_mfa_var(res.uwaifo3, "group")
group_uwaifo3

fviz_mfa_var(res.uwaifo3, "group")
# Contribution to the first dimension
fviz_contrib(res.uwaifo3, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.uwaifo3, "group", axes = 2)
# Contribution to the second dimension
fviz_contrib(res.uwaifo3, "group", axes = 3)

# Contributions to dimension 1
fviz_contrib(res.uwaifo3, choice = "quali.var", axes = 1, top = 20,
             palette = "jco") 
# Contributions to dimension 1
fviz_contrib(res.uwaifo3, choice = "quali.var", axes = 2, top = 20,
             palette = "jco") 
# Contributions to dimension 1
fviz_contrib(res.uwaifo3, choice = "quali.var", axes = 3, top = 20,
             palette = "jco") 
res.uwaifo3$quali.var$coord
res.uwaifo3$quali.var$contrib
res.uwaifo3$quali.var$cos2

fviz_mfa_var(res.uwaifo3, "quali.var", palette = "jco",
             col.var.sup = FALSE,   repel = TRUE)+
  theme_classic()

#change also the legend position from "right" to "bottom."
fviz_mfa_var(res.uwaifo3, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE,
             geom = c("point", "text"), legend = "bottom")+
  theme_classic()

#color individuals by their cos2 values
fviz_mfa_ind(res.uwaifo3, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quali.var", # make qualitative variables invisible
             repel = TRUE)+
  theme_classic()

fviz_mfa_ind(res.uwaifo3,
             habillage = "Stations", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE,
             geom = c("point", "text"))+
  theme_classic()

#######################################################################################

uwaifo2 <- read.csv(("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\Uwaifo_sediment.csv"))

colnames(uwaifo2)
View(uwaifo2)
row.names(uwaifo2) <- uwaifo2[,1]
uwaifo2 <- uwaifo2[,-1]
View(uwaifo2)

res.uwaifo2 <- MFA(uwaifo2,
                  group = c(2, 8, 6),
                  type = c("n", "s", "s"),
                  name.group = c("origin","metals","anti-biotics"),
                  graph = FALSE) 

eig.val_uwaifo2 <- get_eigenvalue(res.uwaifo2)
head(eig.val_uwaifo2)
fviz_screeplot(res.uwaifo2)

group_uwaifo2 <- get_mfa_var(res.uwaifo2, "group")
group_uwaifo2

#The different components can be accessed as follow:
# Coordinates of groups
head(group_uwaifo2$coord)
# Cos2: quality of representation on the factor map
head(group_uwaifo2$cos2)
# Contributions to the dimensions
head(group_uwaifo2$contrib)

# plot of variables
# red color = active groups of variables
# green color = supplementary groups of variables
fviz_mfa_var(res.uwaifo2, "group")

# Contribution to the first dimension
fviz_contrib(res.uwaifo2, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.uwaifo2, "group", axes = 2)
# Contribution to the second dimension
fviz_contrib(res.uwaifo2, "group", axes = 3)

# extract results of quantitative variables
quanti.var_uwaifo2 <- get_mfa_var(res.uwaifo2, "quanti.var")
quanti.var_uwaifo2

quali.var_uwaifo2 <- get_mfa_var(res.uwaifo2, "quali.var")
quali.var_uwaifo2

# Coordinates
head(quanti.var_uwaifo2$coord)
# Cos2: quality on the factore map
head(quanti.var_uwaifo2$cos2)
# Contributions to the dimensions
head(quanti.var_uwaifo2$contrib)

fviz_mfa_var(res.uwaifo2, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE)+
  theme_classic()

fviz_mfa_var(res.uwaifo2, "quali.var", palette = "jco",
             col.var.sup = FALSE,   repel = TRUE)+
  theme_classic()

#change also the legend position from "right" to "bottom"
fviz_mfa_var(res.uwaifo2, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

#change also the legend position from "right" to "bottom"
fviz_mfa_var(res.uwaifo2, "quali.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1
fviz_contrib(res.uwaifo2, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco") # variables are coloured by groups

# Contributions to dimension 1
fviz_contrib(res.uwaifo2, choice = "quali.var", axes = 1, top = 20,
             palette = "jco") # variables are coloured by groups


# Contributions to dimension 2
fviz_contrib(res.uwaifo2, choice = "quali.var", axes = 2, top = 20,
             palette = "jco") # variables are coloured by groups

# Contributions to dimension 3
fviz_contrib(res.uwaifo2, choice = "quali.var", axes = 3, top = 20,
             palette = "jco") 

# Contributions to dimension 4
fviz_contrib(res.uwaifo2, choice = "quali.var", axes = 4, top = 20,
             palette = "jco") 

# Contributions to dimension 2
fviz_contrib(res.uwaifo2, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")# Contributions to dimension 2

# Contributions to dimension 3
fviz_contrib(res.uwaifo2, choice = "quanti.var", axes = 3, top = 20,
             palette = "jco")# Contributions to dimension 2

# most contributing variables
fviz_mfa_var(res.uwaifo2, "quanti.var", col.var = "contrib",
             gradient.cols = c("blue", "#FC4E07", "green"),
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo2, choice = "quanti.var", axes = 1)

# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo2, choice = "quanti.var", axes = 2)

# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo2, choice = "quanti.var", axes = 3)
# results for individuals
ind <- get_mfa_ind(res.uwaifo2)
ind

#color individuals by their cos2 values
fviz_mfa_ind(res.uwaifo2, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quali.var", # make qualitative variables invisible
             repel = TRUE)+
  theme_classic()


fviz_mfa_ind(res.uwaifo2,
             habillage = "Stations", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE,
             geom = c("point"))+
  theme_classic()


fviz_mfa_ind(res.uwaifo2,
             habillage = "Stations", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE)
  theme_classic()




