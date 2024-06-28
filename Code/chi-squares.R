library(tidyverse)
#-----------------------------------------------------------------
Ciprofloxacin <- read.csv("C:\\Users\\DELL\\Desktop\\chi_sq.csv")

row.names(Ciprofloxacin) <- Ciprofloxacin$Resistance

# Remove the first column (if needed)
Ciprofloxacin <- Ciprofloxacin[, -1]  

view(Ciprofloxacin)
chisq.test(Ciprofloxacin)

#-----------------------------------------------------------------
Augmentin <- read.csv("C:\\Users\\DELL\\Desktop\\chi_sq.csv")

row.names(Augmentin) <- Augmentin$Resistance

# Remove the first column (if needed)
Augmentin <- Augmentin[, -1]  

view(Augmentin)
chisq.test(Augmentin)

#-----------------------------------------------------------------
Imipenem <- read.csv("C:\\Users\\DELL\\Desktop\\chi_sq.csv")

row.names(Imipenem) <- Imipenem$Resistance

# Remove the first column (if needed)
Imipenem <- Imipenem[, -1]  

chisq.test(Imipenem)
#-----------------------------------------------------------------

Cefepime <- read.csv("C:\\Users\\DELL\\Desktop\\chi_sq.csv")

row.names(Cefepime) <- Cefepime$Resistance

# Remove the first column (if needed)
Cefepime <- Cefepime[, -1]  

chisq.test(Cefepime)


#-----------------------------------------------------------------

antibiotics <- read.csv("C:\\Users\\DELL\\Desktop\\Eloho\\Ebuka\\CA_antibiotics.csv")
rownames(antibiotics) <- antibiotics$Age
antibiotics <- antibiotics[,-1]

install.packages("factoextra")
install.packages("FactoMineR")


res.ca.antibiotics <- CA(antibiotics, graph = FALSE)
summary(res.ca.antibiotics)

eig_ <- get_eigenvalue(res.ca.antibiotics)

fviz_screeplot(res.ca.antibiotics, addlabels = TRUE, ylim = c(0, 50)) ## 100


biplot_anti <-fviz_ca_biplot(res.ca.antibiotics, alpha.col = 0.5,
                             map ="colgreen", arrow = c(FALSE, FALSE),
                             repel = TRUE,
                             col.col = "blue", 
                             col.row = "black",
                             pointsize = 5, size.text = 2)+
  theme_classic()
biplot_anti

mosq.desc <- dimdesc(res.ca.mosq, axes = 1:2)
mosq.desc

