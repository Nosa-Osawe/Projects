library(tidyverse)
library(readxl)

lens <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\lens_table.xlsx",
                   sheet = "Sheet1")

lens <- lens %>% 
  rename(Time="Time" ,
         Capsule="Capsule",
         Epithelium= "Epithelium",
         Fibres= "Fibres",
         Compactness= "Compactness",
         Orientation="Orientation",
         Cell_shape_size="Cell Shape & Size",
         Nuclear_staining="Nuclear Staining") %>% 
  as.data.frame()

Capsule.poisson <- glm(Capsule~Time, data = lens, family = poisson(link = "log"))
summary(Capsule.poisson)
confint(Capsule.poisson)

Epithelium.poisson <- glm(Epithelium~Time, data = lens, family = poisson(link = "log"))
summary(Epithelium.poisson)

Fibres.poisson <- glm(Fibres~Time, data = lens, family = poisson(link = "log"))
summary(Fibres.poisson)

Compactness.poisson <- glm(Compactness~Time, data = lens, family = poisson(link = "log"))
summary(Compactness.poisson)

Orientation.poisson <- glm(Orientation~Time, data = lens, family = poisson(link = "log"))
summary(Orientation.poisson)

Cell_shape_size.poisson <- glm(Cell_shape_size~Time, data = lens, family = poisson(link = "log"))
summary(Cell_shape_size.poisson)

Nuclear_staining.poisson <- glm(Nuclear_staining~Time, data = lens, family = poisson(link = "log"))
summary(Nuclear_staining.poisson)
