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

Capsule.poisson <- lm(Capsule~Time, data = lens )
summary(Capsule.poisson)


Epithelium.poisson <- lm(Epithelium~Time, data = lens )
summary(Epithelium.poisson)

Fibres.poisson <- lm(Fibres~Time, data = lens )
summary(Fibres.poisson)

Compactness.poisson <- lm(Compactness~Time, data = lens )
summary(Compactness.poisson)

Orientation.poisson <- lm(Orientation~Time, data = lens )
summary(Orientation.poisson)

Cell_shape_size.poisson <- lm(Cell_shape_size~Time, data = lens )
summary(Cell_shape_size.poisson)

Nuclear_staining.poisson <- lm(Nuclear_staining~Time, data = lens )
summary(Nuclear_staining.poisson)


lensT <- lens %>% 
  pivot_longer(
    cols = -Time,
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  as.data.frame()

ggplot(lensT, aes(x = Time, y = Value)) +
  geom_line() +
  facet_wrap(~ Category, scales = "free_y") +  
  labs(x= "Time (Hrs)",
       y= "Scale (Grade)")+
  theme_minimal()
  


######################################################################################

retina <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\lens_table.xlsx",
                             sheet = "Sheet3")
str(retina)
colnames(retina)

Autolysis<- lm(Autolysis~Time, data = retina )
summary(Autolysis)     

ONL_Pyknosis<- lm(ONL_Pyknosis~Time, data = retina )
summary(ONL_Pyknosis)   

INL_Pyknosis<- lm(INL_Pyknosis~Time, data = retina )
summary(INL_Pyknosis)  

Pyknosis_of_Ganglion<- lm(Pyknosis_of_Ganglion~Time, data = retina )
summary(Pyknosis_of_Ganglion)  

Photoreceptor_homo<- lm(Photoreceptor_homo~Time, data = retina )
summary(Photoreceptor_homo)  

Retinal_seperation<- lm(Retinal_seperation~Time, data = retina )
summary(Retinal_seperation)  

OPL_homo<- lm(OPL_homo~Time, data = retina )
summary(OPL_homo)  

IPL_Homo<- lm(IPL_Homo~Time, data = retina)
summary(IPL_Homo)

Retina_thickness_Reduction <- lm(Retina_thickness_Reduction~Time, 
                                  data = retina)
summary(Retina_thickness_Reduction)  



retina %>% 
  rename(
    "Cell autolysis"="Autolysis",
    "Pyknosis of the ONL"= "ONL_Pyknosis",
    "Pyknosis of the INL"="INL_Pyknosis",
    "Pyknosis of ganglion cell layer"="Pyknosis_of_Ganglion",
    "Homogenization of the photoreceptor layer"="Photoreceptor_homo",
    "Separation of retinal layers (Neuroretina from RPE)"="Retinal_seperation",
    "Homogeniszation of OPL"="OPL_homo",
    "Homogenization of IPL"="IPL_Homo",
    "Reduction in retina thickness"="Retina_thickness_Reduction"
  ) %>% 
  pivot_longer(
    cols = -Time,
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  as.data.frame() %>% 
ggplot(aes(x = Time, y = Value, colour = "red")) +
  geom_line() +
  facet_wrap(~ Category, ncol=2, scales = "free_y") +  
  labs(x= "PMI (Hours)",
       y= "Scale (Grade)")+
  scale_x_continuous(n.breaks= 12, breaks = c(0,12,24,48,72)) +
  guides(colour = 'none')+
  theme_minimal()


#####################################################################################

retina2 <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\lens_table.xlsx",
                     sheet = "Sheet2")


Autolysis<- lm(Autolysis~Time, data = retina2 )
summary(Autolysis)     

ONL_Pyknosis<- lm(ONL_Pyknosis~Time, data = retina2 )
summary(ONL_Pyknosis)   

INL_Pyknosis<- lm(INL_Pyknosis~Time, data = retina2 )
summary(INL_Pyknosis)  

Pyknosis_of_Ganglion<- lm(Pyknosis_of_Ganglion~Time, data = retina2 )
summary(Pyknosis_of_Ganglion)  

Photoreceptor_homo<- lm(Photoreceptor_homo~Time, data = retina2 )
summary(Photoreceptor_homo)  

Retinal_seperation<- lm(Retinal_seperation~Time, data = retina2 )
summary(Retinal_seperation)  

OPL_homo<- lm(OPL_homo~Time, data = retina2 )
summary(OPL_homo)  

IPL_Homo<- lm(IPL_Homo~Time, data = retina2)
summary(IPL_Homo)

Retina_thickness_Reduction <- lm(Retina_thickness_Reduction~Time, 
                                 data = retina2)
summary(Retina_thickness_Reduction)  


retina2 %>% 
  rename(
    "Cell autolysis"="Autolysis",
    "Pyknosis of the ONL"= "ONL_Pyknosis",
    "Pyknosis of the INL"="INL_Pyknosis",
    "Pyknosis of ganglion cell layer"="Pyknosis_of_Ganglion",
    "Homogenization of the photoreceptor layer"="Photoreceptor_homo",
    "Separation of retinal layers (Neuroretina from RPE)"="Retinal_seperation",
    "Homogeniszation of OPL"="OPL_homo",
    "Homogenization of IPL"="IPL_Homo",
    "Reduction in retina thickness"="Retina_thickness_Reduction"
  ) %>% 
  pivot_longer(
    cols = -Time,
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Time, y = Value, )) +
  geom_line(linewidth =0.5) +
  facet_wrap(~ Category, ncol=2, scales = "free_y") +  
  labs(x= "PMI (Hours)",
       y= "Scale (Grade)")+
  scale_x_continuous(n.breaks= 12, breaks = c(0,12,24,48,72)) +
  guides(colour = 'none')+
  theme_minimal()
