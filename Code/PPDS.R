library(tidyverse)
library(agricolae)


ppds <- read.csv("C:\\Users\\DELL\\Desktop\\PPDS\\working_data_csv.csv")
view(ppds)


unique(ppds$Day)
unique(ppds$Group)

str(ppds)
attach(ppds)

anova <- aov(leucocyte~ Group*Day, data = ppds)
summary(anova)

TukeyHSD(anova)

anova2 <- aov(Platelet~ Group*Day, data = ppds)
summary(anova2)

HSD.test(anova2, trt = c("Group", "Day"), group = TRUE)$groups


egg <- read.csv("C:\\Users\\DELL\\Desktop\\PPDS\\egg_data_csv.csv")
str(egg)
unique(egg$Group)
unique(egg$Day)
view(egg)
attach(egg)

eggnova <- aov(leucocyte~ Group*Day, data = egg)
summary(eggnova)
TukeyHSD(eggnova)

eggnova2 <- aov(Platelet~ Group*Day, data = egg)
summary(eggnova2)
TukeyHSD(eggnova2)







eggnova2 <- aov(Platelet ~ Group * Day, data = egg)
summary(eggnova2)

tukey_hsd <- HSD.test(eggnova2, trt = c("Group", "Day"), group = TRUE)
print(tukey_hsd$groups)







#############################################################################################


car_data <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\car_data.csv", 
                     stringsAsFactors = TRUE)

view(car_data)

car_data<-car_data %>% 
  select(-X,-X.1,-X.2)


hist(car_data$Price)
boxplot(car_data$Price)

shapiro.test(car_data$Price)
