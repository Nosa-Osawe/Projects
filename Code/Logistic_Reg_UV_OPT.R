library(tidyverse)
library(readxl)

uv <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Copy of Ella data analysis(1).xlsx",
             sheet = 'workingSheet')

view(uv)
colnames(uv)

uv_demo <- as.data.frame(uv[,c(1:5,27,33)])

str(uv_demo)
uv_demo <- uv_demo %>% 
  mutate(Gender= factor(Gender, levels = c("Female", "Male")),
         Age= factor(Age, levels = c("22-24", "19-21", "16-18", "25-27", "28 30")),
         Knowledge.2= factor(Knowledge.2, levels = c("Poor", "Good")),
         Knowledge.1=factor(Knowledge.1, levels = c("Poor", "Good")),
         Department= ifelse(Department=="OPTOMETRY", "Optometry", "Others"),
         Faculty= factor(Faculty))
str(uv_demo)

MLR1 <-glm(Knowledge.1~ .-Knowledge.2, data = uv_demo, family = binomial)
summary(MLR1)
as.data.frame(exp(coef(MLR1)))
as.data.frame(confint(MLR1))

MLR2 <-glm(Knowledge.2~ .-Knowledge.1, data = uv_demo, family = binomial)
summary(MLR2)
as.data.frame(exp(coef(MLR2))) 
as.data.frame(confint(MLR2)) 




