library(tidyverse)

wbcd <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\wdbc.csv")
view(wbcd)
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$Diagnosis)
wbcd$Diagnosis <- factor(wbcd$Diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$Diagnosis)) * 100, digits = 1)
