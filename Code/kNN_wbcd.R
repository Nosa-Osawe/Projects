library(tidyverse)
library(class)
library(gmodels)

wbcd <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\wdbc.csv")
view(wbcd)
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$Diagnosis)
wbcd$Diagnosis <- factor(wbcd$Diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$Diagnosis)) * 100, digits = 1)

summary(wbcd$radius1,wbcd$area1 )

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area1)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]   # takes just the first column
wbcd_test_labels <- wbcd[470:569, 1]


wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)


CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)    ##### good performance

###### improve model performance
# try this : wbcd_z <- as.data.frame(scale(wbcd[-1]))


