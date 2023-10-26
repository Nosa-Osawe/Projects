library(tidyverse)
library(class)
library(gmodels)

credit <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\credit.csv")
str(credit)
colnames(credit)

table(credit$Status.of.existing.checking.account)
table(credit$Savings.account.bonds)

table(credit$X1...Good..2...Bad)

set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
head(credit_rand$Credit.amount) # to show that the randomity works

credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$X1...Good..2...Bad))
prop.table(table(credit_test$X1...Good..2...Bad))



