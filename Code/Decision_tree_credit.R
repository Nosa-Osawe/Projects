library(tidyverse)
library(class)
library(gmodels)
library(C50)


credit <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\credit.csv")
str(credit)
colnames(credit)
credit$X1...Good..2...Bad <- factor(credit$X1...Good..2...Bad)
credit$Credit.history  <- factor(credit$Credit.history)
credit$Purpose <- factor(credit$Purpose)
credit$Savings.account.bonds  <- factor(credit$Savings.account.bonds )
credit$Present.employment.since  <- factor(credit$Present.employment.since )
credit$Personal.status.and.sex  <- factor(credit$Personal.status.and.sex )
credit$Other.debtors...guarantors <- factor(credit$Other.debtors...guarantors)
credit$Property <- factor(credit$Property)
credit$Housing <- factor(credit$Housing)
credit$Job <- factor(credit$Job)
credit$Status.of.existing.checking.account  <- factor(credit$Status.of.existing.checking.account )
credit$Telephone  <- factor(credit$Telephone)
credit$foreign.worker  <- factor(credit$foreign.worker)

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

credit_model <- C5.0(credit_train[-21], credit_train$X1...Good..2...Bad)
credit_model
summary(credit_model)

credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$X1...Good..2...Bad, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

###### improve performance
credit_boost10 <- C5.0(credit_train[-21], credit_train$X1...Good..2...Bad,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$X1...Good..2...Bad, credit_boost_pred10,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))
    # error rate is 21%

#assigning a cost or penalty matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)


credit_cost <- C5.0(credit_train[-21], credit_train$X1...Good..2...Bad,
                      costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$X1...Good..2...Bad, credit_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

# we just increased false negatives at the cost of reducing false positive





