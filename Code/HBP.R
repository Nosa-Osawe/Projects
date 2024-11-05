require(tidyverse)


nurce <- read.csv("C:\\Users\\DELL\\Desktop\\ndata.csv", fileEncoding = "UTF-8")
view(nurce)
attach(nurce)
colnames(nurce)

mod5 <- glm(HP_C ~ Gender+Age+Married+Lec.Rank, data = nurce, family = binomial)
unique(nurce$Lec.Rank)
 
nurce$Married <- factor(nurce$Married, levels = c("Single","Married"))
nurce$Lec.Rank <- factor(nurce$Lec.Rank, levels = c("Graduate Assistant",
                                                    "Assistant Lecturer",
                                                    "Lecturer I",
                                                    "Lecturer II",
                                                    "Senior Lecturer",
                                                    "Professor"))
nurce$knowledge_HP<- factor(nurce$knowledge_HP,
                            levels = c("No", "Yes"),
                            labels = c("0","1"))

MLR1 <-glm(knowledge_HP~ Gender+Age+Married+Lec.Rank, data = nurce, family = binomial)
summary(MLR1)

exp(coef(MLR1))
confint(MLR1)


lm1 <- lm(PhyExcer_C~Gender+Age+Married+Lec.Rank, data = nurce)
summary(lm1)

lm2 <- lm(MoitorSaltHyp_C~Gender+Age+Married+Lec.Rank, data = nurce)
summary(lm2)

lm3 <- lm(DietHyp_C~Gender+Age+Married+Lec.Rank, data = nurce)
summary(lm3)

lm4 <- lm(MedCheckBP_C~Gender+Age+Married+Lec.Rank, data = nurce)
summary(lm4)
