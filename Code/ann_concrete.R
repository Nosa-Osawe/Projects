library(tidyverse)
library(class)
library(gmodels)
install.packages("neuralnet")
library(neuralnet)

concrete <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\concrete_Data.csv", 
                      stringsAsFactors = TRUE)  ### bring in all strings as factors


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))}
concrete_norm <- as.data.frame(lapply(concrete, normalize))

summary(concrete_norm$Concrete.compressive.strength)

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

concrete_model <- neuralnet(Concrete.compressive.strength ~ Cement.component + Blast.Furnace.Slag +
                              Fly.Ash + Water + Superplasticizer +
                              Coarse.Aggregate + Fine.Aggregate + Age..day.,
                            data = concrete_train)


plot(concrete_model)

model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$Concrete.compressive.strength)
### a good model that can be improved upon

concrete_model2 <- neuralnet(Concrete.compressive.strength ~ Cement.component + Blast.Furnace.Slag +
                              Fly.Ash + Water + Superplasticizer +
                              Coarse.Aggregate + Fine.Aggregate + Age..day.,
                            data = concrete_train, hidden = 5)

plot(concrete_model2)

  model_results2 <- compute(concrete_model2, concrete_test[1:8])
  predicted_strength2 <- model_results2$net.result
  cor(predicted_strength2, concrete_test$Concrete.compressive.strength)
  
  
  
