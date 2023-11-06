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
  
##########################################################################################################
head(iris)
attach(iris)
  iris <- iris %>% mutate_if(is.character, as.factor)
  summary(iris)  
  set.seed(245)
  data_rows <- floor(0.80 * nrow(iris))
  train_indices <- sample(c(1:nrow(iris)), data_rows)
  train_data <- iris[train_indices,]
  test_data <- iris[-train_indices,]
  
  model = neuralnet(
    Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
    data=train_data,
    hidden=c(5,2), # first layer with 4 neutrons and second with two neutrons
    linear.output = FALSE
  )
    
  plot(model,rep = "best")

  pred <- predict(model, test_data)
  labels <- c("setosa", "versicolor", "virginca")
  prediction_label <- data.frame(max.col(pred)) %>%   #  creates a data frame with the column index of the maximum value in each row of the "pred" variable  
    mutate(pred=labels[max.col.pred.]) %>%
    select(2) %>%
    unlist()
  
  table(test_data$Species, prediction_label)  
  
  check <- as.numeric(test_data$Species) == max.col(pred)
  accuracy <- (sum(check)/nrow(test_data))*100
  print(accuracy)
  