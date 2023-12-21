library(psych)
library(tidyverse)

attrition <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\employee_attrition.csv", 
                      stringsAsFactors = TRUE)
view(attrition) 
str(attrition)

attrition %>% 
  select(Work_len_quaters) %>% 
  filter(Work_len_quaters > 5) %>% 
  head() #       Views some Work_len_quaters

attrition$Designation <- factor(attrition$Designation, 
                                levels = c("1", "2", "3", "4", "5"))
attrition$Joining.Designation <- factor(attrition$Joining.Designation, 
                                        levels = c("1", "2", "3", "4", "5"))
attrition$Education_Level <- factor(attrition$Education_Level, 
                                    levels=  c("College", 
                                               "Bachelor",
                                               "Master"))
attach(attrition)

view(attrition)

samplesize = 0.70*nrow(attrition)
set.seed(99)
my_index = sample(seq_len(nrow(attrition)), size = samplesize)
#Creating training and test set 
datatrain_attrition <- attrition[my_index,]
datatest_attrition <- attrition[-my_index,]


attrtion_model1 <-lm(Work_len_quaters~ Promotions+Salary+
                       Quarterly.Rating+
                       year.code*Salary+Designation
                       ,
                     data = datatrain_attrition)
summary(attrtion_model1)
### We can say confidently that latest quarterly rating, number of promotions, 
# number of years already in the company, and work designation level are the biggest factors
## affecting the length of stay of workers in this organization.

attrtion_model2 <-lm(Work_len_quaters~ Promotions+Salary+
                       Quarterly.Rating+
                       year.code*Salary+Designation
                     ,
                     data = datatest_attrition)
summary(attrtion_model2)      ## 85% Accuracy

# Assuming your linear model is attrtion_model1
# Assuming your test data is datatest_attrition

model_pred <- function(model, dataframe) {
  # Extract the relevant variables from the test data
  features <- dataframe[, c("Promotions", "Salary", 
                            "Quarterly.Rating", "year.code", "Designation")]
  
  predicted_values <- predict(model,  features)
  
  result <- cbind(dataframe, predicted_quaters = ifelse(predicted_values >= 0, 
                                    round(predicted_values, 0), 
                                    "0")
                  )
  
  return(result)
}

# Use the function to predict and bind the results
newdata1 <- model_pred(attrtion_model1, attrition)
view(newdata1)

write.csv(newdata1,"C:\\Users\\HP\\Documents\\Projects\\Data\\attrition_new1.csv")

newdata2 <- model_pred(attrtion_model1, datatrain_attrition)
view(newdata2)
