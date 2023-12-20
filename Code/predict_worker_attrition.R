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
q_breaks <- c(0, 3, 6, 9,  12, 15, 18, 21, 24, 27, 30)
q_labels <- c("1","2", "3", "4", "5", "6", "7", "8", "9", "10")



# Create the 'Work_len_quaters' column
attrition_filtered <- attrition %>%
  mutate(quaters = cut(Work_len_quaters, breaks = q_breaks, 
                       labels = q_labels, 
                       include.lowest = TRUE))
# we would try to predict Work_len_quaters
attrition_filtered$quaters <- as.integer(attrition_filtered$quaters)


sum(is.na(attrition_filtered))

view(attrition_filtered)
samplesize = 0.70*nrow(attrition_filtered)
set.seed(99)
my_index = sample(seq_len(nrow(attrition_filtered)), size = samplesize)
#Creating training and test set 
datatrain_attrition <- attrition_filtered[my_index,]
datatest_attrition <- attrition_filtered[-my_index,]


attrtion_model1 <-lm(quaters~ Promotions+Salary+
                       Quarterly.Rating+
                       year.code*Salary+Designation
                       ,
                     data = datatrain_attrition)
summary(attrtion_model1)
### We can say confidently that latest quarterly rating, number of promotions, 
# number of years already in the company, and work designation level are the biggest factors
## affecting the length of stay of workers in this organization.

attrtion_model2 <-lm(quaters~ Promotions+Salary+
                       Quarterly.Rating+
                       year.code*Salary+Designation
                     ,
                     data = datatest_attrition)
summary(attrtion_model2)      ## 82% Accuracy

# Assuming your linear model is attrtion_model1
# Assuming your test data is datatest_attrition

predict_and_bind_test <- function(attrtion_model1, datatest_attrition) {
  # Extract the relevant variables from the test data
  features <- datatest_attrition[, c("Promotions", "Salary", 
                            "Quarterly.Rating", "year.code", "Designation")]
  
  # Predict using the linear model
  predicted_values <- predict(attrtion_model1, newdata = features)
  
  # Column bind the test data with the predicted values
  result <- cbind(datatest_attrition, Predicted_Values = predicted_values)
  
  return(result)
}


# Use the function to predict and bind the results
augmented_test_data <- predict_and_bind_test(attrtion_model1, datatest_attrition)

# Print or view the new data frame
print(augmented_test_data)

# Optionally, save the new data frame to an object
result_object <- augmented_test_data


# Use the function like this:
# Replace "attrtion_model1" with the name of your linear model
# Replace "datatest_attrition" with the name of your test data frame
# augmented_test_data <- predict_and_bind_test(attrtion_model1, datatest_attrition)






