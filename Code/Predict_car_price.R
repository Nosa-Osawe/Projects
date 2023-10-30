library(tidyverse)
library(psych)


car_price <- read.csv("C:\\Users\\user\\Documents\\Nigerian_Car_Prices.csv",
                      stringsAsFactors = TRUE)
view(car_price)
car_price <- car_price[,-11:-13] ### just to delete the nonsense column generated
str(car_price)
attach(car_price)

# lets see how price vary by condidtion
aggregate(data = car_price, Price ~ Condition+Year.of.manufacture, mean, na.rm = TRUE)
# trying to cut don some outliers
car_price$Condition<- ifelse(car_price$Condition == "Foreign Used" | 
                               car_price$Condition == "Nigerian Used",
                             car_price$Condition, NA)
car_price$Engine.Size <- ifelse(car_price$Engine.Size >= 900 & car_price$Engine.Size< 6000,
                                car_price$Engine.Size, NA)
car_price$Mileage      <- ifelse(car_price$Mileage >= 10000 & car_price$Mileage< 500000,
                                car_price$Mileage, NA)
car_price<-na.omit(car_price)

#### see data distribution
pairs.panels(car_price[c("Year.of.manufacture", "Mileage", "Price", "Engine.Size")])

Car_model <- lm( Price~ Fuel*Mileage+ Year.of.manufacture+ Condition+
                   Condition*Year.of.manufacture+ 
                   Transmission*Make*Condition + 
                    + Engine.Size+ Build*Year.of.manufacture+ Build*Condition*Engine.Size*Make+
                   Make*Condition*Year.of.manufacture+
                   Fuel*Year.of.manufacture*Make+ Year.of.manufacture*Fuel+
                   Condition*Mileage,
                data = car_price)
summary(Car_model) # 75% accuracy

##############################################################################
######### 1. LOAD DATASET #############
car <- read.csv("C:\\Users\\user\\Documents\\Nigerian_Car_Prices.csv", stringsAsFactors = TRUE)
car <- car[,-11:-13]
nrow(car)


######### 2. CLEAN DATASET ##############


# Remove rows with NA values, just incase
car <- na.omit(car)

#check one more time for na values
sum(apply(is.na(car), 1, any))
nrow(car)

########## 3. SPLIT INTO TESTING AND TRAINING SETS ##########
# Set a seed for reproducibility
set.seed(123)

# Generate random indices for the training and testing sets
train_indices <- sample(nrow(car), 0.7 * nrow(car))  # 70% for training
test_indices <- setdiff(1:nrow(car), train_indices) # 30% testing
head(train_indices)

# Create training and testing sets using indices
train_data <- car[train_indices, ]
test_data <- car[test_indices, ]

# check size of testing and training datasets
nrow(train_data)
nrow(test_data)


########### 4. TRAIN MODEL TO PREDICT LITERACY ############
glm.build <- glm(Build ~ Price + Fuel + Engine.Size + Transmission
                    , data = train_data, family = "binomial")


########### 5. PREDICT build USING MODEL ##############
glm.build.predict <- predict(glm.build, test_data, type = 'response')

#print first few results
head(glm.build.predict,3)

########### 6. CHECK MODEL PERFORMANCE ###############
#convert predicted values back to True/False
test_data$predict.build <- ifelse(glm.build.predict >= .5,"SUV", "Not SUV")

head(test_data$predict.build ,5)
head(test_data$Build,5)

#Determine accuracy of model
car_accuracy <- mean(test_data$predict.build == test_data$Build)
print(car_accuracy)

table(car$Build)
