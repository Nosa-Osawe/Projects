library(tidyverse)
library(psych)


car_price <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\Nigerian_Car_Prices.csv",
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

# Specify the file path where you want to save the CSV file
Car_file_path <- "C:\\Users\\user\\Desktop\\carprice.csv"

# Save the data frame as a CSV file
write.csv(car_price, file = Car_file_path, row.names = FALSE)

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

### collect exchange rate data from CBN website
CBN <- read.csv("C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\exchange01112023.csv")

# Select rows where "column_name" is between 10 and 20
exchange <- CBN %>%select(Rate.Year, Selling.Rate) %>% 
  filter(Rate.Year >= 2000, Rate.Year <= 2020)

# lets check for outliers


selling_rate_summary <- exchange %>%
  group_by(Rate.Year) %>%
  summarize(
    min = min(Selling.Rate),
    max = max(Selling.Rate),
    mean = mean(Selling.Rate),
    q1 = quantile(Selling.Rate, 0.25),
    q3 = quantile(Selling.Rate, 0.75)
  )


average_exchange <- exchange %>%
  group_by(Rate.Year) %>%
  summarize(Selling.Rate = mean(Selling.Rate)) ### ops something is wrong


# Create box plots for sales by year

exchange_Rate_boxplot <- exchange %>%
  select(Rate.Year, Selling.Rate) %>%
  ggplot() +
  geom_boxplot(aes(x=factor(Rate.Year), y = Selling.Rate))+ 
  labs(title = "Selling rate per year", x = "Year", y = "Selling rate")
### significant outliers ##Wahala

Filtered_exc_rate <- exchange %>%
  group_by(Rate.Year) %>%
  filter(Selling.Rate >= quantile(Selling.Rate, 0.25)
         & Selling.Rate <= quantile(Selling.Rate, 0.75))
## lets check
head(Filtered_exc_rate, 10)
tail(Filtered_exc_rate, 10)
# Now, 'filtered_df' contains only rows within the IQR for each year

# lets create another box plot to see outliers

Filtered_exchange_Rate_boxplot <- Filtered_exc_rate %>%
  select(Rate.Year, Selling.Rate) %>%
  ggplot() +
  geom_boxplot(aes(x=factor(Rate.Year), y = Selling.Rate))+ 
  labs(title = "Selling rate per year", x = "Year", y = "Selling rate")
## No sig. outliers

## find average

average <- Filtered_exc_rate %>%
  group_by(Rate.Year) %>%
  summarize(Selling.Rate = mean(Selling.Rate))

print(average)

# lets try to merge our car price data with the  average exchange rate data
# identical year names
car.price <- car_price %>%
  rename("Year" = Year.of.manufacture)
average <-average %>%
  rename("Year" = Rate.Year)


merged_Data <- inner_join(car.price,average, by = "Year")


#### lets compare with the initial model

pairs.panels(merged_Data[c("Year", "Mileage", 
                         "Price", "Engine.Size", "Selling.Rate")])

Car.model <- lm( Price~ Fuel*Mileage+ Year+ Condition+Selling.Rate+
                   Condition*Year+ 
                   Selling.Rate*Year+
                   Transmission*Make*Condition + 
                   Engine.Size*Selling.Rate+ 
                   Build*Year+
                   Build*Condition*Engine.Size*Make+
                   Make*Condition*Year+
                   Fuel*Year*Make+ Year*Fuel+
                   Condition*Mileage*Selling.Rate+
                   Selling.Rate*Build+
                   Selling.Rate*Year*Engine.Size*Condition*Make*Build,
                 data = merged_Data)
summary(Car.model) # 88.3% accuracy

#### write the data to memory
write.csv(merged_Data, 
          "C:\\Users\\user\\Documents\\GitHub\\Nosa_thesis\\Data\\updated_cardata.csv")

