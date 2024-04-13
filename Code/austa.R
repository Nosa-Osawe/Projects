library(tidyverse)


 Austa <- read.csv("C:\\Users\\HP\\Desktop\\GS\\Austa_Data.csv")
 View(Austa)
 attach(Austa)
 
# Define the age groups
age_breaks <- c(5, 7, 11, 14, 17)
age_labels <- c("Age 5-7",
                "Age 8-11", "Age 12-14", 
                "Age 15-17")



# Create the 'age_group' column
df_Austa <- Austa %>%
  mutate(age_group = cut(AGE, breaks = age_breaks, 
                         labels = age_labels, 
                         include.lowest = TRUE)) ### don't mutate more than once

head(df_Austa)
view(df_Austa)
write.csv(df_Austa, file ="C:\\Users\\HP\\Desktop\\GS\\new_Austa_Data.csv")
