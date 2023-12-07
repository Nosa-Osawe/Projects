library(tidyverse)
library(haven)
library(MASS)
library(dplyr)
music <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\age_predict_using_music_Data.csv")

##      Data cleaning

filtered_music <- music[, 2:29]
filtered_music <- filtered_music[, !(names(filtered_music) %in% c('X', 'X.1'))]

colnames(filtered_music) <- c("age", "primary.streaming.service", "Hours.per.day",
                             "while.working", "instrumentalist","composer", 
                             "fav.genre", "exploratory", "foreign.language",
                             "BPM", "Classical", "Country",
                             "Hippop", "Jazz", "K.Pop", 
                             "Latin","Lofi", "Metal", 
                             "Pop", "R.B", "Rap", 
                             "Rock", "video.game.music") # giving it better col.names

filtered_music <- filtered_music[, 1:23]
filtered_music <- na.omit(filtered_music)

sum(is.na(filtered_music))
nrow(filtered_music)
filtered_music <- na.omit(filtered_music)

 filtered_music %>%
  group_by(primary.streaming.service) %>%
  summarize(count = n())

 filtered_music %>%
  group_by(instrumentalist) %>%
  summarize(count = n())

 
 filtered_music <- filtered_music %>%
   filter(instrumentalist %in% c("No", "Yes"))

# Define the age groups
age_breaks <- c(10, 18, 23,  30, Inf)
age_labels <- c("below 18","19-23", "24-30", "30+")

# Create the 'age_group' column
df_filtered <- filtered_music %>%
  mutate(age_group = cut(age, breaks = age_breaks, 
                         labels = age_labels, 
                         include.lowest = FALSE)) ### don't mutate more than once


write.csv(df_filtered, 
          file = "C:\\Users\\HP\\Documents\\Projects\\Data\\music_data.csv",
          row.names = FALSE)

df_filtered$primary.streaming.service <- factor(df_filtered$primary.streaming.service )
df_filtered$while.working <- factor(df_filtered$while.working,  levels = c("No", "Yes"))
df_filtered$instrumentalist <- factor(df_filtered$instrumentalist, levels = c("No", "Yes"))
df_filtered$composer <-factor(df_filtered$composer, levels = c("No", "Yes"))
df_filtered$fav.genre <- factor(df_filtered$fav.genre)
df_filtered$exploratory <- factor(df_filtered$exploratory, levels = c("No", "Yes"))
df_filtered$foreign.language <- factor(df_filtered$foreign.language, levels = c("No", "Yes"))
df_filtered$Classical <- factor(df_filtered$Classical, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Country <- factor(df_filtered$Country, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Hippop <- factor(df_filtered$Hippop, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Jazz <- factor(df_filtered$Jazz, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$K.Pop <- factor(df_filtered$K.Pop, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Latin <- factor(df_filtered$Latin, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Lofi <- factor(df_filtered$Lofi, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Metal <- factor(df_filtered$Metal, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Pop <- factor(df_filtered$Pop, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$R.B <- factor(df_filtered$R.B, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Rap <- factor(df_filtered$Rap, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$Rock <- factor(df_filtered$Rock, levels = c("Never", "Rarely", "Sometimes","Very frequently"))
df_filtered$video.game.music <- factor(df_filtered$video.game.music, 
                                       levels = c("Never", "Rarely", "Sometimes","Very frequently"))

attach(df_filtered)
df_filtered <- na.omit(df_filtered)

#Dividing data into training and test set
#Random sampling 
samplesize = 0.70*nrow(df_filtered)
set.seed(100)
index_m = sample(seq_len(nrow(df_filtered)), size = samplesize)
#Creating training and test set 
datatrain_music <- df_filtered[index_m,]
datatest_music <- df_filtered[-index_m,]


music_model <- polr(age_group ~ while.working+composer + 
                      instrumentalist + fav.genre + primary.streaming.service 
                    +Rock + R.B + video.game.music,
                    data = datatrain_music ,
                    Hess = TRUE)
summary(music_model)


predict_music <- predict(music_model, datatest_music)
table(datatest_music$age_group, predict_music)
mean(as.character(datatest_music$age_group) != as.character(predict_music)) 
# misclassification error is high


##### tryin out a lm
age_model <- lm( age~ Classical+primary.streaming.service+
             +age_group,
                 data = datatest_music)
summary(age_model)

