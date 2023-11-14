library(tidyverse)


music <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\age_predict_using_music_Data.csv")

##      Data cleaning
filtered_music<- music %>% select(2:29)%>% 
  select(-X,-X.1)

colnames(filtered_music) <- c("age", "primary.streaming.service", "Hours.per.day",
                             "while.working", "instrumentalist","composer", 
                             "fav.genre", "exploratory", "foreign.language",
                             "BPM", "Classical", "Country",
                             "Hippop", "Jazz", "K.Pop", 
                             "Latin","Lofi", "Metal", 
                             "Pop", "R.B", "Rap", 
                             "Rock", "video.game.music")
filtered_music <- filtered_music %>% select(1:23)
view(filtered_music)

sum(is.na(filtered_music))
nrow(filtered_music)
filtered_music <- na.omit(filtered_music)
