library(tidyverse)
library(readxl)

getwd()

eye <- read_excel("Data/Corrected_TeleEyeCare_Survey_Responses_166.xlsx")


glimpse(eye)




eye <- eye %>% 
  mutate(Knowledge_1= factor(Knowledge_1, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_2= factor(Knowledge_2, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_3= factor(Knowledge_3, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_4= factor(Knowledge_4, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_5= factor(Knowledge_5, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_6= factor(Knowledge_6, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_7= factor(Knowledge_7, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_8= factor(Knowledge_8, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_9= factor(Knowledge_9, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             )),
         Knowledge_10= factor(Knowledge_10, levels = c("1","2","3","4","5"),
                             labels = c("Strongly Disagree",
                                        "Disagree", 
                                        "Neutral",
                                        "Agree", 
                                        "Strongly Agree"
                             ))) %>% 
  mutate(Challenge_1= factor(Challenge_1, levels = c("1","2","3","4","5"),
                             labels = c("Not a Challenge",
                                        "Slight Challenge", 
                                        "Moderate Challenge",
                                        "Significant Challenge", 
                                        "Major Challenge"
                             )),
         Challenge_2= factor(Challenge_2, levels = c("1","2","3","4","5"),
                             labels = c("Not a Challenge",
                                        "Slight Challenge", 
                                        "Moderate Challenge",
                                        "Significant Challenge", 
                                        "Major Challenge"
                             )),
         Challenge_3= factor(Challenge_3, levels = c("1","2","3","4","5"),
                             labels = c("Not a Challenge",
                                        "Slight Challenge", 
                                        "Moderate Challenge",
                                        "Significant Challenge", 
                                        "Major Challenge"
                             )),
         Challenge_4= factor(Challenge_4, levels = c("1","2","3","4","5"),
                             labels = c("Not a Challenge",
                                        "Slight Challenge", 
                                        "Moderate Challenge",
                                        "Significant Challenge", 
                                        "Major Challenge"
                             )),
         Challenge_5= factor(Challenge_5, levels = c("1","2","3","4","5"),
                             labels = c("Not a Challenge",
                                        "Slight Challenge", 
                                        "Moderate Challenge",
                                        "Significant Challenge", 
                                        "Major Challenge"
                             )),
         Challenge_6= factor(Challenge_6, levels = c("1","2","3","4","5"),
                             labels = c("Not a Challenge",
                                        "Slight Challenge", 
                                        "Moderate Challenge",
                                        "Significant Challenge", 
                                        "Major Challenge"
                             ))) %>% 
  mutate(Opportunity_1= factor(Opportunity_1, levels = c("1","2","3","4","5"),
                               labels = c("Strongly Disagree",
                                          "Disagree", 
                                          "Neutral",
                                          "Agree", 
                                          "Strongly Agree"
                             )),
         Opportunity_2= factor(Opportunity_2, levels = c("1","2","3","4","5"),
                               labels = c("Strongly Disagree",
                                          "Disagree", 
                                          "Neutral",
                                          "Agree", 
                                          "Strongly Agree"
                             )),
         Opportunity_3= factor(Opportunity_3, levels = c("1","2","3","4","5"),
                               labels = c("Strongly Disagree",
                                          "Disagree", 
                                          "Neutral",
                                          "Agree", 
                                          "Strongly Agree"
                             )),
         Opportunity_4= factor(Opportunity_4, levels = c("1","2","3","4","5"),
                               labels = c("Strongly Disagree",
                                          "Disagree", 
                                          "Neutral",
                                          "Agree", 
                                          "Strongly Agree"
                             )),
         Opportunity_5= factor(Opportunity_5, levels = c("1","2","3","4","5"),
                               labels = c("Strongly Disagree",
                                          "Disagree", 
                                          "Neutral",
                                          "Agree", 
                                          "Strongly Agree"
                             ))) %>% 
  mutate(Age = case_when(
    Age %in% c(21:30)  ~ "21 - 30 yrs",
    Age %in% c(31:40)  ~ "31 - 40 yrs",
    Age %in% c(41:50)  ~ "41 - 50 Yrs",
    Age %in% c(51:60)  ~ "51 - 60 yrs"
  )) 
  
# view(eye)

write.csv(eye, file = "Data/Corrected_TeleEyeCare.csv")



