library(tidyverse)
library(readxl)
library(corrplot)
 

biogas <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Mercy.xlsx", 
                     sheet = 'Sheet1')
# view(biogas)

biogas_L  <- biogas %>% 
  pivot_longer(
    cols = -c(1,2),
    names_to = "Type",
    values_to = "Values") %>%
  mutate(Biodigester = case_when(
      Type %in% c("M1", "M2", "M3")  ~ "Modular",
      Type %in% c("S1", "S2", "S3")  ~ "Singular"
    )) %>% 
  as.data.frame()


biogas_summary <- biogas_L %>% 
  group_by(Parameters, Day, Biodigester) %>% 
  summarise(Mean = mean(Values),
            SD = sd(Values)) %>% 
  as.data.frame()



biogas_summary %>% 
  filter(Parameters == "Ammonia (mg/L)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Ammonia (mg/L) over Time",
       x = "Days",
       y = "Mean Ammonia (mg/L)") +
  theme_minimal()

biogas_summary %>% 
  filter(Parameters == "Ash Contents (%)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Ash Contents (%) over Time",
       x = "Days",
       y = "Ash Contents (%)") +
  theme_minimal()

  
biogas_summary %>% 
  filter(Parameters == "COD (mg/L)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "COD (mg/L) over Time",
       x = "Days",
       y = "COD (mg/L)") +
  theme_minimal()


biogas_summary %>% 
  filter(Parameters == "TVA (mg/L)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "TVA (mg/L) over Time",
       x = "Days",
       y = "TVA (mg/L)") +
  theme_minimal()



biogas_summary %>% 
  filter(Parameters == "Temperature") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Temperature over Time",
       x = "Days",
       y = "Temperature") +
  theme_minimal()

biogas_summary %>% 
  filter(Parameters == "Total Solids (%)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Total Solids (%) over Time",
       x = "Days",
       y = "Total Solids (%)") +
  theme_minimal()


biogas_summary %>% 
  filter(Parameters == "Volatile Solids (%)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Volatile Solids (%) over Time",
       x = "Days",
       y = "Volatile Solids (%)") +
  theme_minimal()


biogas_summary %>% 
  filter(Parameters == "pH") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "pH over Time",
       x = "Days",
       y = "pH") +
  theme_minimal()


biogas_summary %>% 
  filter(Parameters == "Cumulative Biogas Volume (mL)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Cumulative Biogas Volume (mL) over Time",
       x = "Days",
       y = "Cumulative Biogas Volume (mL)") +
  theme_classic()



biogas_summary %>% 
  filter(Parameters == "Biogas Volume (mL)") %>% 
  ggplot(aes(x = Day, y = Mean, colour = Biodigester)) +
  geom_line() +   
  geom_point(aes(shape = Biodigester), size = 2) +  
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.5) +   
  scale_colour_manual(values = c("orange", "blue"))+
  labs(title = "Biogas Volume (mL) over Time",
       x = "Days",
       y = "Biogas Volume (mL)") +
  theme_classic()

lm1 <- lm(Values ~ Day+  
           I(Day^3) +
            Biodigester, data = biogas_L %>% 
            filter(Parameters == "Biogas Volume (mL)"))
summary(lm1)




biogas_L %>% 
  filter(Parameters == "Biogas Volume (mL)") %>% 
  ggplot(aes(x = Day, y = Values, colour = Biodigester, fill = Biodigester))+
  geom_point(size = 2, position = position_jitter(0.3), alpha = 0.6, aes(shape = Biodigester))+
  #geom_line(aes(y = fitted(lm1)),  linewidth = 1, alpha = 0.9)+
  geom_smooth(aes(y = fitted(lm1)), linewidth = 1, alpha = 0.9, se = TRUE)+
  scale_colour_manual(values = c("orange", "blue"))+
  scale_fill_manual(values = c("orange", "blue"))+
  labs(title = "Biogas Volume (mL) over Time",
       x = "Days",
       y = "Biogas Volume (mL)") +
  theme_bw()

lm2 <- lm(Values ~ Day+  
            Biodigester, data = biogas_L %>% 
            filter(Parameters == "Cumulative Biogas Volume (mL)"))
summary(lm2)

 
variables <- biogas_L %>% 
  pivot_wider(
    names_from = "Parameters",
    values_from = "Values"
  ) %>% 
  select(-c(1:3)) %>% 
  as.data.frame()


corrplot::corrplot(cor(variables),  method =c("number"),
                   addCoef.col='black',
                   tl.cex = 1, tl.col = 'black', type = "lower")


corrplot(cor(variables),  method =c("color"),
         addCoef.col='black',
         number.cex = 0.7,
         tl.cex = 0.8, tl.col = 'black', 
         type = "lower")
title(main = "Correlation Matrix for all physicochemicals", 
      col.main = "Black", cex.main = 1.2)



write.csv(as.data.frame(cor(variables)),
          file = "C:\\Users\\DELL\\Desktop\\corr_matrix.csv")
