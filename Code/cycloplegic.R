library(tidyverse)
library(readxl)

c.data<- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\cycloplegics.xlsx",
                    sheet = "working") %>% 
  rename(non_cyclo = "NON_CYCLOPLEGIC(BEFORE)",
         cyclo = "CYCLOPLEGIC(AFTER)",
         final_prescrib = "FINAL_Prx",
         refractive_error = "REFRACTIVE_ERROR")

# view(c.data)

c.data_e <- c.data %>%
  mutate(
    error_non = abs(non_cyclo - final_prescrib),
    error_cyclo = abs(cyclo - final_prescrib)
  )


Myopic.e <- c.data_e %>% 
  filter(refractive_error== "Myopic")

Hyperopic.e <- c.data_e %>% 
  filter(refractive_error == "Hyperopic")


c.data_e %>% 
  group_by(refractive_error) %>% 
  summarise(mean_error_noncyclo = mean(error_non),
            mean_error_cyclo = mean(error_cyclo),
            se_error_noncyclo = sd(error_non)/sqrt(length(error_non)),
            se_error_cyclo = sd(error_cyclo)/sqrt(length(error_cyclo)))


t.test(Myopic.e$error_non, Myopic.e$error_cyclo, var.equal = FALSE,
       paired = TRUE)

t.test(Hyperopic.e$error_non, Hyperopic.e$error_cyclo, var.equal = FALSE,
       paired = TRUE)


c.data_e %>% 
  rename('Non-cycloplegic' = error_non,
         'Cycloplegic' = error_cyclo) %>% 
  pivot_longer(
    cols = -c(1:4),
    names_to = "Error",
    values_to = "E.Value"
  ) %>% 
  filter(refractive_error== "Myopic") %>% 
  ggplot(aes(x= Error, y = E.Value))+
  stat_summary(
    geom = "bar", fun = mean, fill = "lightgreen", width = 0.6
    )+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.3, color= "black")+
  labs(title = " Myopic",
       x= "",
       y= 'Absolute error')+
  theme_classic()


c.data_e %>% 
  rename('Non-cycloplegic' = error_non,
         'Cycloplegic' = error_cyclo) %>% 
  pivot_longer(
    cols = -c(1:4),
    names_to = "Error",
    values_to = "E.Value"
  ) %>% 
  filter(refractive_error== "Hyperopic") %>% 
  ggplot(aes(x= Error, y = E.Value))+
  stat_summary(
    geom = "bar", fun = mean, fill = "lightgreen", width = 0.6
  )+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.3, color= "black")+
  labs(title = " Hyperopic",
       x= "",
       y= 'Absolute error')+
  theme_classic()




c.data_e %>% 
  rename("Non-cycloplegic" ="non_cyclo",
         "Cycloplegic"= "cyclo") %>% 
  pivot_longer(
    cols = c(2,3),
    names_to = "Cyclopegia",
    values_to = "values"
  ) %>% 
  filter(refractive_error== "Myopic") %>% 
  ggplot(aes(x= Cyclopegia, y = values))+
  stat_summary(
    geom = "point", fun = mean, size = 5, color = "red"
  )+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.2, color= "red")+
  labs(title = " Myopic",
       x= "",
       y= 'Value')+
  theme_classic()

c.data_e %>% 
  rename("Non-cycloplegic" ="non_cyclo",
         "Cycloplegic"= "cyclo") %>% 
  pivot_longer(
    cols = c(2,3),
    names_to = "Cyclopegia",
    values_to = "values"
  ) %>% 
  filter(refractive_error== "Hyperopic") %>% 
  ggplot(aes(x= Cyclopegia, y = values))+
  stat_summary(
    geom = "point", fun = mean, size = 5, color = "red"
  )+
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               linewidth = 1, width= 0.2, color= "red")+
  labs(title = "Hyperopic",
       x= "",
       y= 'Value')+
  theme_classic()


c.data_error<- c.data_e %>% 
  mutate(diff_cyclo = as.numeric(non_cyclo - cyclo)) %>% 
  as.data.frame()


Myopic.error <- c.data_e %>% 
  filter(refractive_error == "Myopic")

Hyperopic.error <- c.data_e %>% 
  filter(refractive_error == "Hyperopic")

c.data_e %>% 
  group_by(refractive_error) %>% 
  summarise(mean_non_cyclo = mean(non_cyclo),
            mean_cyclo = mean(cyclo),
            se_non_cyclo = sd(non_cyclo)/sqrt(length(non_cyclo)),
            se_cyclo = sd(cyclo)/sqrt(length(cyclo)))

t.test(Myopic.error$non_cyclo, Myopic.error$cyclo, paired =  TRUE, var.equal = FALSE)
t.test(Hyperopic.error$non_cyclo, Hyperopic.error$cyclo, paired =  TRUE, var.equal = FALSE)



# 

myopic_mean_Val= (Myopic.error$non_cyclo + Myopic.error$cyclo)/2
myopic_diff_val = (Myopic.error$non_cyclo - Myopic.error$cyclo)
myopic_bias = mean(myopic_diff_val)
myopic_loa = 1.96 * sd(myopic_diff_val)
m_lower = myopic_bias- myopic_loa
m_upper = myopic_bias+ myopic_loa

ggplot(data.frame(myopic_mean_Val, myopic_diff_val), 
       aes(x = myopic_mean_Val, y = myopic_diff_val)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_hline(yintercept = myopic_bias, color = "blue") +
  geom_hline(yintercept = m_upper, linetype = "dashed", color = "red") +
  geom_hline(yintercept = m_lower, linetype = "dashed", color = "red") +
  labs(title = "Bland-Altman Plot: Myopic",
       x = "Mean of Cycloplegic and Non-cycloplegic", 
       y = "Non-cycloplegic -  Cycloplegic (D)")+
  theme_classic()


#    "Hyperopic"
Hyperopic_mean_Val= (Hyperopic.error$non_cyclo + Hyperopic.error$cyclo)/2
Hyperopic_diff_val = (Hyperopic.error$non_cyclo - Hyperopic.error$cyclo)
Hyperopic_bias = mean(Hyperopic_diff_val)
Hyperopic_loa = 1.96 * sd(Hyperopic_diff_val)
h_lower = Hyperopic_bias- Hyperopic_loa
h_upper = Hyperopic_bias+ Hyperopic_loa


ggplot(data.frame(Hyperopic_mean_Val, Hyperopic_diff_val), 
       aes(x = Hyperopic_mean_Val, y = Hyperopic_diff_val)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_hline(yintercept = Hyperopic_bias, color = "blue") +
  geom_hline(yintercept = h_upper, linetype = "dashed", color = "red") +
  geom_hline(yintercept = h_lower, linetype = "dashed", color = "red") +
  labs(
    title = "Bland-Altman Plot: Hyperopic",
    x = "Mean of Cycloplegic and Non-cycloplegic", 
    y = "Non-cycloplegic -  Cycloplegic (D)"
  ) +
  theme_classic()









# 

########################################################################################



