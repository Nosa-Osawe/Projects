###  poisson regressio

### https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/

install.packages("datasets")
library(datasets)
library(arm) # we use it her to compare models
install.packages("broom")
install.packages("ggstance")
library(broom)
library(ggstance)

data(warpbreaks)

data <- warpbreaks
str(warpbreaks)
hist(data$breaks)
#### Check the variance and the mean
# if mean = variance, then there is no problem. else; overdispersion or underdispersion

mean(data$breaks)
var(data$breaks)    ### variance is higher than the mean (Overdispersion)

# If the Residual Deviance is greater than the degrees of freedom, 
# then over-dispersion exists. This means that the estimates are correct, 
# but the standard errors (standard deviation) are wrong and unaccounted for by the model.

poisson_model <- glm(breaks ~ wool + tension, data= data, family = poisson(link = "log"))
summary(poisson_model)
# due to overdispersion, use family = quasipoisson

poisson.model2 <- glm(breaks ~ wool + tension, data = data, family = quasipoisson(link = "log"))
summary(poisson.model2)



# extract coefficients from first model using 'coef()'
coef1 = coef(poisson_model)

# extract coefficients from second model
coef2 = coef(poisson.model2)

# extract standard errors from first model using 'se.coef()'
se.coef1 = se.coef(poisson_model)

# extract standard errors from second model
se.coef2 = se.coef(poisson.model2)

# use 'cbind()' to combine values into one dataframe
models.both <- cbind(coef1, se.coef1, coef2, se.coef2, exponent = exp(coef1))

# show dataframe
models.both

# use 'predict()' to run model on new data
predicted <- predict(poisson.model2, newdata = warpbreaks, type = "response")


predict_ <- predict(poisson.model2, warpbreaks)
table(warpbreaks$breaks, predict_)
mean(as.character(warpbreaks$breaks) != as.character(predict_))

# Include jtools library
library(jtools)

# plot regression coefficients for poisson.model2
plot_summs(poisson.model2, scale = TRUE, exp = TRUE)


cat_plot(poisson.model2, pred = wool, modx = tension)
# argument 1: regression model
# pred: The categorical variable that will appear on x-axis
# modx: Moderator variable that has an effect in combination to pred on outcome

#######################################################################################

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)
 poisson_Data <- p

 summary(m1 <- glm(num_awards ~ prog + math, family="poisson", 
                   data=poisson_Data))
 
 
 
 library(tidyverse)
 attrition <-  read_csv("Data/employee_attrition.csv")
 attrition <- read.csv("C:\\Users\\HP\\Documents\\Projects\\Data\\employee_attrition.csv", 
                       stringsAsFactors = TRUE)
view(attrition) 
str(attrition)

attrition$Salary <- scale(attrition$Salary)
attach(attrition)

q_breaks <- c(0, 3, 6, 9,  12, 15, 18, 21, 24, 27, 30)
q_labels <- c("1","2", "3", "4", "5", "6", "7", "8", "9", "10")


# Create the 'Work_len_quaters' column
attrition_filtered <- attrition %>%
  mutate(quaters = cut(Work_len_quaters, breaks = q_breaks, 
                         labels = q_labels, 
                         include.lowest = TRUE))

length(attrition_filtered$quaters)
unique(attrition_filtered$quaters)
str(attrition_filtered$quaters)
attrition_filtered$quaters <- as.integer(attrition_filtered$quaters)

mean(attrition_filtered$quaters)
var(attrition_filtered$quaters) ## Over-dispersion likely plays here!

attach(attrition_filtered)
attrition_filtered$quaters <- factor(attrition_filtered$quaters)


samplesize = 0.70*nrow(attrition_filtered)
set.seed(99)
my_index = sample(seq_len(nrow(attrition_filtered)), size = samplesize)
#Creating training and test set 
datatrain_attrition <- attrition_filtered[my_index,]
datatest_attrition <- attrition_filtered[-my_index,]


pred_att <- glm(quaters ~ Quarterly.Rating +Promotions +
                  Promotions, 
                data = datatrain_attrition, 
                family = quasipoisson(link = "log"))

summary(pred_att)

datatrain_attrition %>% 
  filter(quaters =='0') %>% 
  head()

