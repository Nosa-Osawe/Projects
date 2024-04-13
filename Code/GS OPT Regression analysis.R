library(haven)   ### load in SPSS
library(tidyverse)


gs_data <- read_sav("C:\\Users\\HP\\Desktop\\GS\\GS_data.sav")
view(gs_data)

str(gs_data)

gs_data_1<-as.data.frame(gs_data)
view(gs_data_1)

attach(gs_data_1)

age_model <- lm(satisfied~., data = gs_data_1)
summary(age_model)
plot(age_model)

gs_data_1$Gender <- as.factor(gs_data_1$Gender)
gs_data_1$lga <- as.factor(gs_data_1$lga)
gs_data_1$Locat <- as.factor(gs_data_1$Locat)
gs_data_1$AgeCat <- as.factor(gs_data_1$AgeCat)

AgeCat_ <- lm(satisfied~AgeCat, data = gs_data_1)
summary(AgeCat_)

Gender_ <- lm(satisfied~Gender, data = gs_data_1)
summary(Gender_)

Sector_ <-lm(satisfied~Sector, data = gs_data_1)
summary(Sector_)

Locat_ <-lm(satisfied~Locat, data = gs_data_1)
summary(Locat_)

Lga_ <-lm(satisfied~lga, data = gs_data_1)
summary(Lga_)

practiceyears_ <- lm(satisfied~practiceyears, data = gs_data_1)
summary(practiceyears_)

postgraduate_ <- lm(satisfied~postgraduate, data = gs_data_1)
summary(postgraduate_)

salaryrating_ <- lm(satisfied~salaryrating, data = gs_data_1)
summary(salaryrating_)

nonfinancialincentives_ <- lm(satisfied~nonfinancialincentives, data = gs_data_1)
summary(nonfinancialincentives_)

attach(gs_data_1)

jobsecurity_ <- lm(satisfied~jobsecurity, data = gs_data_1)
summary(jobsecurity_)

equipment_ <-lm(satisfied~equipment, data = gs_data_1)
summary(equipment_)


supervision_ <-lm(satisfied~supervision, data = gs_data_1)
summary(supervision_)


encouragement_<-lm(satisfied~encouragement, data = gs_data_1)
summary(encouragement_)

recognition_ <- lm(satisfied~recognition, data = gs_data_1)
summary(recognition_)


responsibility_ <-lm(satisfied~responsibility, data = gs_data_1)
summary(responsibility_)

variety_ <- lm(satisfied~variety, data = gs_data_1)
summary(variety_)

workload_ <- lm(satisfied~workload, data = gs_data_1)
summary(workload_)

control_ <- lm(satisfied~workload, data = gs_data_1)
summary(control_)

support_ <- lm(satisfied~support, data = gs_data_1)
summary(support_)

opportunities_ <- lm(satisfied~opportunities, data = gs_data_1)
summary(opportunities_)


advancement_ <- lm(satisfied~advancement, data = gs_data_1)
summary(advancement_)




satisfactory_pred <- lm(satisfied~ AgeCat+lga+practiceyears+
                           postgraduate+salaryrating+nonfinancialincentives+
                           jobsecurity+equipment+supervision+encouragement+
                           recognition+responsibility+variety+workload+
                           control+support+opportunities+advancement,
                         data = gs_data_1)

summary(satisfactory_pred)










