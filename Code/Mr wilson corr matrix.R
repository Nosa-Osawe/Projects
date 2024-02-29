library(corrplot)

wil <- read.csv("C:\\Users\\HP\\Desktop\\Wil_corr_csv.csv")

attach(wil)
corrplot(cor(wil),  method =c("number"),
         addCoef.col='black',
         tl.cex = 1, tl.col = 'black', type = "lower")

?corrplot
