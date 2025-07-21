library(tidyverse)
library(stats)
library(factoextra)

data("USArrests")
df<-scale(USArrests)
view(df)



fviz_nbclust(df, kmeans, method = "wss")  # within sum of square)
                                          # estimate the optimal number of clusters

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)+ labs(x = "PC2")
