library(tidyverse)
library(plotly)

data＜- mtcars %＞% mutate(cyl = factor(cyl),
                          Model = rownames(mtcars))

# Basic bubble plot in ggplot2

plot1 ＜- data %＞% ggplot(aes(x = wt, y = mpg, size = hp)) +
  geom_point(alpha = 0.5) 

plot1

# Bubble plot with color and custom size

plot2 ＜- data %＞% ggplot(aes(x = wt, y = mpg, size = hp, 
                             color = cyl, label = Model)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(.1, 15))

plot2

# Convert ggplot into plotly plot
p ＜- ggplotly(plot2, width=500, height=500) %＞%
  layout(xaxis = list(range = c(1, 6)),
         yaxis = list(range = c(8, 35)),
         legend = list(x = 0.825, y = .975))

p
