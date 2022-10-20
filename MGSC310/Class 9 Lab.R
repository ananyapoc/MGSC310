#Class 9 Lab

#1. Use dataset Credit from Package ISLR.
library(ISLR)
data(Credit)

#2. Use ?Credit to learn about the data.
?Credit

#3. Plot Balance against Income with the linear fitting line. 
library(ggplot2)
ggplot(data = Credit, aes(x = Income, y = Balance)) + 
geom_point() + geom_smooth(method = "lm", se = F)


