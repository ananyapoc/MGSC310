#Class 7 Lab

library(ISLR)
library(MASS)
library(tidyverse)

# use the Auto dataset in ISLR
data (Auto)

head(Auto)

?Auto

glimpse(Auto)

Auto <- Auto %>% mutate(origin = factor (origin, labels = c("American", "European", "Japanese")))
glimpse(Auto)

Auto %>% group_by(origin) %>% summarize(avg_mpg = mean(mpg, na.rm = T), 
                                        avg_hp = mean(horsepower, na.rm = T), 
                                        num_cars = n())

p <- ggplot(data = Auto, aes(x=horsepower, y = mpg)) + geom_point()
 
print(p)
 
p + xlab("HP") + ylab("MPG") + ggtitle("Distribution of MPG and HP")
 
ggplot(data = Auto, aes(x=horsepower, y = mpg, color = origin)) + geom_point()

#histogram for mpg for different origins 
ggplot(data = Auto, aes(x=mpg, color=origin)) + geom_histogram(bins = 50)

#Linear Regression

#First Model: mpg ~ horsepower
#formula: y ~ x
mod1 <- lm(mpg ~ horsepower, data = Auto)
mod1

#explore the regression results by summary()
summary(mod1)

#extract the coefficients 
coef(mod1)
mod1$coefficients

#prediction
y_hat <- predict(mod1)

head(y_hat)
