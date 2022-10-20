#Class 8 Lab

#random process
set.seed(310)  #set.seed() makes sure we get the same set of random #s
x <- runif(10)
print(x)

#load Auto datanset
library(ISLR)
data(Auto)

dim(Auto)

#split the data set into 70% train and 30% test
library(rsample)

set.seed(310)
auto_split <- initial_split(data = Auto, prop = 0.7) 
auto_train <- training(auto_split)
auto_test <- testing(auto_split)

dim(auto_train)
dim(auto_test)

#train a linear model for mpg ~ horsepower 
mod1 <- lm(mpg ~ horsepower, data = auto_train)
summary(mod1)

pred_train <- predict(mod1)
head(pred_train)

#predict for the test set
pred_test <- predict(mod1, newdata = auto_test)
head(pred_test)
