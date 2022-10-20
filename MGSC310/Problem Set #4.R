#Problem Set 4: Regularization#
#Ananya Pochiraju, Nikita Shetty, Kashish Pandey#

#Question 1: What Predicts Bike Share Usage?#

#a. Navigate to the UCI Machine Learning website and download the data folder 
# for the Bike Sharing Dataset. Run the cleaning code below to prepare the 
# data frames bike_train and bike_test. 
# Then print the first few rows of the bike_train data frame with 
# either the head or slice command.

library('tidyverse')
library('rsample')
library('glmnet')
library('glmnetUtils')
library('coefplot')

Bike_DF <- read.csv("datasets/day.csv")

Bike_DF <- Bike_DF %>% mutate(weathersit = factor(weathersit),
                              season = factor(season),
                              yr = factor(yr),
                              month = factor(mnth),
                              holiday = factor(holiday),
                              weekday = factor(weekday),
                              workingday = factor(workingday),
                              temp_sq = temp * temp,
                              atemp_sq = atemp * atemp,
                              hum_sq = hum * hum,
                              windspeed_sq = windspeed * windspeed,)

set.seed(310)
bike_split <- initial_split(Bike_DF, p = 0.8)
bike_train <- training(bike_split)
bike_test <- testing(bike_split)

head(bike_train)

#b. Using the metadata provided by the UCI website, describe what the dataset 
#contains, and for each variable give a short variable description.

# This dataset contains the daily count of rental bikes between years 2011 and 
# 2012 in the Capital bikeshare system with the corresponding weather and 
# seasonal information. The variable information is as follows:

# instant: record index
# dteday: date
# season: season(1:winter, 2:spring, 3:summer, 4:fall)
# yr: year(0:2011, 1:2012)
# mnth : month ( 1 to 12) 
# hr : hour (0 to 23) 
# holiday : weather day is holiday or not 
# weekday : day of the week 
# workingday : if day is neither weekend nor holiday is 1, otherwise is 0. 
# weathersit : 
# 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, 
# Light Rain + Scattered clouds 
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp : Normalized temperature in Celsius. 
# atemp: Normalized feeling temperature in Celsius. 
# hum: Normalized humidity. The values are divided to 100 (max) 
# windspeed: Normalized wind speed. The values are divided to 67 (max) 
# casual: count of casual users 
# registered: count of registered users 
# cnt: count of total rental bikes including both casual and registered 

#c. Run the command sapply(bike_train, is.factor) to ensure the columns 
# in your data frame that should be factors have been converted to factors 
# appropriately. Can you guess what the sapply function does?

sapply(bike_train, is.factor)

# Sapply applies a function over a list/vector. It takes either a list,vector,
# or dataframe as the input, and then it outputs as a vector or a matrix

#d. Fit a Ridge model against the bike_train dataset to predict cnt as a 
# function of season, holiday, month, workingday, weathersit, temp, hum, 
# and windspeed. Store the model in ridge_fit. Print the fitted model after 
# you have estimated it.

ridge_fit <- cv.glmnet(cnt ~ season + holiday + month + workingday+ weathersit
                       +temp+hum+windspeed,
                       data = bike_train,
                       alpha = 0)

ridge_fit

#e. Call the plot function against the fitted model. 
# Describe the plot (x and y axes). What are the dashed vertical lines?

plot(ridge_fit)

# this plots the lambda (x axis) against the cross validated error rate 
# (y axis). The first vertical dashed line is the value of lambda that 
# minimizes cross validated error (lambda.min), and the second is the value 
# of lambda that minimizes cross validated error plus one estimated 
# standard error (lambda.1se)

#f. What are the values for lambda.min and lambda.1se for the ridge model? 
# Which would you use if you wanted more shrinkage/regularization?

print(ridge_fit$lambda.min)
print(ridge_fit$lambda.1se)

# Lambda.min is 123.7984
# Lambda.1se is 660.6749

# For more shrinkage/regularization we would use the higher lambda 
# (Lambda.1se in this case), because it would penalize the model for 
# overfitting, which would regularize the model more than a lower lambda. 

#g. Use the code below to print the coefficient for the lambda.min and 
# lambda.1se version of the models.

ridge_coefs <- data.frame(
  ridge_lambda_min = coef(ridge_fit, s = ridge_fit$lambda.min) %>% 
   round(3) %>% as.matrix() ,
ridge_lambda_1se = coef(ridge_fit, s = ridge_fit$lambda.1se) %>% 
  round(3) %>% as.matrix() 
) %>% rename(ridge_min = 1, ridge_1se = 2)

print(ridge_coefs)

#h. Do the coefficients using lambda.min have more or less bias than the 
# ones using lambda.1se? Why would we want to use a model that has more 
# bias rather than one that has less?

# The coefficients using lambda.1se have more bias because it would penalize 
# the model more, which could lead to underfitting. We would want to use a 
# model that has more bias rather than less, because even though the 
# model is less complex, it is atleast not overfitting the data.

#i. Using the code below, predict the outcome variable for the training and 
# testing sets for lambda.min.

preds_ridge_train <- predict(ridge_fit, bike_train, s=ridge_fit$lambda.min)
preds_ridge_test <- predict(ridge_fit, bike_test, s=ridge_fit$lambda.min)

#j. Use caret package to calculate RMSE for the training and testing sets. 
# Do you see evidence of overfitting?

library('caret')
RMSE(pred = preds_ridge_train, obs = bike_train$cnt)
RMSE(pred = preds_ridge_test, obs = bike_test$cnt)

# The model has a higher test error than train error, which 
# means the model is not overfit. If the model were overfit, the train error 
# would have been higher than the test error. 

#Question 2: Lassoing Bikeshare Data#

#a. Estimate a Lasso model using the bike_train dataset predicting cnt as a 
# function of season, holiday, month, workingday, weathersit, temp, hum, and 
# windspeed. Print the fitted model after you have estimated it.

lasso_mod <- cv.glmnet(cnt ~ season + holiday + month + workingday+ 
                         weathersit+temp+hum+windspeed,
                       data = bike_train,
                       alpha = 1)

lasso_mod

#b. Use the code below to create the lasso coefficient vector. 
# Print the coefficient vector.

lasso_coefs <- data.frame(
  lasso_min = coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
    round(3) %>% as.matrix() ,
  lasso_1se = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() 
) %>% rename(lasso_min = 1, lasso_1se = 2)

print(lasso_coefs)

#c. Which version of the coefficients have more zero values? And why?

lasso_coefs %>% head()

# lasso_1se has more zero values because it has a higher lambda so there is a  
# greater penalty, which means that it eliminates more variables that are 
# not as important to the model, as compared to lasso_min. 

#d. Use a filter command against the lasso coefficient data frame to print the 
# number of non-zero coefficients for the lambda.min and lambda.1se. 
# How many non-zero coefficients are there for each version of the model?

count_min <- count(filter(lasso_coefs, lasso_min != 0))
count_1se <- count(filter(lasso_coefs, lasso_1se != 0))

count_min
count_1se

# The number of non-zero coefficients for lambda.min: 21
# The number of non-zero coefficients for lasso_1se: 14

#e. Call the plot function against the lasso model and describe the x and y 
# axes, the two dashed vertical lines, and the number on the top of the plot.

plot(lasso_mod)

# this plots the lambda (x axis) against the cross validated error rate (y axis)
# the first vertical dashed line is the value of lambda that minimizes cross 
# validated error (lambda.min), which is at 21. 
# the second is the value of lambda that minimizes cross validated error 
# plus one estimated standard error (lambda.1se), which is at 14. 

#f. Similarly to Ridge, predict the outcome variable for the training and 
# testing sets for lambda.min.

preds_train <- predict(lasso_mod, s = lasso_mod$lambda.min, bike_train)
preds_test <- predict(lasso_mod, s = lasso_mod$lambda.min, bike_test)

#g. Use caret package to calculate RMSE for the training and testing sets. 
# Do you see evidence of overfitting?

library(caret)
RMSE(preds_train, bike_train$cnt)
RMSE(preds_test, bike_test$cnt)

# There is no overfitting taking place because the RMSE for bike_test is higher
# than the RMSE for bike_train. If the model was overfit, 
# the training RMSE would be higher than the testing RMSE. 

#h. Suppose you are a data science manager deciding which model to implement 
# to predict Bikeshare demand. Write a two paragraph summary of your findings. 
# Discuss specifically i) which model to implement and ii) variables that are 
# predictors of bikeshare demand. You may want to use the coefpath function 
# to assist your answer in ii) to help determine the top 4 most important 
# variables since your company is very busy and needs to understand the 
# four most important factors.

coefpath(ridge_fit)
coefpath(lasso_mod)

# The model that would be best utilized for predicting bikeshare demand would 
# be the lasso model. This is because the nature of lasso is that it's a 
# variable selection tool that eliminates any insignificant variables. This 
# allows for full control and ease when trying to determine what predictor 
# variables are useful and which aren't. In comparison, ridge will not fully 
# eliminate variables with insignificant effects,and will instead bring their 
# coefficient values very close to 0 if not exactly 0. This means that the 
# variables still can affect the model, and impact accuracy and any results 
# that are trying to be extracted. 

# For our specific model, we determined that the four variables that were the 
# most useful were season1, weathersit1, weathersit3, and temp. 
# The lambda value that showed these 4 variables was 5.91. It eliminated all 
# other unimportant variables beyond this point. In terms of the affects of the 
# 4 variables on bikeshare demand, weathersit1 and temperature both have 
# positive effect, because both have positive coefficients. However, weathersit3
# and season1 both have negative effects on bikeshare demand, because both 
# have negative coefficients. 







