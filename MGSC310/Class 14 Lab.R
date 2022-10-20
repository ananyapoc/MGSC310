#Class 14 Lab#

#for loop in R
for(i in 1:10)(
  print(i)
)

#loading the libraries
library(ISLR)
library(tidyverse)

data(Auto)
glimpse(Auto)

#remove the column name 
Auto <- Auto %>% select(-name)

#regression model: mpg 

#in-sample model 
mod_insample <- lm(mpg ~ ., 
                   data = Auto)
preds_insample <- predict(mod_insample)

#LOOCV
preds_LOOCV <- NULL

for(i in 1:nrow(Auto)){
  #train
  mod <- lm(mpg ~ .,
            data = Auto %>% slice(-i))
  #prediction
  preds_LOOCV[i] <- predict(mod,
                            newdata = Auto %>% slice(i))
}

#formatting result dataframe
results <- data.frame(
  true = Auto$mpg,
  preds_LOOCV = preds_LOOCV,
  preds_insample = preds_insample
)
head(results)

#Calculate RMSE
library(caret)
#RMSE Function (predicted, true)
RMSE(results$preds_insample,
     results$true)

RMSE(results$preds_LOOCV,
     results$true)

R2(results$preds_insample,
     results$true)

R2(results$preds_LOOCV,
     results$true)
