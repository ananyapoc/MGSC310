#Class 12 Lab#
#Classification#

library(tidyverse)
library(ISLR)

data(Default)

#logistic model
#default ~ balance + income + student

log1 <- glm(default ~ balance + income + student, data = Default, family = binomial)

summary(log1)

#interpretation 

#exp(coefficients)
exp(log1$coefficients)

#to force r to change scientific notation
options(scipen=999)
exp(log1$coefficients)

#to round numbers to 3 decimal places 
round(exp(log1$coefficients),3)

#predicting outcome probabilities (scoring)
scores <- predict(log1, type = 'response')

head(scores)

glimpse(Default)

#add the predicted probabilities to the dataframe 
preds_DF <- data.frame(scores = scores, Default)

head(preds_DF)
