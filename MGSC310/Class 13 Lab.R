#Class 13 Lab#

#cleaning environment
rm(list = ls())

#load ISLR library
library(ISLR)

#load data which has credit card information 
data(Default)
head(Default)

#train a logit model default ~ balance 
#make sure to use glm() and set family = binomial 
logit_fit <- glm(default ~ balance, family = binomial, data = Default)

summary(logit_fit)

#force R to avoid using scientific format
options(scipen = 9)

#convert coefficients to odds ratio
exp(logit_fit$coefficients)

#generate predictions for outcome probabilities 
scores <- predict(logit_fit, type = "response")

predicted_classes <- ifelse(scores > 0.5, "Yes", "No")

head(predicted_classes)

#creating the confusion matrix 
library(yardstick)

#formatting our results dataframe 
results_logit <- data.frame(truth = factor(Default$default), Class1 = scores, 
                            Class2 = 1-scores, 
                            predicted = factor(predicted_classes))

head(results_logit)

#creating a confusion matrix pt2
cm <- conf_mat(results_logit, truth = truth, estimate = predicted)

print(cm)
summary(cm)

#generating the ROC Curve
library(ggplot2)
library(plotROC)

p <- ggplot(data = results_logit, aes(m=Class1, d=truth)) + 
  geom_roc(cutoffs.at = c(0.99, 0.7, 0.6, 0.5, 0.3, 0.1, 0.01))

print(p)
calc_auc(p)
