ebay <- read.csv("datasets/eBay.csv")

library(tidyverse)
ebay %>% glimpse()

ebay_clean <- ebay %>% mutate(Competitive = factor(Competitive))

#1
ggplot(ebay_clean, aes(x = OpenPrice, y = sellerRating, color = Competitive)) + 
  geom_point() + xlab("Open Price (Dollars)") + ylab("Seller Rating") +
  ggtitle("Seller Rating vs. Auction Open Price")

#2
ebay_clean %>% group_by(Competitive) %>% summarize(avg_openprice = 
                                                    mean(OpenPrice, na.rm=T), 
                                                  avg_duration = mean(Duration,
                                                                      na.rm=T))

#3
library(rsample)

set.seed(310)
ebay_split <- initial_split(ebay_clean, prop = 0.7)
ebay_train <- training(ebay_split)
ebay_test <- testing(ebay_split)

dim(ebay_split)

#4
logit_mod <- glm(Competitive ~ ., data = ebay_train, family = binomial)

summary(logit_mod)

#5
exp(logit_mod$coefficients)

#6
scores_train <- predict(logit_mod, type = "response")

scores_test <- predict(logit_mod, type = "response", newdata = ebay_test)

#create a results dataframe#

results_train <- data.frame(truth = ebay_train$Competitive, truth_binary =
                              as.integer(ebay_train$Competitive),
                            scores = scores_train)

results_test <- data.frame(truth = ebay_test$Competitive, truth_binary =
                             as.integer(ebay_test$Competitive),
                           scores = scores_test)

results_test %>% head()

#7
library(plotROC)

roc_train <- ggplot(results_train, aes(m=scores, d=truth_binary)) + geom_roc(
  cutoffs.at = c(0.1,0.2,0.3,0.5,0.6,0.8,0.9)) + ggtitle("ROC Plot Training")

print(roc_train)

calc_auc(roc_train)

#8
#ROC Test

#9 cutoff = 0.6
classes_train <- ifelse(results_train$scores > 0.6, 1, 0)

results_train$classes_binary <- classes_train
results_train$classes_train <- factor(classes_train)

head(results_train)

#diy for test

library(yardstick)
cm_train <- conf_mat(results_train, truth = truth, estimate = classes_train)
#ROC is binary, CM is factor 

print(cm_train)

#10
#Accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (480+281)/(480+281+471+149)
acc

#Sensitivity = (TP)/(TP+FN)
sen <- (281)/(281+471)
sen

#Specificity = (TN)/(TN+FP)
spec <- (480)/(480+149)
spec

#better at catching negative values rather than positive values


















