#Final Project#
#Group Members: Ananya Pochiraju, Kashish Pandey, Nikita Shetty, Kimia Abolhoda

#load all libraries
library(tidyverse)
library(ggplot2)
library(rsample)
library(forcats)
library(caret)
library(yardstick)
library(glmnet)
library(glmnetUtils)
library(plotROC)
library(coefplot)
library(ROSE)
library(ggcorrplot)

#load dataset
stroke <- read.csv("datasets/healthcare-dataset-stroke-data.csv", 
                        stringsAsFactors = TRUE)
glimpse(stroke)

#visualization
corr.table <- cor(stroke %>% select(age, hypertension, heart_disease, 
                                    avg_glucose_level, stroke))

ggcorrplot(corr.table, type = 'lower')

#data cleaning
stroke_data <-  stroke %>% filter(bmi != "N/A", gender != "Other") %>% 
  mutate(bmi = as.integer(bmi), hypertension = as.factor(hypertension),
         heart_disease = as.factor(heart_disease), stroke = as.factor(stroke))

glimpse(stroke_data)
summary(stroke_data)

stroke_upsampling <- ROSE(stroke ~.,
                     data = stroke_data,
                     N = 1000,
                     p = 1/5)

table(stroke_upsampling$data$stroke)
glimpse(stroke_upsampling)

#split the dataset for the train-test split
set.seed(310)
stroke_data_split <- initial_split(data = stroke_upsampling$data, prop = 0.80)
stroke_data_train <- training(stroke_data_split)
stroke_data_test <- testing(stroke_data_split)

dim(stroke_data_split)

##################################
# LOGISTIC REGRESSION MODEL
##################################

logit1 <- glm(stroke ~ gender + age + hypertension + heart_disease + 
                ever_married + work_type + Residence_type +
                avg_glucose_level + bmi + smoking_status,
              data = stroke_data_train,
              family = binomial)

summary(logit1)

# to force R not to use scientific format
options(scipen = 999)

# exp(coefficients) to convert to odds
exp(logit1$coefficients)

#rounding numbers
round(exp(logit1$coefficients), 3)

#generating predicted probabilities for test/training sets 
scores_train <- predict(logit1, type = "response")

pred_train_classes <- ifelse(scores_train > 0.15, 1, 0 )
head(pred_train_classes)

#testing
scores_test <- predict(logit1, type = "response", newdata = stroke_data_test)

pred_test_classes <- ifelse(scores_test > 0.15, 1, 0)
head(pred_test_classes)

#roc dataframes
roc_train_log <- data.frame(truth = as.integer(stroke_data_train$stroke), 
              Class1 = scores_train, Class2 = 1 - scores_train)

roc_test_log <- data.frame(truth = as.integer(stroke_data_test$stroke), 
              Class1.1 = scores_test, Class1.2 = 1 - scores_test)

# generating ROC curves
p_train_log <- ggplot(roc_train_log, aes(m=scores_train, d=truth)) + 
  geom_roc(cutoffs.at = c(0.9, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.01))

print(p_train_log)

calc_auc(p_train_log)

#testing
p_test_log <- ggplot(roc_test_log, aes(m=scores_test, d=truth)) +
  geom_roc(cutoffs.at = c(0.9, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.01))

print(p_test_log)

calc_auc(p_test_log)

# Calculate and print the confusion matrices for the test and training sets.
results_train <- data.frame(truth=stroke_data_train$stroke,
    Class1 = scores_train, Class2 = 1-scores_train,
    pred_train = factor(pred_train_classes))

cm_train <- conf_mat(results_train, truth = truth, estimate = pred_train)
print(cm_train)

#testing
results_test <- data.frame(truth=stroke_data_test$stroke,
    Class.1 = scores_test, Class.2 = 1-scores_test,
    pred_test = factor(pred_test_classes))

cm_test <- conf_mat(results_test, truth = truth, estimate = pred_test)
print(cm_test)

# Calculate the accuracy, sensitivity and specificity for the testing and 
#training confusion matrices.

#accuracy
cm_table_train <- cm_train$table
#accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (cm_table_train[1,1] + cm_table_train[2,2])/sum(cm_table_train)
print(acc)

#sensitivity = TP/P = TP/(TP+FN)
sen <- cm_table_train[2,2]/sum(cm_table_train[,2])
print(sen)

#specificity = TN/N = TN/(TN+FP)
spe <- cm_table_train[1,1]/sum(cm_table_train[,1])
print(spe)

#testing

#accuracy
cm_table_test <- cm_test$table
#accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (cm_table_test[1,1] + cm_table_test[2,2])/sum(cm_table_test)
print(acc)

#sensitivity = TP/P = TP/(TP+FN)
sen <- cm_table_test[2,2]/sum(cm_table_test[,2])
print(sen)

#specificity = TN/N = TN/(TN+FP)
spe <- cm_table_test[1,1]/sum(cm_table_test[,1])
print(spe)

# Interpretation of the Model
# The model is very good at determining when a patient has a stroke, 
# which is very important for the purposes of our project because we want to 
# know when a person is likely to have a stroke based on the variables present,
# not when they won't have a stroke. The accuracy of the
# models were around 69.2% for the training data and 72.5% for the testing data. 
# The model is not overfit because the training data accuracy is lower than 
# the testing data. This means this model can be useful and provide helpful 
# information on variables that can predict susceptibility to having a stroke. 
# What was also great about the logistic model results is that sensitivity was
# high, so the model is much better at predicting a person actually
# having a stroke and not mistaking it for not having a stroke, which would be
# very dangerous in a real world application. 

##################################
# RIDGE MODEL
##################################

ridge_mod <- cv.glmnet(stroke ~ .,
                       data = stroke_upsampling$data,
                       alpha = 0,
                       family = binomial)

# print the two model suggested values of lambda:
print(ridge_mod$lambda.min)
print(ridge_mod$lambda.1se)

# print coefficient using lambda.min
coef(ridge_mod, s = ridge_mod$lambda.min) %>% round(3)

# print coefficient using lambda.1se
coef(ridge_mod, s = ridge_mod$lambda.1se) %>% round(3)

# put into coefficient vector
ridge_coefs <- data.frame(
  ridge_min = coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
    round(3) %>% as.matrix() ,
  ridge_1se = coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
    round(3) %>% as.matrix()) %>% rename(ridge_min = 1, ridge_1se = 2)

ridge_coefs

# use the plot function to see the MSE
# path as we vary lambda (the amount of penalization)
plot(ridge_mod)

### examine coefficient shrinkage path
coefpath(ridge_mod)

# prediction

#generating predicted probabilities for test/training sets 
pred_train_ridge <- predict(ridge_mod, s = ridge_mod$lambda.min, 
                             stroke_data_train, type = "response")

pred_train_classes_ridge <- ifelse(pred_train_ridge > 0.2, 1, 0 )
head(pred_train_classes_ridge)

#testing
pred_test_ridge <- predict(ridge_mod, s = ridge_mod$lambda.min, 
                            stroke_data_test, type = "response")

pred_test_classes_ridge <- ifelse(pred_test_ridge > 0.2, 1, 0)
head(pred_test_classes_ridge)

#creating ROC dataframes
roc_train_ridge <- data.frame(truth = as.integer(stroke_data_train$stroke), 
                Class1.1 = pred_train_ridge, Class2.1 = 1 - pred_train_ridge)

roc_test_ridge <- data.frame(truth = as.integer(stroke_data_test$stroke), 
                  Class1.2 = pred_test_ridge, Class2.2 = 1 - pred_test_ridge)

# generating ROC curves
p_train_ridge <- ggplot(roc_train_ridge, aes(m=pred_train_ridge, d=truth)) + 
  geom_roc(cutoffs.at = c(0.9, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.01))

print(p_train_ridge)

calc_auc(p_train_ridge)

#testing
p_test_ridge <- ggplot(roc_test_ridge, aes(m=pred_test_ridge, d=truth)) + 
  geom_roc(cutoffs.at = c(0.9, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.01))

print(p_test_ridge)

calc_auc(p_test_ridge)

# formatting our results dataframes
results_train_ridge <- data.frame(truth = factor(stroke_data_train$stroke),
  Class1 = pred_train_ridge,
  Class2 = 1 - pred_train_ridge,
  pred_train_ridge = factor(pred_train_classes_ridge))

head(results_train_ridge)

#testing
results_test_ridge <- data.frame(
  truth = factor(stroke_data_test$stroke),
  Class1 = pred_test_ridge,
  Class2 = 1 - pred_test_ridge,
  pred_test_ridge = factor(pred_test_classes_ridge))

head(results_test_ridge)

#creating the confusion matrices

levels(results_train_ridge$truth)
levels(results_train_ridge$pred_train_ridge)

cm1_ridge <- conf_mat(results_train_ridge, truth = truth, 
             estimate = pred_train_ridge)

print(cm1_ridge)

#testing
cm2_ridge <- conf_mat(results_test_ridge, truth = truth,
              estimate = pred_test_ridge)

print(cm2_ridge)

#accuracy
cm1_ridge_train <- cm1_ridge$table
#accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (cm1_ridge_train[1,1] + cm1_ridge_train[2,2])/sum(cm1_ridge_train)
print(acc)

#sensitivity = TP/P = TP/(TP+FN)
sen <- cm1_ridge_train[2,2]/sum(cm1_ridge_train[,2])
print(sen)

#specificity = TN/N = TN/(TN+FP)
spe <- cm1_ridge_train[1,1]/sum(cm1_ridge_train[,1])
print(spe)

#testing

#accuracy
cm2_ridge_test <- cm2_ridge$table
#accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (cm2_ridge_test[1,1] + cm2_ridge_test[2,2])/sum(cm2_ridge_test)
print(acc)

#sensitivity = TP/P = TP/(TP+FN)
sen <- cm_table_test[2,2]/sum(cm_table_test[,2])
print(sen)

#specificity = TN/N = TN/(TN+FP)
spe <- cm_table_test[1,1]/sum(cm_table_test[,1])
print(spe)

# Interpretation of the Model
# The ridge model is good at determining when a patient is more likely to have 
# a stroke from what we could gather from the confusion matrices. The accuracy
# of the models were average as well, with accuracy around 72.8% for the 
# training data and 75.5% for the testing data. Since the training data accuracy 
# is lower than testing data accuracy the model is not overfit. However, we do
# not believe that ridge provided the best results to help answer our question 
# and motivation because the coefpath plot shows all the coefficients eventually 
# becoming 0s, but at the same rate. This meant that there weren't enough splits 
# between when the model made each coefficient very small, so there were no 
# conclusive results on which variables had the most impact.

#############################
# LASSO MODEL
##############################

# note cv.glmnet automatically performs k-fold cross-validation 

lasso_mod <- cv.glmnet(stroke ~ ., data = stroke_upsampling$data, alpha = 1,
                       family = binomial)

# print the two model suggested values of lambda:
print(lasso_mod$lambda.min)
print(lasso_mod$lambda.1se)

# to examine the coefficients we must say what value of lambda we want to use.
coef(lasso_mod, s = lasso_mod$lambda.min) %>% round(3)

# print coefficient using lambda.1se
coef(lasso_mod, s = lasso_mod$lambda.1se) %>% round(3)

# put into coefficient vector
lasso_coefs <- data.frame(
  lasso_min = coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
    round(3) %>% as.matrix() ,
  lasso_1se = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    round(3) %>% as.matrix()) %>% rename(lasso_min = 1, lasso_1se = 2)

print(lasso_coefs)

# use the plot function to see the 
# MSE path as we vary lambda (the amount of penalization)
plot(lasso_mod)

# examine coefficient shrinkage path
coefpath(lasso_mod)

# number of non-zero predictors
lasso_coefs %>% head()

# for lambda.min
lasso_coefs %>% 
  select(lasso_min) %>% 
  filter(lasso_min!=0) %>% 
  nrow()

# for lambda.1se
lasso_coefs %>% 
  select(lasso_1se) %>% 
  filter(lasso_1se!=0) %>% 
  nrow()

# prediction

# generating predicted probabilities for test/training sets 
pred_ls_train <- predict(lasso_mod, s = lasso_mod$lambda.min,stroke_data_train,
                         type = "response")

pred_train_ls_classes <- ifelse(pred_ls_train > 0.25, 1, 0 )
head(pred_train_ls_classes)

#testing
pred_ls_test <- predict(lasso_mod, s = lasso_mod$lambda.min,stroke_data_test,
                        type = "response")

pred_test_ls_classes <- ifelse(pred_ls_test > 0.25, 1, 0)
head(pred_test_ls_classes)

# formatting our results dataframes
results_train_ls <- data.frame(truth = factor(stroke_data_train$stroke),
     Class1 = pred_ls_train, Class2 = 1 - pred_ls_train,
    predicted_ls_train = factor(pred_train_ls_classes, levels = c(0,1)))

head(results_train_ls)

#testing
results_test_ls <- data.frame(truth = factor(stroke_data_test$stroke),
    Class1 = pred_ls_test, Class2 = 1 - pred_ls_test,
    predicted_ls_test = factor(pred_test_ls_classes, levels = c(0,1)))

head(results_test_ls)

# creating dataframes for the ROC Curves
roc_train_ls <- data.frame(truth = as.integer(stroke_data_train$stroke), 
                    Class1.2.1 = pred_ls_train, Class2.2.1 = 1 - pred_ls_train)

roc_test_ls <- data.frame(truth = as.integer(stroke_data_test$stroke), 
                    Class1.2.2 = pred_ls_test, Class2.2.2 = 1 - pred_ls_test)

# generating ROC curve
p_train_ls <- ggplot(roc_train_ls, aes(m=pred_ls_train, d = truth)) + 
  geom_roc(cutoffs.at = c(0.9, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.01))

print(p_train_ls)

calc_auc(p_train_ls)

#testing
p_test_ls <- ggplot(roc_test_ls, aes(m=pred_ls_test, d=truth)) +
  geom_roc(cutoffs.at = c(0.9, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.01))

print(p_test_ls)

calc_auc(p_test_ls)

# creating the confusion matrices
levels(results_train_ls$truth)
levels(results_train_ls$predicted_ls_train)

#training
cm1_lasso <- conf_mat(results_train_ls, truth = truth,
             estimate = predicted_ls_train)

print(cm1_lasso)

#testing
cm2_lasso <- conf_mat(results_test_ls, truth = truth,
             estimate = predicted_ls_test)
print(cm2_lasso)

#accuracy
cm1_lasso <- cm1_lasso$table
#accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (cm1_lasso[1,1] + cm1_lasso[2,2])/sum(cm1_lasso)
print(acc)

#sensitivity = TP/P = TP/(TP+FN)
sen <- cm1_lasso[2,2]/sum(cm1_lasso[,2])
print(sen)

#specificity = TN/N = TN/(TN+FP)
spe <- cm1_lasso[1,1]/sum(cm1_lasso[,1])
print(spe)

#testing 

#accuracy
cm2_lasso <- cm2_lasso$table
#accuracy = (TN+TP)/(TN+TP+FN+FP)
acc <- (cm2_lasso[1,1] + cm2_lasso[2,2])/sum(cm2_lasso)
print(acc)

#sensitivity = TP/P = TP/(TP+FN)
sen <- cm2_lasso[2,2]/sum(cm2_lasso[,2])
print(sen)

#specificity = TN/N = TN/(TN+FP)
spe <- cm2_lasso[1,1]/sum(cm2_lasso[,1])
print(spe)

# Interpretation of the Model
# The lasso model is good at determining when a patient is more likely to have
# a stroke from what we could gather from the confusion matrices. The accuracy 
# of the models were different from each other, since accuracy was around 75.9% 
# for the training data and 80.4% for the testing data. Since the training data 
# accuracy is not higher than testing data accuracy the model is not overfit. 
# We believe that the model performed well! After interpreting the coefplot, 
# the most important variables in determining whether someone will have a 
# stroke or not are age, hypertension0, heart_disease0, and 
# average blood glucose level.

