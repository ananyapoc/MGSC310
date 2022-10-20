#------------------------------------------------
### Bagging
#------------------------------------------------
# using Boston data
library(MASS)
data(Boston)

?Boston

library(tidyverse)

Boston %>% glimpse()

#install.packages("randomForest")
library(randomForest)


# using random forest function with mtry = p
library(rsample)
set.seed(310)
Boston_split <- initial_split(Boston, prop = 0.8)
Boston_train <- training(Boston_split)
Boston_test <- testing(Boston_split)

# performance of the model
bag.boston <- randomForest(medv ~ .,
                           mtry = 13,
                           data = Boston_train,
                           ntree = 500,
                           importance = TRUE)
print(bag.boston)

plot(bag.boston)

# performance of the model
preds_test_bag <- predict(bag.boston, newdata = Boston_test)
head(preds_test_bag)

# RMSE
library(caret)
RMSE(preds_test_bag, Boston_test$medv)

#------------------------------------------------
### Random Forest
#------------------------------------------------
# random forest with mtry = 4

rf.boston <- randomForest(medv ~ .,
                          data = Boston_train,
                          ntree = 500,
                          mtry = 4,
                          importance = TRUE)

print(rf.boston)
plot(rf.boston)
# performance of the model
RMSE(preds_test_rf, Boston_test$medv)


#------------------------------------------------
### Variable Importance
#------------------------------------------------
# importance
importance(rf.boston)
# importance plot
varImpPlot(rf.boston)
#------------------------------------------------
### Random Forest Explanations
#------------------------------------------------

#install.packages("randomForestExplainer")
library(randomForestExplainer)
library(ggplot2)

# plot min depth distribution
plot_min_depth_distribution(rf.boston)
# plot variable two-way importance measure
plot_multi_way_importance(rf.boston)
# plot variable two-way importance measure
# change x to "mse_increase" and y to "node_purity_increase"
plot_multi_way_importance(rf.boston, x_measure = "mse_increase",
                          y_measure = "node_purity_increase")
# plot two variables interaction effect: lstat and rm
plot_predict_interaction(rf.boston, Boston_train, "lstat", "rm")

# explanation file 
explain_forest(rf.boston, interactions=TRUE, data=Boston_train)


#---------------------------------------------------------------
# Random Forest Exercises (Classification)
#---------------------------------------------------------------
# for classification use type=classification in 
#   randomForest()
#
# 1. Estimate a random forest model predicting survival using the predictors 
#    Pclass, Sex, Age, SibSp and Fare. First set mtry = 5, and select ntree = 400. 
#    Call this model rf_fit
# 2. Call the print function against rf_fit
# 3. call the plot() function against the fitted model and describe the plot. 
# 4. How many trees should we use to estimate the model?
# 5. Generate a variable importance plot and explain the plot. 


