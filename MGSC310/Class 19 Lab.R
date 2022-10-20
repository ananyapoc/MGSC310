#------------------------------------------------
### Regression Tree
#------------------------------------------------
library(tidyverse)
library(ISLR)

# Load Hitters data from ISLR

data("Hitters")

Hitters %>% glimpse()

summary(Hitters)

# Do necessary data transformation:
#   - dropping missing

colSums(is.na(Hitters))
Hitters_clean <- Hitters %>% drop_na(Salary)

# Split data into training (75%) and test (25%)

library(rsample)
set.seed(2021)
Hitters_split <- initial_split(Hitters_clean, prop=0.75)
Hitters_train <- training(Hitters_split)
Hitters_test <- testing(Hitters_split)

dim(Hitters_split)

# Train the tree

# outcome: Salary
# predictors: AtBat + Hits + HmRun + RBI + Walks + Years + Division 
# + Assists + Errors

library(tree)

tree_hitters <- tree(Salary ~ AtBat + Hits + HmRun + RBI + Walks + Years 
                     + Division + Assists + Errors, data = Hitters_train)

# Plot the tree

plot(tree_hitters)
text(tree_hitters)

# Predict the test

preds_test <- predict(tree_hitters, newdata = Hitters_test)

head(preds_test)

# Measure RMSE test

library(caret)

RMSE(preds_test, Hitters_test$Salary)

# cross-validation to find best tree size

tree_hitters_cv <- cv.tree(tree_hitters)

# report the results

tree_hitters_cv

# plot the error vs tree 

plot(tree_hitters_cv$size, tree_hitters_cv$dev, type='b')

# prune the tree by the best size

pruned_tree <- prune.tree(tree_hitters, best = 5)

# plot the pruned tree (plot for the best sub-tree)

plot(pruned_tree)
text(pruned_tree)

# Predict the test

preds_test_pruned <- predict(pruned_tree, newdata = Hitters_test)

RMSE(preds_test_pruned, Hitters_test$Salary)

#------------------------------------------------
### In-Class Exercise: Classification Tree
#------------------------------------------------
# Load the titanic dataset. Convert Survived, Pclass, and Sex into 
#    factor. Split the dataset into training (75%) and test (25%)
titanic <- read.csv("datasets/titanic.csv")
titanic <- titanic %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

set.seed(2021)
titanic_split <- initial_split(titanic, prop=0.75)
titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)

# 1. Estimate a classification tree model predicting survival as a function 
#    of Sex, Pclass, Age, SibSp and Fare paid using the tree command.
#    Store this model as tree_titanic
# 2. Use the plot function on the fitted object to produce the tree plot.
#    Use text function to show the labels for the nodes and splits.
#    You can use option pretty=0 in the text function to show levels of factors.
# 3. Who has the best chance of survival?
# 4. Use predict function to predict the survival for the test set.
#    Use the option type = 'class' in predict function for classification.
# 5. If you have time, use cv.tree function to do cross-validation for
#    different size of tree. Store the results in tree_titanic_cv object.
#    Plot the cross-validation error the tree size. 
#    What is the best tree size?
