# MGSC 310 Exam #
# Spring 2021 #
# Prof. Shahryar Doosti #
# Ananya Pochiraju # 

#load all packages and libraries
library(tidyverse)
library(forcats)
library(ggplot2)
library(ggridges)
library(lindia)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(caret)
library(plotROC)
library(yardstick)
library(ggcorrplot)

# Question 1 
# Youtube data to predict Youtube video’s total views

# a. Download the datasets youtube_train.csv and youtube_test.csv. Load these 
# datasets as the objects youtube_train and youtube_test using the read.csv 
# function with the option stringsAsFactors = TRUE. The Youtube dataset 
# contains the following variables:

# genre: Main genre of the content creator
# us_creator: Whether the creator is American (1) or not (0)
# type: Type of content creator
# duration: Duration of the video (minutes)
# female_percent: Percentage of audience that are female
# age_13_17:Percentage of audience that are between 13 and 17 years old
# age_18_24: Percentage of audience that are between 14 and 24 years old
# age_25_34: Percentage of audience that are between 25 and 34 years old
# age_35_44: Percentage of audience that are between 35 and 44 years old
# age_45_55: Percentage of audience that are between 45 and 55 years old
# age_55_plus: Percentage of audience that are older than 55.
# us_audience: Percentage of audience that are US based.
# sponsored: Whether the video is sponsored (1) or not (0)
# monthly_views_growth: Creator’s monthly growth in viewership
# followers: Number of creator’s followers (in 1000)
# uploads_90d: Number of video uploads in past 90 days
# month: Month of video published date (1 to 12)
# dow: Day of week of published date (1 to 7)
# live: Whether the video is live (1) or not (0)
# views: Number of views for a video (in 1000)

youtube_train <- read.csv("Documents/MGSC310/datasets/youtube_train.csv", stringsAsFactors = TRUE)
youtube_test <- read.csv("Documents/MGSC310/datasets/youtube_test.csv", stringsAsFactors = TRUE)

# b. Use the glimpse function against the training and test datasets to 
# determine the data types of each dataset.

glimpse(youtube_train)
glimpse(youtube_test)

# c. Which variables are stored as factor variables? By looking at the variable 
# descriptions, what variables should be factor? Use mutate to change the type 
# of the variable to the appropriate type. (Hint: try both training and testing)

# The variables currently being stored as factor variables are only genre and
# type. The other variables that should be factor variables are us_creator, 
# sponsored, and live. 

youtube_train <- youtube_train %>% mutate (us_creator = factor(us_creator), 
                                           sponsored = factor(sponsored), 
                                            live = factor(live))
youtube_test <- youtube_test %>% mutate (us_creator = factor(us_creator),
                                          sponsored = factor(sponsored),
                                          live = factor(live))

# d. Using ggplot, produce a scatter plot of views by followers, faceted by 
# genre. Include a linear trend line for each plot. For which video genres are 
# the relationship between views and followers the strongest?

ggplot(data=youtube_train, mapping = aes(x = views, y = followers)) + 
  geom_point(alpha = 0.4) + theme_minimal() + 
  geom_smooth(method = "lm", se = F) + facet_wrap(~genre)

# The video genres where the relationship between views and followers is 
# strongest is food/drink, news/politics, and people/blogs. Entertainment could 
# also be part of this assumption. 

# e. Using the package ggridges produce a ridgeplot of views by genre. 
# Which genre seems to generate the most viewership? 
# Which seems the most dispersed?

ggplot(youtube_train, aes(x=views, y=genre, fill=genre)) + geom_density_ridges()

# The genre that seems to generate the most viewership is gaming, because the 
# views go up to the 500-750 thousand view range, which is much more than any of
# the other categories. The ones that seem most dispersed are people/blogs and 
# food/drink, because after the initial peak in both genres, there are still 
# views that trail into the 500 thousand range. 

# f. Using the group_by() and summarize() commands, confirm your suspicion by 
# calculating the mean and standard deviation of views for each genre. Which 
# genre has the highest average views? Which genre has the
# largest standard deviation?

youtube_train %>% group_by(genre) %>% summarize(avg_views = mean(views), 
                                                stdev_views = sd(views)) 

# The genre with the highest average views is Gaming, and the genre with the 
# largest standard deviation is Food and Drink. 

# g. Build a linear regression model to predict views as a function of 
# followers, duration, us_creator, and type. Store this model as lm_mod1 
# and print the model summary.

lm_mod1 <- lm(views ~ followers + duration + us_creator + type, data = 
                youtube_train)
summary(lm_mod1)

# h. Interpret the coefficients for the variables followers and duration. For 
# full credit you must give a complete answer.

# For every 1 unit increase in followers, the predicted views increase 
# by 91.49 thousand. For every 1 unit increase in duration, the predicted views 
# decrease by 0.557 thousand. 

# i. Interpret the coefficient on typeinfluencer.

# The base reference for type is brand, so a 1 unit increase in typeinfluencer
# increases views by 91.49 thousand more than if the type were to be brand. 

# j. What variables are statistically significant? What is the meaning of 
# p-value? What is the p-value for us_creator?

# The variables that are statistically significant are followers, duration,
# typeinfluencer, and typemedia company. A p-value signifies how likely 
# the results observed were able to be seen due to chance. The p-value for 
# us_creator was 0.601, which is greater than the usual significance value of 
# 0.05, so I had to conclude that us_creator does not have a significant role 
# in determining the number of views. 

# k. Use the mutate function to create the variable lviews which is the log of 
# the views. Estimate a regression of log of views (lviews) against followers, 
# duration, us_creator, and type. Store this model as lm_mod2 and print 
# the summary of the model.

youtube_train <- youtube_train %>% mutate(lviews = log10(views))
lm_mod2 <- lm(lviews ~ followers + duration + us_creator + type, data = 
                youtube_train)
summary(lm_mod2)

# l. Compare goodness of fit of lm_mod1 and lm_mod2 by comparing 𝑅2
# Which model has a better fit to the training data?

# The fit of lm_mod1 was better than the fit of lm_mod2 because the r2 on mod1 
# was 0.457, and the r2 on mod2 was 0.397. The higher the r2, the better. 

# m. Plot the residual versus fitted values for both lm_mod1 and lm_mod2 using 
# lindia package. Are they homoskedastic or heteroskedastic?

gg_resfitted(lm_mod1)
gg_resfitted(lm_mod2)

# The errors are not evenly spread across the graph for mod1, but they are more
# spread evenly for mod2, so I would say that mod1 is heteroskedastic 
# and mod2 is homoskedastic. 
#------------------------------------------------------------------------------
# Question 2: Regularized Views & Regressions 

# a. Use the select command to remove lviews from the training dataset. 
# Estimate a ridge regression model using the youtube data to predict views 
# using all other variables. Store this model as ridge_mod. Call the print 
# command against the ridge_mod when you are done.

youtube_train <- youtube_train %>% select(-lviews)

ridge_mod <- cv.glmnet(views ~ ., data = youtube_train, alpha = 0)
print(ridge_mod)

# b. Print the values for lambda.min and lambda.1se. Explain the meaning of 
# lambda.min and lambda.1se. Which one has more bias?

print(ridge_mod$lambda.min)
print(ridge_mod$lambda.1se)

# lambda.min stores the value of lambda that minimizes cross-validated error.
# lambda.lse stores the value of lambda that minimizes cross-validated error 
# + 1 estimated standard error. lambda.lse has more bias. 

# c. Estimate a lasso model against the same dataset using the same equation. 
# Store this as lasso_mod and call the print function against the model.

lasso_mod <- cv.glmnet(views ~ ., data = youtube_train, alpha = 1)
print(lasso_mod)

# d. Call the plot function against the lasso model and describe the 
# plot as you would to a non-expert.

plot(lasso_mod)

# This graph is showing the lambda, or penalty, against the cross validated 
# error rate. The first vertical dashed line is at 26, which is where the 
# lambda.min value is, and the second vertical dashed line is at 17, which is 
# where the lambda.lse value is. 

# e. Estimate a linear model using the same model against the same dataset. 
# Store this model as lm_mod3 and call the summary command against the model.

lm_mod3 <- lm(views ~ ., data = youtube_train)
summary(lm_mod3)

# f. Print the coefficients of Lasso model for lambda.min and lambda.1se. 
# How many non-zero variables are in each model?

lasso_coefs <- data.frame(
  lasso_min = coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
    round(3) %>% as.matrix() ,
  lasso_1se = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() 
) %>% rename(lasso_min = 1, lasso_1se = 2)

print(lasso_coefs)

count_min <- count(filter(lasso_coefs, lasso_min != 0))
count_1se <- count(filter(lasso_coefs, lasso_1se != 0))

count_min
count_1se

# The number of non-zero coefficients for lambda.min: 27
# The number of non-zero coefficients for lasso_1se: 19

# g. Which variables of the Lasso model with lambda.min have been shrunk to 
# zero? Explain this result as you would to a non-expert.

# The variables that were shrunk to 0 were whether the genre was people/blogs, 
# whether the creator was American, whether the creator was from a
# media company, the age range of the audience between 14 and 24, whether the 
# creator was sponsored, and whether they were live. These variables were 
# determined to not have much effect on the overall performance of the model,
# so they were 0'ed out. 

# h. Generate a prediction data frame that holds the training predictions and 
# the test predictions for three models (OLS, Ridge, Lasso) and the true 
# values. Use the lambda.1se value for shrinkage for the regularized models. 
# You may want to create two dataframes, one for the test predictions, and 
# one for the training predictions. Then, using head function, print the 
# first rows for the training and testing results.

pred_train_OLS <-  predict(lm_mod3, newdata = youtube_train)
pred_train_ridge <- predict(ridge_mod, youtube_train, s=ridge_mod$lambda.min)
pred_train_lasso <- predict(lasso_mod, youtube_train, s=lasso_mod$lambda.min)

training_preds <- data.frame("true_all" = youtube_train$views, 
                             "pred_OLS" = pred_train_OLS,
                             "ridge" = pred_train_ridge,
                             "lasso" = pred_train_lasso)
head(training_preds)

pred_test_OLS <-  predict(lm_mod3, newdata = youtube_test)
pred_test_ridge <- predict(ridge_mod, youtube_test, s=ridge_mod$lambda.min)
pred_test_lasso <- predict(lasso_mod, youtube_test, s=lasso_mod$lambda.min)

testing_preds <- data.frame("true_all2" = youtube_test$views, 
                            "pred_OLS2" = pred_test_OLS,
                            "ridge2" = pred_test_ridge,
                            "lasso2" = pred_test_lasso)
head(testing_preds)

# i. Plot the predicted vs. true plots for the three models. Be sure your x 
# and y limits are set to be between 0 and 500.

ggplot(training_preds, aes(x=true_all, y=pred_OLS)) + geom_point() + 
  ggtitle("OLS") + xlim(0,500) + ylim(0,500)
ggplot(training_preds, aes(x=true_all, y=X1)) + geom_point() +
  ggtitle("Ridge") + xlim(0,500) + ylim(0,500)
ggplot(training_preds, aes(x=true_all, y=X1.1)) + geom_point() +
  ggtitle("Lasso") + xlim(0,500) + ylim(0,500)

ggplot(testing_preds, aes(x=true_all2, y=pred_OLS2)) + geom_point() +
  ggtitle("OLS Test") + xlim(0,500) + ylim(0,500)
ggplot(testing_preds, aes(x=true_all2, y=X1)) + geom_point() +
  ggtitle("Ridge Test") + xlim(0,500) + ylim(0,500)
ggplot(testing_preds, aes(x=true_all2, y=X1.1)) + geom_point() +
  ggtitle("Lasso Test") + xlim(0,500) + ylim(0,500)

# j. Calculate root mean squared error (rmse) in the testing and training sets 
# for the OLS, Ridge, and Lasso models. Is the model overfit or underfit and 
# if so how do we know? Which model performs the best?

RMSE(pred=training_preds$pred_OLS,obs=youtube_train$views)
RMSE(pred=training_preds$X1,obs=youtube_train$views)
RMSE(pred=training_preds$X1.1,obs=youtube_train$views)

RMSE(pred=testing_preds$pred_OLS2,obs=youtube_test$views)
RMSE(pred=testing_preds$X1,obs=youtube_test$views)
RMSE(pred=testing_preds$X1.1,obs=youtube_test$views)

# All 3 models have a higher train error than test error which 
# means that none of the models are overfit. If they were to be overfit then 
# the train error would be less than the test error because the model has 
# been adjusted to fit the bias of the dataset. I believe that all the models 
# performed similarly so there is no exact right answer for which model did the
# best out of all 3 of them, but since ridge and lasso had cross-validation,
# i would assume that either of those are the better performing models. 

# k. If the goal is to maximize the exposure of the videos (views), what would 
# you suggest as the best strategy to the content creators? What variables 
# affect the viewership? What would you recommend as to the best model to 
# predict views? How confident would you feel about your best model?

# The best strategies would be to label yourselves as influencers and to also 
# produce more gaming content. Those two variables positively affected the views
# the most. In terms of the best model to predict views, I found that since all
# 3 models had similar RMSE scores, that it would not make a difference between 
# using OLS or ridge or lasso, however since ridge and lasso did have 
# cross validation, it would be the smarter choice to use one of those. I am 
# very confident about all my models. 

#------------------------------------------------------------------------------
# Question 3
# Breast cancer data to predict benign or malignant type

# a. Download the files cancer_train.csv and cancer_test.csv. 
# Load them into R and store them as the objects cancer_train and cancer_test. 
# The datasets contain ten real-valued features computed for each cell nucleus 
# of abnormal lump described below:

# id: ID number
# radius: mean of distances from center to points on the perimeter
# texture: standard deviation of gray-scale values
# perimeter
# area
# smoothness: local variation in radius lengths
# compactness: perimeter^2 / area - 1.0
# concavity: severity of concave portions of the contour
# concave points: number of concave portions of the contour
# symmetry
# fractal dimension: “coastline approximation” - 1
# malignant: whether the diagnosis is benign (0) or malignant (1)

cancer_train <- read.csv("datasets/cancer_train.csv", stringsAsFactors = TRUE)
cancer_test <- read.csv("datasets/cancer_test.csv", stringsAsFactors = TRUE)

# b. Run glimpse() over the dataframe to ensure the data 
# have been loaded in the proper format.

glimpse(cancer_train)
glimpse(cancer_test)

# c. Print the correlation matrix and the correlation plot on the training set. 
# What variables have the highest correlation with malignant?

corr.table <- cor(cancer_train)
ggcorrplot(corr.table, type = 'lower')

# The variables with the highest correlation are radius, perimeter, and area.

# d. Estimate a logistic model to predict malignant. Use all the variable 
# except id. Print and summarize the output.

cancer_logit <- glm(malignant ~ radius + texture + perimeter + area + 
                      smoothness + compactness + concavity + concave.points +
                      symmetry + fractal, family = binomial,
                    data=cancer_train)

summary(cancer_logit)

# e. Interpret the coefficients on radius and texture. What variables seem to 
# increase the risk of malignant diagnosis?

options(scipen = 999)

# convert coef to odds ratio
exp(cancer_logit$coefficients)
#radius
print(1-0.216000913639093)
#texture
print(1-1.388574447156443)

# As radius increases by 1 unit,  the odds of the cancer being malignant
# multiply by 78.3%. As texture increases by 1 unit, the odds of the cancer 
# being malignant decrease by 38.8%. 

# f. What variables seem to be more impactful on the diagnosis? Why? 

# The variables that seem more impactful are radius and compactness, because 
# those two had exponent values less or close to 1, so when calculating the odds
# ratio for those numbers, the ratios will be positive, which means they will
# show an increased risk of a malignant diagnosis. 

# g. Describe what False Positive and False Negative are in this problem. 
# Which one is more costly?

# False positive is when someone is said to have a malignant cancer when they
# do not, and false negative is when someone is said to not have a malignant
# cancer but they actually do. The one that is more costly is the false 
# negative because then it goes undetected. 

# h. “Score” the model by generating predicted values for the 
# test and training sets.

scores_train <- predict(cancer_logit, type = "response")
pred_train_classes <- ifelse(scores_train > 0.5,"Yes","No")
head(pred_train_classes)

scores_test <- predict(cancer_logit, newdata=cancer_test, type = "response")
pred_test_classes <- ifelse(scores_test > 0.5,"Yes","No")
head(pred_test_classes)

# i. Generate ROC curves for the test and training sets. Be sure to give them 
# a title so we can distinguish them.

results_train <- data.frame(truth=cancer_train$malignant,
                            Class1 = scores_train,
                            Class2 = 1-scores_train,
                            pred_train = as_factor(pred_train_classes))

p_train <- ggplot(results_train, aes(m=Class1, d=truth)) +   
  geom_roc(cutoffs.at = c(0.99,0.9,0.7,0.5,0.3,0.1,0)) + ggtitle("ROC TRAIN")

print(p_train)

results_test <- data.frame(truth=cancer_test$malignant,
                           Class.1 = scores_test, Class.2 = 1-scores_test,
                           pred_test = as_factor(pred_test_classes))

p_test <- ggplot(results_test, aes(m=Class.1, d=truth)) +
  geom_roc(cutoffs.at = c(0.99,0.9,0.7,0.5,0.3,0.1,0)) + ggtitle("ROC TEST")

print(p_test)

# j. Calculate the AUC in the testing and training sets. Does the AUC and 
# ROC indicate the model is over or underfit? How do we know?

calc_auc(p_train)
calc_auc(p_test)

# The model has a good fit because the testing and training AUC are very close 
# to one another. It would be considered overfit if the size of the gap between 
# the two was large and it would be considered underfit if there was no gap at 
# all. In this case, we have a good model because there is a very small gap 
# between test and train.

# k. Choose a reasonable cutoff threshold and produce confusion matrices.

pred_train_classes <- ifelse(scores_train > 0.5,"Yes","No")

results_train <- data.frame(truth=cancer_train$malignant, 
                            truth_factor = factor(ifelse
                            (cancer_train$malignant==1, "Yes", "No")),
                            Class1 = scores_train,
                            Class2 = 1-scores_train,
                            pred_train = factor(ifelse(scores_train > 0.5,
                                                       "Yes","No")))

cm_train <- conf_mat(results_train, truth = truth_factor, estimate = pred_train)
cm_train

pred_test_classes <- ifelse(scores_test > 0.5,"Yes","No")

results_test <- data.frame(truth=cancer_test$malignant,
                           truth_factor = factor(ifelse
                          (cancer_test$malignant==1, "Yes", "No")),
                           Class.1 = scores_test, Class.2 = 1-scores_test,
                           pred_test = factor(ifelse(scores_test > 0.5,
                                                     "Yes","No")))

cm_test <- conf_mat(results_test, truth = truth_factor, estimate = pred_test)
cm_test

# l. Calculate the accuracy, sensitivity and specificity in the test set only. 
# Is the model more accurate for the positives or for the negatives?

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

# The model is more accurate for the negatives because specificity is higher.




