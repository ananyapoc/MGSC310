#Problem Set 3#
#Kashish Pandey, Ananya Pochiraju, Nikita Shetty#

#load all libraries here
library(tidyverse)
library(forcats)
library(rsample)
library(ggplot2)
library(lindia)
library(caret)
library(plotROC)
library(yardstick)

#Question 1: Movie Residuals and Predicted Values

# 1a. Run the cleaning code below to clean the movie dataset.
movies1 <- read.csv("datasets/movies.csv")

movies <- 
  movies1 %>% 
  mutate(budgetM = budget/1000000,
         grossM = gross/1000000,
         profitM = grossM - budgetM,
         ROI = profitM/budgetM,
         blockbuster = ifelse(profitM > 100, 1,0) %>% 
           factor(., levels = c("0","1")),
         blockbuster_numeric = ifelse(profitM > 100, 1,0), 
         genre_main = 
           as.factor(unlist(map(strsplit
         (as.character(movies1$genres),"\\|"),1))) %>% fct_lump(12),
         rating_simple = fct_lump(content_rating, n = 6)) %>%
  filter(budget < 400000000, 
         content_rating != "", 
         content_rating != "Not Rated",
         !is.na(gross)) %>% 
  mutate(rating_simple = rating_simple %>% fct_drop()) %>% 
  rename(director = director_name, 
         title = movie_title,
         year = title_year) %>% 
  select(-c(actor_1_name, actor_2_name,actor_3_name,actor_1_facebook_likes, 
            actor_2_facebook_likes,actor_3_facebook_likes, 
            movie_imdb_link, budget, gross, aspect_ratio, 
            num_voted_users,num_user_for_reviews)) %>% 
  relocate(title, year, country, director, budgetM, grossM, profitM, ROI,
           imdb_score, genre_main, rating_simple, language, duration) %>% 
  distinct()

#1b. Split the movie dataset into testing and training sets, giving 30% of 
#the data for the test set. Use set.seed(310).

set.seed(310)
movies_split <- initial_split(movies, prop = 0.70)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)
dim(movies_split)

# 1c. Use the training set and estimate a model predicting movie gross (grossM) 
#using imdb_score, budgetM, and rating_simple as independent variables. 
#Use the relevel command to change the reference category of ratings to “R”. 
#Print the summary of this regression table.

mod1 <- lm(grossM ~ imdb_score+ budgetM + relevel(rating_simple,ref = "R"), 
           data = movies_train)

# use summary function and interpret the coefficients
summary(mod1)

# 1d. Interpret the coefficient on budgetM. Holding fixed imdb_score, 
#does spending more money on movies seem to have a net positive return on movie 
#gross?

# For every 1 million dollar increase in the budget, 
# the movie gross returns a net positive increase of 0.95 million dollars

# 1e. Interpret the coefficient on a movie rated G.

# Compared to R rated movies, G rated movies generate a net positive return of
# 21.32 million gross more

# 1f. Use the predict function to generate the predictions in the test and 
#training set.

pred_train <-  predict(mod1, newdata = movies_train)
head(pred_train)

pred_test <- predict(mod1, newdata = movies_test)
head(pred_test)

#1g. Plot the predicted versus true outcome in the test and training set.

training_preds <- data.frame("true" = movies_train$grossM, "pred" = pred_train)
head(training_preds)

ggplot(training_preds, aes(x=true, y=pred)) + geom_point()

testing_preds <- data.frame("true2" = movies_test$grossM, "pred2" = pred_test)
head(testing_preds)

ggplot(testing_preds, aes(x=true2, y=pred2)) + geom_point()

# 1h. Use lindia package to create a scatter plot of residuals against the 
#fitted values (predicted outcome) for the training set. Do the errors appear 
#homoskedastic or heteroskedastic?

gg_resfitted(mod1)
# The errors appear to be heteroskedastic because the errors 
# are not evenly spread across the graph 

# 1i. Use RMSE function in caret package to calculate in-sample and 
#out-of-sample RMSE. Is the model overfit? And if so, how do we know?

RMSE(pred = pred_train, obs = movies_train$grossM)
RMSE(pred = pred_test, obs = movies_test$grossM)

# The model has a higher train error than test error which 
# means the model is not overfit. If it were to be overfit then the train error 
# would be less than the test error because the model has been adjusted 
# to fit the bias of the dataset. 

#Question 2: What Predicts Blockbuster Movies?

# 2a. The variable “blockbuster” in the movies dataframe equals 1 if a movie 
#earns more than $100M USD, and 0 otherwise. Fit a logistic regression model to 
#predict whether a movie is a blockbuster using imdb_score, budgetM, year, 
#director_facebook_likes and genre_main as predictors. Store this logistic model 
#as movies_logit1 and run the summary command over the fitted logistic model 
#object. Be sure to fit the model against the training data!

movies_logit1 <- glm(blockbuster_numeric ~ imdb_score + budgetM + year
                     + director_facebook_likes + genre_main, family = binomial, 
                     data = movies_train)

summary(movies_logit1)

# 2b. Exponentiate the fitted coefficient vector and print the results. 
#Interpret the coefficient on genre_mainCrime. (Meaning communicate that 
#variable’s impact on the model in technically correct but plain language.)

#force r to avoid using scientific format
options(scipen = 999)

# convert coef to odds ratio
exp(movies_logit1$coefficients)
print(1-0.135439112140)

# The model suggests that after controlling for imdb_score, budgetM, year, 
# director_facebook_likes and genre_main, 
# genre_mainCrime is 86.4% (1 - exp(coefficient)) less likely to be a 
#blockbuster movie. 

# 2c. Interpret the coefficient on imdb_score. Is it true that blockbusters get 
#worse IMDB scores?

# As IMDB score increases by 1 rating the odds of the movie being a blockbuster 
# multiply by 278%. It's false that blockbusters get worse imdb scores because 
# the likelihood of a movie being a blockbuster increases imdb score.

# 2d. Using movies_logit1 generate predicted probabilities for the test and 
#training sets. Print the top of each of these score vectors using the head() 
#command.

scores_train <- predict(movies_logit1, type = "response")
pred_train_classes <- ifelse(scores_train > 0.5,"Yes","No")
head(pred_train_classes)

scores_test <- predict(movies_logit1, newdata=movies_test, type = "response")
pred_test_classes <- ifelse(scores_test > 0.5,"Yes","No")
head(pred_test_classes)

# 2e. Produce two ROC plots, one each for the test and training sets. Be sure 
#to label the cutoff probabilities along the ROC lines using the cutoffs.at 
# = c(0.99,0.9,0.7,0.5,0.3,0.1,0) option as shown in the lecture slides.

results_train <- data.frame(truth=movies_train$blockbuster_numeric,
                                  Class1 = scores_train,
                                  Class2 = 1-scores_train,
                            pred_train = as_factor(pred_train_classes))
#testing
results_test <- data.frame(truth=movies_test$blockbuster_numeric,
                                Class.1 = scores_test, Class.2 = 1-scores_test,
                           pred_test = as_factor(pred_test_classes))
head(results_test)

#make ROC Graphs
p_train <- ggplot(results_train, aes(m=Class1, d=truth)) +   
  geom_roc(cutoffs.at = c(0.99,0.9,0.7,0.5,0.3,0.1,0))

print(p_train)

p_test <- ggplot(results_test, aes(m=Class.1, d=truth)) +
  geom_roc(cutoffs.at = c(0.99,0.9,0.7,0.5,0.3,0.1,0))

print(p_test)

#2f. Based on ROC on the training set, pick an appropriate cutoff. Using the 
#predicted probabilities, assign classes for the test and the training sets.

pred_train_classes <- ifelse(scores_train > 0.1,"Yes","No")
pred_test_classes <- ifelse(scores_test > 0.1,"Yes","No")

#2g. Use yardstick package to calculate and print the confusion matrices for 
# the test and training sets.

#training
results_train <- data.frame(truth=movies_train$blockbuster_numeric,
                            truth_factor = factor(ifelse
                            (movies_train$blockbuster==1, "Yes", "No")),
                            Class1 = scores_train,
                            Class2 = 1-scores_train,
                            pred_train = factor(
                              ifelse(scores_train > 0.1,"Yes","No")))

cm_train <- conf_mat(results_train, truth = truth_factor, estimate = pred_train)
cm_train

#testing
results_test <- data.frame(truth=movies_test$blockbuster_numeric,
                           truth_factor = factor(ifelse
                            (movies_test$blockbuster==1, "Yes", "No")),
                           Class.1 = scores_test, Class.2 = 1-scores_test,
                           pred_test = factor(ifelse(scores_test > 0.1,
                                                     "Yes", "No")))

cm_test <- conf_mat(results_test, truth = truth_factor, estimate = pred_test)
cm_test

# 2h. Calculate the accuracy, sensitivity and specificity for the testing and 
#training confusion matrices.

#training

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

# 2i
calc_auc(p_train)
calc_auc(p_test)

#The model has a good fit because the testing and training AUC are very close 
#to one another. It would considered overfit if the size of the gap between the 
#two was large and it would be considered underfit if there was no gap at all. 
#In this case, we have a good model because there is a very small gap between 
#test and train. To adjust a model that is overfit ,I would train more data and 
#perform cross validation. To adjust a model that is underfit, I would increase 
#the number of parameters or increase model complexity.
