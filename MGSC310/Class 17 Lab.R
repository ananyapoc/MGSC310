# ----------------------------------------------------------
# Load Libraries
# ----------------------------------------------------------
options(scipen = 9)

library('tidyverse')
library('rsample')
library('glmnet')
library('glmnetUtils')
library('forcats')

# load and clean the data
movies <- read.csv("datasets/movies.csv")

movies_clean <- 
  movies %>% 
  mutate(budgetM = budget/1000000,
         grossM = gross/1000000,
         profitM = grossM - budgetM,
         genre_main = as.factor(unlist(map(strsplit(as.character(movies$genres)
                                ,"\\|"),1))) %>% fct_lump(5),
         rating = fct_lump(content_rating, n = 3),
         cast_total_facebook_likes000s = cast_total_facebook_likes / 1000,
         country = fct_lump(country,2)
  ) %>%
  filter(budget < 400000000, 
         content_rating != "", 
         content_rating != "Not Rated") %>% 
  select(-c(director_name,movie_title,title_year,actor_1_name, actor_2_name,
            actor_3_name,movie_imdb_link, budget, gross, budgetM, grossM, 
            genres, plot_keywords,language,country,content_rating)) %>% 
  drop_na()

# split train(0.8)/test(0.2)
set.seed(310)
movies_split <- initial_split(movies_clean, 0.8)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)

# ----------------------------------------------------------
# Fit Lasso Model
# ----------------------------------------------------------

# note cv.glmnet automatically performs 
# k-fold cross-validation 
lasso_mod <- cv.glmnet(profitM ~ .,
                       data = movies_train,
                       # note alpha = 1 sets Lasso!  
                       alpha = 1)

# Note that lasso estimates a series of models, one for 
# every value of lambda -- the amount of shrinkage

# use the plot function to see the MSE
# path as we vary lambda (the amount of penalization)
plot(lasso_mod)

# print the two model suggested values of lambda:
print(lasso_mod$lambda.min)
#
print(lasso_mod$lambda.1se)

# to examine the coefficients we must say what value of 
# lambda we want to use.
coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
  round(3)

# print coefficient using lambda.1se
coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
  round(3)

# put into coefficient vector
lasso_coefs <- data.frame(
  lasso_min = coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
    round(3) %>% as.matrix() ,
  lasso_1se = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() 
) %>% rename(lasso_min = 1, lasso_1se = 2)

print(lasso_coefs)


# examine coefficient shrinkage path
library('coefplot')
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
preds_train <- predict(lasso_mod, s = lasso_mod$lambda.min, movies_train)
preds_test <- predict(lasso_mod, s = lasso_mod$lambda.min, movies_test)

# RMSE
library(caret)
# train
RMSE(preds_train, movies_train$profitM)
# test
RMSE(preds_test, movies_test$profitM)

ggplot(movies_clean, aes(x = profitM)) +
  geom_histogram(bins=50) +
  geom_vline(xintercept = 45, color='red', linetype='dashed')+
  geom_vline(xintercept = -45, color='red', linetype='dashed')

# ----------------------------------------------------------
#  Lab Exercises
# ----------------------------------------------------------
# 1. Load the semiconductor dataset and split into testing and 
#    training sets
semi <- read.csv('datasets/semiconductor.csv')
semi_split <- initial_split(semi, 0.9)
semi_train <- training(semi_split)
semi_test <- testing(semi_split)

# 2. Estimate a lasso model using the training data, with FAIL as the 
#    outcome variable, and every other variable in the data frame as the 
# predictors.
#    Use the option "family = "binomial"
#    Store this model as lasso_mod2

lasso_mod2 <- cv.glmnet(FAIL ~ .,
                        data = semi_train,
                        family = binomial,
                        #note alpha = 1 sets Lasso!
                        alpha = 1)

# 3. What does the option "alpha = 1" in cv.glmnet mean?

#Alpha = 1 sets the lasso. a = 1 is a lasson regression while a = 0 is a ridge 
# regression

# 4. What does the option "family = "binomial"" mean?

# family = binomial recognizes it as a classification model 

# 5. How is cv.glmnet different from the function glmnet()?

# glmnet() doesn't include cross validation, but cv.glmnet utilizes 
# cross-validation

# 6. Call the plot function against lasso_mod2. 
#    Describe the plot as well as the two vertical dashed lines

plot(lasso_mod2)

print(lasso_mod2$lambda.min)

print(lasso_mod2$lambda.lse)

# 7. Store the lambda.1se Lasso coefficients into a data frame
#    called coef_lasso_1se and print the coefficients

coef_lasso_lse <- coef(lasso_mod2, s = lasso_mod2$lambda.lse) %>% round(3)

# 8. Store the lambda.min Lasso coefficients into a data frame
#    called coef_lasso_min and print the coefficients

coef_lasso_min <- coef(lasso_mod2, s = lasso_mod2$lambda.min) %>% round(3)

# 9. How many variables are non-zero using lambda.min and lambda.1se? 
#     Why are they different? When would you use one versus another?

# 10. If you have time, use the coefpath against the lasso mod to see which 
#     variables are shrunk first to zero as we increase lambda. 


