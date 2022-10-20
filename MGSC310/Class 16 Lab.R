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
         genre_main = as.factor(unlist(map(strsplit(as.character(movies$genres),"\\|"),1))) %>% fct_lump(5),
         rating = fct_lump(content_rating, n = 3),
         cast_total_facebook_likes000s = cast_total_facebook_likes / 1000,
         country = fct_lump(country,2)
  ) %>%
  filter(budget < 400000000, 
         content_rating != "", 
         content_rating != "Not Rated") %>% 
  select(-c(director_name,movie_title,title_year,actor_1_name, actor_2_name,
            actor_3_name,movie_imdb_link, budget, gross, budgetM, grossM, genres, 
            plot_keywords,language,country,content_rating)) %>% 
  drop_na()

# split train(0.8)/test(0.2)
set.seed(310)
movies_split <- initial_split(movies_clean, 0.8)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)

# ----------------------------------------------------------
# Fit Ridge Model
# ----------------------------------------------------------

# estimate a Ridge model using glmnet
# note if you get an error make sure you 
#  have loaded glmnetUtils
ridge_mod <- cv.glmnet(profitM ~ .,
                       data = movies_train,
                       # note alpha = 0 sets ridge!  
                       alpha = 0)

# print the two model suggested values of lambda:

print(ridge_mod$lambda.min)
#
print(ridge_mod$lambda.1se)

# print coefficient using lambda.min
coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
  round(3)

# print coefficient using lambda.1se
coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
  round(3)

# put into coefficient vector
ridge_coefs <- data.frame(
  ridge_min = coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
    round(3) %>% as.matrix() ,
  ridge_1se = coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() 
) %>% rename(ridge_min = 1, ridge_1se = 2)

ridge_coefs

# use the plot function to see the MSE
# path as we vary lambda (the amount of penalization)
plot(ridge_mod)

### examine coefficient shrinkage path
library('coefplot')
coefpath(ridge_mod)

# ----------------------------------------------------------
#  Lab Exercises
# ----------------------------------------------------------
semi <- read.csv('datasets/semiconductor.csv')
glimpse(semi)

# 1. Load the semiconductor dataset and split into testing and 
#    training sets

semi_split <- initial_split(semi, 0.9)
semi_train <- training(semi_split)
semi_test <- testing(semi_split)

# 2. Using the function cv.glmnet estimate a ridge model using 
#    the training data and call this ridge_mod2. 
#    Note we must set family = "binomial" to specify 
#    the binary y variable

ridge_mod <- cv.glmnet(FAIL ~ .,
                       data = semi_train,
                       # note alpha = 0 sets ridge!  
                       alpha = 0, family = "binomial")

# 3. What does the "cv" in cv.glmet stand for? Why is it used here?

#It stands for cross-validation. It is used here because ridge and lasso are 
#forms of cross validation, since they filter out predictor variables to 
#determine an optimal number of variables for the most accurate model.

# 4. Call the plot function against the ridge_mod2 and describe the plot. 
#    What do the two vertical dashed lines represent?

# 5. Print the coefficient vector using lambda.1se. Store this vector as 
#    ridge_coef_1se.

# 6. Print the coefficient vector using lambda.min. Store this vector as
#    ridge_coef_min

# 7. Use the code below to change the ridge matrices from sparse matrices (efficient for
#    large number of rows) to regular matrices. And then combine the coefficient vectors
#    into one coefficient vector for both ridge 1se and ridge min. 

ridge_coef_min <- ridge_coef_min %>% as.matrix() %>% 
  as.data.frame() %>% 
  round(3)

ridge_coef_1se <- ridge_coef_1se %>% as.matrix() %>% 
  as.data.frame() %>% 
  round(3)

ridge_coefs <- bind_cols(ridge_mod_min, ridge_mod_1se) %>% 
  rename(Ridge_min = 1, Ridge_1se = 2)

# 8. Print ridge_coefs and describe the difference between 
#    the coefficients for ridge_mod2 with lambda.min versus lambda.1se? 
#    How are they different and why? 


# 9. If you have time, load the library 'coefplot' and run the function 
#    'coefpath' against the ridge model. 


