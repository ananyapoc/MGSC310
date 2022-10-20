#Problem Set 5: Predicting Movie Revenue Using Decision Trees#
#Ananya Pochiraju, Nikita Shetty, Kashish Pandey#

#load all packages 

library('tidyverse')
library('rsample')
library('ggridges')
library('tree')
library('caret')
library('randomForest')
library('randomForestExplainer')
library('ggplot2')

#a. Use the IMDB 5000 dataset and the following code to clean and prepare the 
# dataset for modeling.

options(scipen=99)
set.seed(310)

movies <- read.csv("datasets/movies.csv")

movies_clean <- movies %>% 
  filter(budget < 4e+08) %>% 
  filter(content_rating != "", content_rating != "Not Rated", 
         plot_keywords != "", !is.na(gross))

movies_clean <- movies_clean %>% 
  mutate(genre_main = unlist(map(strsplit(as.character(movies_clean$genres),
                                          "\\|"), 1)), 
         plot_main = unlist(map(strsplit
                                (as.character(movies_clean$plot_keywords), 
                                  "\\|"), 1)), 
         grossM = gross/1e+06, 
         budgetM = budget/1e+06)

movies_clean <- movies_clean %>% 
  mutate(genre_main = fct_lump(genre_main,7), 
         plot_first = fct_lump(plot_main, 20), 
         content_rating = fct_lump(content_rating,4), 
         country = fct_lump(country, 8), 
         language = fct_lump(language, 4), 
         cast_total_facebook_likes000s = cast_total_facebook_likes/1000) %>% 
  drop_na()

top_director <- movies_clean %>% 
  group_by(director_name) %>% 
  summarize(num_films = n()) %>% 
  top_frac(0.1) %>% 
  mutate(top_director = 1) %>% 
  select(-num_films)

movies_clean <- movies_clean %>% 
  left_join(top_director, by = "director_name") %>% 
  mutate(top_director = replace_na(top_director, 0)) %>% 
  select(-c(director_name, actor_2_name, gross, genres, actor_1_name, 
            movie_title, actor_3_name, plot_keywords, movie_imdb_link, budget, 
            color, aspect_ratio, plot_main, actor_3_facebook_likes, 
            actor_2_facebook_likes, color, num_critic_for_reviews, 
            num_voted_users, num_user_for_reviews, actor_2_facebook_likes))

movies_split <- initial_split(movies_clean, prop=0.7)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)

#b. Produce a ridgeline plot showing grossM against plot_first. Which plot 
# keywords are associated with the most blockbusters (gross above $300M)?

ggplot(data = movies_clean, mapping = aes(x=grossM, y = plot_first, 
                                    fill = plot_first))+geom_density_ridges()

#The keywords associated with the most blockbusters are "battle", 
# "friend", and "college". 

#c. Use tree package to estimate a regression tree to predict grossM using all 
# other variables as predictors.

glimpse(movies_clean)

tree_movies <- tree(grossM ~ duration + director_facebook_likes 
                    + actor_1_facebook_likes + cast_total_facebook_likes 
                    + facenumber_in_poster
                    + language + country + content_rating + title_year 
                    + imdb_score + movie_facebook_likes + genre_main + budgetM 
                    + plot_first + cast_total_facebook_likes000s + top_director, 
                    data = movies_train)
summary(tree_movies)

#d. Plot the fitted tree using the plot() function. Use text() to add text to
# the object. You can use digit=2 and pretty=0 options in text function.

plot(tree_movies)
text(tree_movies, digit=2, pretty=0, cex = .5)

#e. Identify the leaf node for blockbuster movies. Describe the path on the 
# tree that results in blockbuster movies. What about the movies with the 
# lowest revenue? What is the recipe for disaster in terms of the variables 
# on the tree?

# The leaf node that results in blockbuster movies is the far right node that 
# results in a gross revenue of 570 million dollars. The path to blockbuster 
# movies is decided by movies that are higher than 71 million in budget, 
# higher than 182.5 million in budget, and have an IMDB score greater than 7.85. 
# The movies with the lowest revenue have a budget less than 71 million dollars, 
# less than 27.25 million, and have a title year greater than 1992.5. The leaf 
# node for this path is 21 million dollars in gross revenue. The recipe for 
# disaster is having very low budget movies that are made more recently, have
# a lower amount of facebook likes, and are not rated or rated lower than PG-13. 

#f. Use the predict function to get predicted values for data in the training 
# and test sets. Calculate RMSE for both sets.

preds_train <- predict(tree_movies, newdata = movies_train)

head(preds_train)

preds_test <- predict(tree_movies, newdata = movies_test)

head(preds_test)

RMSE(preds_train, movies_train$grossM)
RMSE(preds_test, movies_test$grossM)

#g. Use cross-validation to find the best tree size. What is the best 
# tree size? Do we need to prune the tree?

tree_movies_cv <- cv.tree(tree_movies)
tree_movies_cv

plot(tree_movies_cv$size, tree_movies_cv$dev, type='b')

# The best size is 5, and we need to prune the tree because the RMSEs for the 
# train set is lower than the test set and we want to prevent overfitting.

#h. Estimate a random forest with 200 trees. Use mtry = 8 as a parameter. 
# Describe what the mtry parameter does.

rf.movies <- randomForest(grossM ~ .,
                          mtry = 8,
                          data = movies_train,
                          ntree = 200,
                          importance = TRUE)
print(rf.movies)
plot(rf.movies)

# mtry is the number of variables randomly sampled as candidates at each split.

#i. Get predicted values from the random forest model in the training and test 
# set and calculate RMSE for both.

preds_train_rf <- predict(rf.movies, newdata = movies_train)

head(preds_train_rf)

preds_test_rf <- predict(rf.movies, newdata = movies_test)

head(preds_test_rf)

RMSE(preds_train_rf, movies_train$grossM)
RMSE(preds_test_rf, movies_test$grossM)

#j. In the random forest model, which variables are more important?

important_variables(rf.movies)

# The variables that are most important are budgetM, imdb_score, and movie 
# facebook likes because they have the lowest depths in the tree. 

#k. Use randomForestExplainer package and plot_min_depth_distribution 
# to visualize variable importance. What do you take away?

# plot min depth distribution
plot_min_depth_distribution(rf.movies)

# The budget, facebook likes and IMDB score are the biggest influences on the 
# profits of the movie and are the most important variables to decide what the
# profit of each movie will be. Title year is the least important. 

#l. Comment on the models you have estimated. 
# Which model performs the best and why?

# The decision tree model performed best because the train and test sets had 
# similar RMSE's. The random forest model had a much higher test error 
# than train error and was extremely overfit. The decision tree model was also 
# slightly overfit, but much less than the other model which is why 
# we believe it performed better than the other model.
  
