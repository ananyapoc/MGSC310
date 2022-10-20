library(tidyverse)
library(ggplot2)
library(GGally)
library(forcats)
library(rsample)

#1a
college <- read.csv("datasets/college.csv")

#1b
rownames(college) <- college[,1]
head(college)

college <- college %>% select(-X)
head(college)

#1c
summary(college)

#1d
ggpairs(data = college,columns = 1:6)

names(college)
#1e
ggplot(data = college, mapping = aes(x = Top10perc, y = S.F.Ratio, color = factor(Private))) + geom_point()
#### FINISH THIS- More Private schools have students from the top 10% of their high school class than public schools


#2a
movies <- read_csv("datasets/movies.csv")

movies_clean <- 
  movies %>% 
  mutate(budgetM = budget/1000000,
         grossM = gross/1000000,
         profitM = grossM - budgetM,
         ROI = profitM/budgetM,
         genre_main = as.factor(unlist(map(strsplit(as.character(movies$genres),"\\|"),1))) %>% fct_lump(12),
         rating_simple = fct_lump(content_rating, n = 6)
  ) %>%
  filter(budget < 400000000, 
         content_rating != "", 
         content_rating != "Not Rated") %>% 
  mutate(rating_simple = rating_simple %>% fct_drop()) %>% 
  rename(director = director_name, 
         title = movie_title,
         year = title_year) %>% 
  select(-c(actor_1_name, actor_2_name,actor_3_name,actor_1_facebook_likes, actor_2_facebook_likes,actor_3_facebook_likes, 
            movie_imdb_link, budget, gross, aspect_ratio, num_voted_users,num_user_for_reviews)) %>% 
  relocate(title, year, country, director, budgetM, grossM, profitM, ROI, imdb_score, genre_main, rating_simple, language, duration)

#2b
set.seed(310)
movies_split <- initial_split(data = movies_clean, prop = .8)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)

#2c
nrow(movies_train)
nrow(movies_test)

#2d
mod1 <- lm(profitM ~ imdb_score,
           data = movies_train)

summary(mod1)

#2e
#For each 1 unit increase in IMDB score, the predicted profit increases by $13,678,500.

#2f
# The p-value associated with the estimate of imdb_score is < 2.2e-16. A p-value signifies how likely 
# the results we observed were able to be seen due to chance. Because our p-value is extremely low, lower
# than the significance value 0.05, which means the imdb_score does seem to play a significant role in determining
# profitM



       