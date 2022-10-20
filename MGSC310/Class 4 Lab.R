#Class 4 Lab#

library(tidyverse)
movies <- read.csv("movie_metadata.csv", stringsAsFactors = F)
movies = as_tibble(movies)
movies

#Ways to View Data
names(movies)
dim(movies)
summary(movies)
glimpse(movies)

#NA Values 
mean(movies$budget, na.rm = T)
is.na(movies$budget)
budget_noblank <- movies$budget
budget_noblank <- budget_noblank[!is.na(budget_noblank)]
sum(is.na(movies$budget))

#Slice
movies%>% slice(1:10)
movies%>% slice(300:310)

#Arrange
movies%>% arrange(desc(budget))
  #arrange and slice
movies%>% arrange(desc(budget), title_year)%>% slice(1:10)

#Select
movies_director <- movies %>% select(director_name, movie_title)
glimpse(movies_director)
movies %>% select(starts_with("actor"))
  #everything#
movies%>% select(director_name, movie_title, title_year, everything())

#Filter
movies_big <- movies %>% filter(budget > 100000000)
glimpse(movies_big)
  #english films only 
movies_eng <- movies %>% filter(language == "English")
glimpse(movies_eng)
  #english and spanish films
movies_lang <- movies %>% filter(language == "English" | language == "Spanish")
glimpse(movies_lang)
  #big budget spanish films 
movies_big_spanish <- movies %>% filter(language == "Spanish" & budget > 1e8)
glimpse(movies_big_spanish)



