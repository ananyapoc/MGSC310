#Class 5 Lab
library(tidyverse)
movies <- read.csv("datasets.csv", stringsAsFactors = F)
as_tibble(movies)
view(movies)

#1 What are the highest grossing Steven Spielberg films?
movies_dir <- movies %>% filter(director_name == "Steven Spielberg")
movies_dir %>% arrange(desc(gross))%>% slice(1:10)
movies_dir %>% select(movie_title, gross)

#2 How many "PG 13" movies are there in the database? (hint: use nrow())
movies_PG13 <- movies %>% filter( content_rating == "PG-13")
nrow(movies_PG13)
