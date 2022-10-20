#Problem Set 1: R Programming and Data Exploration#
#Ananya Pochiraju MGSC310 Section 01#

#Question 1#

# a. Check your current working directory to make sure that is set right
setwd("/Users/ananyapochiraju/Desktop/MGSC310")
getwd()

# b. Download the IMDB-5000 from the Canvas site. Put this file in a folder “datasets”. Read the dataset into R and call this file movies.
library(tidyverse)
movies <- read.csv("datasets/movies.csv", stringsAsFactors = F)
as_tibble(movies)

# c. What are the dimensions of the dataset? Programatically determine this using a function.
dim(movies)

#d. What are the names of the variables in the dataset? Hint: use the function names here. 
names(movies)

#e. How do we determine the data types of each variable? Use the pipe operator (%>%) to pass the movies dataset to the glimpse() function. Which variables are characters?
movies %>% glimpse()
movies_char <- movies %>% select_if(is.character)
movies_char %>% glimpse()

#f. Use the slice() function to show the first 5 rows of the dataset.
movies %>% slice(1:5)

#g. Use the arrange() function to order the dataset in descending order by gross or total box-office revenue in $USD. Use the slice function to show the ten movies with the largest box-office revenue in the dataset. Which director is the king of the box office?
movies_topgross <- movies %>% arrange(desc(gross)) %>% slice(1:10) %>% select(director_name, gross)
movies_topgross %>% glimpse()

#h. Use the select() function to remove the variable movie_imdb_link. Store the smaller dataset as movies_sub. Run the dimension command to ensure that movies_sub has one fewer variable than movies.
movies_sub <- movies %>% select(-movie_imdb_link)
dim(movies_sub)

#i. How many movies are rated as PG-13? (Hint. use content_rating column.) What is the average revenue (gross) for PG-13 movies?
movies_PG13 <- movies %>% filter(content_rating == "PG-13")
nrow(movies_PG13)
avg_rev_PG13 <- movies %>% group_by(content_rating) %>% summarize(gross_avg = mean(gross, na.rm = TRUE))
avg_rev_PG13 %>% print()

#Question 2#
library(ggplot2)
#a. Use the package ggplot2 to create a scatter plot of IMDB score on the x axis and movie gross on the y-axis.
ggplot(data = movies, mapping = aes(x = imdb_score, y = gross)) + geom_point()

#b. This looks okay, but there are so many points it is hard to see how much underlying data each point represents. Use the option alpha = 1/10 to reduce the transparency of each data point.
ggplot(data = movies, mapping = aes(x = imdb_score, y = gross)) + geom_point(alpha = 0.1)

#c. It would be more readable if the units of the y-axis were in millions of dollars versus just USD. Use the mutate() function to create a new variable gross_M which lists gross in millions of dollars. Store this new variable in the movies dataset.
movies <- movies %>% mutate(gross_M = gross/1000000) %>% glimpse()

#d. Create a scatter plot of imdb_score against gross_M and use the geom_smooth function to make a smoothing line. Is there a relationship between movie gross and IMDB score?
ggplot(data = movies, mapping = aes(x = imdb_score, y = gross_M)) + geom_point() + geom_smooth()

#e. Use the filter command to only include movies by James Cameron. Use the command geom_text(aes(label = movie_title), color = "blue") to plot movie gross against IMDB score for James Cameron films, where the points are the movie titles. (Note, you do not need to add + geom_point() to the plot since the text are the points!)
movies_james_c <- movies %>% filter(director_name == "James Cameron") 
ggplot(data = movies_james_c, mapping = aes(x = imdb_score, y = gross_M)) + geom_text(aes(label = movie_title), color = "blue") 

#f. This looks okay, but the text is clipped on the right and left of the plot. Use the functions xlim() and ylim() to create more reasonable ranges for these axes.
ggplot(data = movies_james_c, mapping = aes(x = imdb_score, y = gross_M)) + geom_text(aes(label = movie_title), color = "blue") + xlim(7,9) + ylim(30,800)

#g. Add better axes titles using the functions xlab() and ylab().
ggplot(data = movies_james_c, mapping = aes(x = imdb_score, y = gross_M)) + geom_text(aes(label = movie_title), color = "blue") + xlim(7,9) + ylim(30,800) + xlab("Movie IMDB Score") + ylab("Gross Revenue (in millions)")

