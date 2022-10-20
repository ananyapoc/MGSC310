#Class 11 Lab#

#1. Load nba_players.csv dataset and apply the basic cleaning.

#load libraries
library(tidyverse)

#load nba_players.csv data
nba <- read.csv("datasets/nba_players.csv")

#cleaning dataset
nba <- nba %>% filter(draft_round %in% c("Undrafted", "1", "2")) %>% mutate (draft_round = factor(draft_round), USA = ifelse(country =="USA", 1,0), college_simple = fct_lump(college, n = 5), position = cut(player_height, breaks =c(-Inf, 195, 207, Inf), labels = c("Guard", "Forward", "Center")))

#2. Split the dataframe into the training (75%) and test sets (25%)
library(rsample)
set.seed(310)
nba_split <- initial_split(nba, prop=0.75)
nba_train <- training(nba_split)
nba_test <- testing(nba_split)

dim(nba_split)

#3. To find out the effect of defensive (rebounds)and offensive (points)plays on the overall performance of the team (net rating), regress net_rating over pts and reb(use the training set).

mod1 <- lm(net_rating ~ pts + reb, data = nba)
summary(mod1)


