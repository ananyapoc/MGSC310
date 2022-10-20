#------------------------------------------------
### Load NBA
#------------------------------------------------
library(tidyverse)
nba <- read.csv("datasets/nba_players.csv")

# group players' data by taking average of players' stats over seasons
nba_players <- nba %>% 
  group_by(player_name) %>% 
  summarize(across(where(is.numeric),mean)) %>% 
  select(-c(net_rating,X)) %>% 
  as.data.frame()

nba_players %>% glimpse()

# set the player_name as row.names and remove the player_name column
row.names(nba_players) <- nba_players$player_name
nba_players <- nba_players %>% select(-player_name)

head(nba_players)

#------------------------------------------------
### K-Means
#------------------------------------------------
kmeans3 <- kmeans(nba_players,
                  centers=3,
                  nstart=25)
kmeans3$centers
kmeans3$cluster


library(factoextra)
fviz_cluster(kmeans3, data=nba_players)

#------------------------------------------------
### Scaling
#------------------------------------------------

# scale 
nba_players_scaled <- nba_players %>% mutate(across(everything(),scale)) %>%
  mutate(across(everything(), as.vector)) %>% as.data.frame()

head(nba_players_scaled)

# Redo the Kmeans Clustering
kmeans3_scaled <- kmeans(nba_players_scaled, centers = 3, nstart = 25)
kmeans3_scaled$centers

# cluster plot

# store the cluster information in the data frame
nba_players_scaled$cluster <- kmeans3_scaled$cluster

head(nba_players_scaled)

# use ggpairs to see if the clusters are meaningful
library(GGally)

ggpairs(nba_players_scaled, c(2,3,5,7,11,13), mapping = 
          aes(color=factor(cluster), alpha=0.5))


#------------------------------------------------
### PCA
#------------------------------------------------

# identify the two most important dimensions
pca_nba <- prcomp(nba_players, center=TRUE, scale.=TRUE)

summary(pca_nba)

# visualization
fviz_pca_biplot(pca_nba)
