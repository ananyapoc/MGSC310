#------------------------------------------------
### Load NBA
#------------------------------------------------
library(tidyverse)

nba <- read.csv("datasets/nba_players.csv")
glimpse(nba)

# group players' data by taking average of players' stats over seasons
nba_players <- nba %>% group_by(player_name) %>% summarize(across(where
          (is.numeric),mean)) %>% select(-c(X, net_rating)) %>% as.data.frame()

nba_players %>% glimpse()

# set the player_name as row.names and remove the player_name column

row.names(nba_players) <- nba_players$player_name

nba_players <- nba_players %>% select(-player_name)

head(nba_players)

#------------------------------------------------
### How Many Clusters?
#------------------------------------------------
#install.packages("factoextra")
library(factoextra)

# Explore these methods on your own
# elbow method
fviz_nbclust(nba_players,
             kmeans,
             method="wss") +
  geom_vline(xintercept=4, linetype =2) + 
  labs(subtitle ="Elbow Method")

# Silhouette method
fviz_nbclust(nba_players,
             kmeans,
             method="silhouette")

# Gap Statistic method
fviz_nbclust(nba_players,
             kmeans,
             method="gap_stat",
             nboot=100)
#--------

#install.packages('NbClust')
library("NbClust")
Nb_cl <- NbClust(nba_players %>% sample_frac(0.1),
                 diss=NULL,
                 distance="euclidean",
                 min.nc = 2,
                 max.nc = 10,
                 method="kmeans")


Nb_cl$Best.nc[1,]

dev.off()
barplot(table(Nb_cl$Best.n[1,]), xlab = "Number of Clusters", 
ylab = "Number of Criteria", main = "Number of Clusters Chosen by 26 Criteria")

#------------------------------------------------
### K-Means
#------------------------------------------------
# use kmeans with 3 clusters

kmeans3 <- kmeans(nba_players, centers = 3, nstart = 25)

# explore the cluster information

kmeans3$centers
kmeans3$cluster

kmeans3$cluster["Jason Kidd"]

# visualize the clusters




#------------------------------------------------
### Scaling
#------------------------------------------------

# scale 




# Redo the Kmeans Clustering



# cluster plot



# store the cluster information in the data frame


# use ggpairs to see if the clusters are meaningful



