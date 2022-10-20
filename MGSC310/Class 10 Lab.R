#Class 10 Lab#

#load libraries
library(tidyverse)
library(forcats)

#------------------------------------------------------
# 1. Reading Data
#------------------------------------------------------

#load nba_players.csv data
nba <- read.csv("datasets/nba_players.csv")

#description of the variables 
# 1)  X:                 index
# 2)  player_name:       Name of the player
# 3)  team_abbreviation: Abbreviated name of the team the player played for (at the end of the season)
# 4)  age:               Age of the player
# 5)  player_height:     Height of the player (in centimeters)
# 6)  player_weight:     Weight of the player (in kilograms)
# 7)  college:           Name of the college the player attended
# 8)  country:           Name of the country the player was born in (not necessarily the nationality)
# 9)  draft_year:        The year the player was drafted
# 10) draft_round:       The draft round the player was picked
# 11) draft_number:      The number at which the player was picked in his draft round
# 12) gp:                Games played throughout the season
# 13) pts:               Average number of points scored
# 14) reb:               Average number of rebounds grabbed
# 15) ast:               Average number of assists distributed
# 16) net_rating:        Team's point differential per 100 possessions while the player is on the court
# 17) oreb_pct:          Percentage of available offensive rebounds the player grabbed while he was on the floor
# 18) dreb_pct:          Percentage of available defensive rebounds the player grabbed while he was on the floor
# 19) usg_pct:           Percentage of team plays used by the player while he was on the floor (FGA + Possession Ending FTA + TO) / POSS)
# 20) ts_pct:            Measure of the player's shooting efficiency that takes into account free throws, 2 and 3 point shots (PTS / (2*(FGA + 0.44 * FTA)))
# 21) ast_pct:           Percentage of teammate field goals the player assisted while he was on the floor
# 22) season:            NBA season 

nba %>% glimpse()
summary(nba)

unique(nba$draft_round)
length(unique(nba$country))
length(unique(nba$college))

#------------------------------------------------------
#2. Feature Transformation (Cleaning)
#------------------------------------------------------

nba <- nba %>% filter(draft_round %in% c("Undrafted", "1", "2")) %>% mutate (draft_round = factor(draft_round), USA = ifelse(country =="USA", 1,0), college_simple = fct_lump(college, n = 5), position = cut(player_height, breaks =c(-Inf, 195, 207, Inf), labels = c("Guard", "Forward", "Center")))
nba %>% glimpse()

#------------------------------------------------------
#3. Data Exploration
#------------------------------------------------------

#let's calculate the (pairwise) correlation among the variables: player_height, player_weight, gp, pts, reb, ast, net_rating
corr.table <- cor(nba %>% select(player_height, player_weight, gp, pts, reb, ast, net_rating))

#that would be hard to read. let's use a visualization tool.
#use "ggcorrplot" package
library(ggcorrplot)

ggcorrplot(corr.table, type = 'lower')

#all the numeric columns of data
corr.table <- cor(nba %>% select_if(is.numeric) %>% select(-X))
ggcorrplot(corr.table, type = 'lower')

#------------------------------------------------------
#4. Group Summary Statistics
#------------------------------------------------------

#players' summary statistics
#top 10 player with highest maximum points
nba %>% group_by(player_name) %>% summarize(avg_pts = mean(pts), max_pts = max(pts)) %>% arrange(-max_pts) %>% slice(1:10)

#------------------------------------------------------
#5. Visualization 
#------------------------------------------------------

#average assists per position over time 
nba_ast <- nba %>% group_by(season, position) %>% summarize(avg_ast = mean(ast))

ggplot(nba_ast, aes(x=season, y=avg_ast, group=position)) + geom_line(aes(color=position)) + theme(axis.text.x = element_text(angle = 90))

#shooting efficiency
ggplot(nba, aes(x = age, y=ts_pct)) + geom_point()
nba_shooting <- nba %>% group_by(age) %>% summarize(avg_shooting = mean(ts_pct))
nba_shooting %>% glimpse()
ggplot(nba_shooting, aes(x=age, y=avg_shooting)) + geom_point() + geom_line()

#ggridges
library(ggridges)

#distribution of players' height over the years
ggplot(nba, aes(x=player_height, y=season, fill=season)) + geom_density_ridges()

#average points per draft number
nba_draft <- nba %>% filter(draft_number!="Undrafted") %>% mutate(draft_number = as.integer(draft_number)) %>% group_by(draft_number) %>% summarize(avg_pts = mean(pts)) %>% arrange(avg_pts)
ggplot(nba_draft, aes(x=draft_number, y=avg_pts)) + geom_point() + geom_smooth()





