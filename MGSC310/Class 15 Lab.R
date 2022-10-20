#--------------------------------------------------
# 1. Data Loading
#--------------------------------------------------

# Now, open the data set using read.csv() and store it in 'comic' dataframe
comic <- read.csv("datasets/comic_characters.csv")

# What is its dimension?

# So there are 23272 records (obs) in this data!
# Check out the variables names

# Cool! Here is the description of the variables:

# 1)  page_id          | The unique identifier for that characters page within 
# the wikia
# 2)  name             | The name of the character 
# 3)  full_name        | The full name of the character
# 4)  urlslug          | The unique url within the wikia that takes you to the 
#character 
# 5)  ID               | The identity status of the character (Secret Identity, 
#Public identity, [on marvel only: No Dual Identity]) 
# 6)  ALIGN            | If the character is Good, Bad or Neutral 
# 7)  EYE              | Eye color of the character 
# 8)  HAIR             | Hair color of the character 
# 9)  SEX              | Sex of the character (e.g. Male, Female, etc.) 
# 10)  GSM             | If the character is a gender or sexual minority (e.g. 
#Homosexual characters, bisexual characters) 
# 11) ALIVE            | If the character is alive or deceased 
# 12) APPEARANCES      | The number of appareances of the character in comic 
#books (as of Sep. 2, 2014. Number will become increasingly out of date as time
#goes on.) 
# 13) FIRST APPEARANCE | The month and year of the character's first appearance 
#in a comic book, if available 
# 14) YEAR             | The year of the character's first appearance in a comic 
#book, if available
# 15) INC              | The creator company (Marvel and DC)


# Check the types of the variables
library(tidyverse)
comic %>% glimpse()

#--------------------------------------------------
# 2. Data Exploration
#--------------------------------------------------

# Question: Which company has more publications?
comic %>% group_by(INC) %>% summarize(num_chars = n(), avg_apprs = 
                                        mean(APPEARANCES, na.rm=TRUE))


# Question: Are there more villains in DC comics?
table(comic$ALIGN)

library(ggplot2)
# create a bar plot for count of type of characters
# Help: 
#      - geom_bar() creates a bar plot
#      - geom_bar(aes(fill=INC)) creates two barplots per INC (on the same plot)
#      - position='dodge' is an option for geom_bar which creates side by 
#side bar plots not stacked
#      - rotate the x lables by theme(axis.text.x = element_text(angle = 90, 
#hjust = 1))   | hjust: changes the alignment

ggplot(comic, aes(x = ALIGN)) + 
  geom_bar(aes(fill=INC), position = 'dodge') +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill="Corporation",
       title = "Number of Characters per Type", 
       x = "Character Alignment", y = "Count")

# Question: Do villains appear more in DC comics?

# create a bar plot for appearances of types of characters
# Help: 
#       - stat='identity' in geom_bar() creates a bar plot with a y variable
ggplot(comic, aes(x = ALIGN, y = APPEARANCES)) + 
  geom_bar(aes(fill=INC) ,stat = "identity", position = 'dodge') +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Appearances per Character Type", x = "Character Alignment", 
       y = "Appearances")

# EXERCISE: Do the above for identity

# Question: When the most popular characters have been created?

# create a bar plot for x = YEAR, and y = APPEARANCES
ggplot(comic, aes(x = YEAR, y = APPEARANCES)) + 
  geom_bar(aes(fill=INC), stat='identity',alpha=0.8) + 
  theme_light() 

# Question: Who are the top 10 most popular characters?


# top 10 overall
library(tidyverse)
comic %>% arrange(-APPEARANCES) %>% slice(1:10) %>% 
  select(name, INC, APPEARANCES)

# top 10 for DC and Marvel (two ways)
comic %>% filter(INC == "dc") %>% arrange(-APPEARANCES) %>% slice(1:10) %>%
  select(name, INC, APPEARANCES)

#top 10 for dc
comic %>% group_by(INC) %>% arrange(-APPEARANCES) %>% slice(1:10) %>%
  select(name, INC, APPEARANCES)

# Question: What is the most popular Eye color?

# create a bar plot for Eye color


# do the above for non-missing


# Question: Which creator has higher female character ratio?
comic %>% 
  count(INC, SEX) %>% 
  group_by(INC) %>% 
  mutate(sex_ratio = prop.table(n)) %>% 
  select(-n) %>% 
  spread(INC, sex_ratio)
  

# use table() to create the frequency table
table(comic$SEX,comic$INC)
# now create a proportion table
prop.table(table(comic$SEX,comic$INC),2)

round(prop.table(table(comic$SEX,comic$INC),2),3)

#--------------------------------------------------
# 3. Data Cleaning
#--------------------------------------------------

# missing
colSums(is.na(comic))

# cleaning
comic_clean <- comic %>% drop_na(APPEARANCES,HAIR,YEAR)

# check how many missing variables are there for APPEARANCE
is.na(comic_clean$APPEARANCES)

# let's create a variable to show the number of years since inception
# and convert the variables that should be factor
comic_clean <- comic_clean %>% 
  mutate(age = 2021 - YEAR,
         ALIGN = factor(ALIGN),
         EYE = factor(EYE),
         HAIR = factor(HAIR),
         ALIVE = factor(ALIVE),
         INC = factor(INC))

# The goal is to use qualitative variables in our regression model.
# First, check how many levels are there for ALIGN and HAIR: use levels()
levels(comic_clean$ALIGN)
levels(comic_clean$HAIR)
length(levels(comic_clean$HAIR))
# There are 28 levels for HAIR For the model, we'd better to
#   choose a few more important levels and stack others into one category.

# Let's start from HAIR
# use table function to check the frequency of the levels
table(comic_clean$HAIR)

# bar plot of HAIR
ggplot(comic_clean, aes(HAIR)) +
  geom_bar() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# we can use fct_lump() function to select the more frequent levels
# n is number of levels we want without others
# create a new variable HAIR_simple for the reduced factors
comic_clean <- comic_clean %>% 
  mutate(HAIR_simple = fct_lump(HAIR, n= 4))

# use table function again but for ALIGN_simple
table(comic_clean$ALIGN_simple)

ggplot(comic_clean, aes(HAIR_simple)) +
  geom_bar() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# We can do this for ALIGN as well.
# As an exercise create ALIGN_simple by keeping the top 2 level:
#       Good Character and Bad Character

# We want to build a model to predict
#   whether a character is popular or not

# Create a dummay (binary) variable based on APPEARANCES
#   popular = 1 if APPEARANCES > 30
#   popular = 0 otherwise
comic_clean <- comic_clean %>% mutate(popular_binary = ifelse(APPEARANCES > 30,
                                                              1,0),popular = 
                                        factor(popular_binary, labels=c("No", 
                                                                        "Yes")))

table(comic_clean$popular)



  


#--------------------------------------------------
# 4. Data Split
#--------------------------------------------------

# Let's make a train and test set. Take 80% of observations for train and the 
#rest for test
# use set.seed(310)




#--------------------------------------------------
# 5. Classification
#--------------------------------------------------

# Now, let's try to build a model to predict
#   whether a character is popular or not

# What is the distribution of popular

# Select the variables that you think it might be useful as predictors
# Remember, you cannot use APPEARANCES (we used it to define our target)

# Use train data to build a logit model to predict a popular character





# Interpret the coefficients


# Score the models for both train and test



# Choose 0.5 cutoff to classify popularity



# Create the confusion matrix for the above cutoff
# and calculate accuracy, sensitivity, and specificity




# Do you think 0.5 is a good cutoff?
# How should we change it?

# Plot the ROC curve on train and decide which cutoff is the best



# Compare AUC for train and test. Do you see overfitting?



# Overall, what do you infer on comic characters' popularity?
