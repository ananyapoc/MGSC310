#Class 6 Lab 
library(ggplot2)
university <- read.csv("university.csv")
head(university)

#scatter plot GRE against GPA
ggplot(data = university, mapping = aes(x=GPA, y=GRE)) + geom_point(alpha = 0.5) + theme_light() + xlab("GPA Score (out of 4)") + ylab("GRE Score")

#use facet_wrap for creating multiple scatter plots for different groups
ggplot(data = university, mapping = aes(x=GPA, y=GRE, color = factor(admitted))) + geom_point() + facet_wrap(~department) 

#add a smoothing line 
ggplot(data = university, mapping = aes(x=GPA, y=GRE, color = factor(admitted))) + geom_point()+geom_smooth(method = "lm") 
