library(readxl)
sparks_data <- read_excel("C:/Users/rofsa/Downloads/spark internship/sparks_data.xlsx", 
                          col_types = c("numeric", "numeric"))
View(sparks_data)

# Importing necessary libraries
library(tidyverse)
library(reshape2)
library(lattice)
# Checking any missing values.
sum(is.na(sparks_data))
# No missing values, making a plot with regression line.
plot(Scores~Hours, data = sparks_data)
Linearplot<-abline(lm(Scores~Hours, data = sparks_data), col = 'green')
# We can see that all the points are coming nearby regession line. Means Hours and Scores possesses some strong relation.
# checking the corelation between these 2 .
corrl_data<-round(cor(sparks_data),2)
corrl_data
# We can see that Scores and Hours are showing very high positive correlation.
# Checking with the heat map.
melted_corrl_data<-melt(corrl_data)
View(melted_corrl_data)
ggplot(data = melted_corrl_data, mapping = aes(x = Var1, y = Var2, fill= value))+geom_tile()
# Checking the heat map which also shows that the relation between hous and scores is very strong.
# We have to predict the score if a person studies for 9.25 hours so making a data test data frame for it.
pd<-as.data.frame(9.25)
colnames(pd)<-"Hours"
# Doing Linear regression for the training data.
LinearModel<-lm(Scores~Hours, data = sparks_data)
summary(LinearModel)
# So, now it becomes prominent that Hours shows a significant impact on Scores. Based on the p value, which is less than .05.
# Our model is-
# Scores = 2.4837 + 9.7758*Hours
# Means a change of 1 unit in hours will change the scores by 9.7758.
#Checking the score based on our model.
Scores = 2.4837+9.7758*9.25
Scores
# Predicting the scores if someone will study for 9.25 hours.
predict(LinearModel, newdata = pd)
# So, Our model has predicted that if someone will study for 9.25hours so the score will be approx 93.
# Thank You.