library(ggplot2)
library(tidyr)
library(dplyr)
library(class)

kval <- 3 # number of neighbors

colleges.df <- read.csv("collegeScoreboard2015Scaled2.csv") # read in the training data
test.df <- read.csv("collegeScoreboard2015Scaled1.csv") # read in the testing data

response.predict <- knn(colleges.df[2:4], test[2:4], colleges.df[,c("Type")], k = kval) # build the KNN model
response.predict # print the testing results

test.df <- mutate(test.df, class.pred = response.predict)
mean(test.df["Type"] != test.df["class.pred"]) # how did we do? calculate the error rate