library(ggplot2)
library(tidyr)
library(dplyr)
library(class)

n <- 200 # number of points in training and testing data sets
colleges.df <- read.csv("collegeScoreboard2015Scaled2.csv") # read in the training data

kval <- 3 # number of neighbors
test <- read.csv("collegeScoreboard2015Scaled1.csv") # read in the testing data

response.predict <- knn(colleges.df[2:4], test[2:4], colleges.df[,c("Type")], k = kval) # build the KNN model
response.predict # print the testing results
