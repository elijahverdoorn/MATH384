library(ggplot2)
library(tidyr)
library(dplyr)
library(class)

maxKVal <- 100
means <- matrix(nrow = maxKVal, ncol = 1)

for (i in 1:maxKVal) {
  kval <- i # number of neighbors
  
  train.df <- read.csv("adult.csv") # read in the training data
  test.df <- read.csv("adult_test.csv") # read in the testing data
  
  response.predict <- knn(train.df[,c("Age", "race", "sex")], 
                          test.df[,c("Age", "race", "sex")], 
                          train.df[,c("X..50k")], k = kval) # build the KNN model
  response.predict # print the testing results
  
  test.df <- mutate(test.df, class.pred = response.predict)
  # how did we do? calculate the error rate
  means[i] <- mean(test.df[,c("X1")] != test.df["class.pred"])
}

min(means) # at k = 78

