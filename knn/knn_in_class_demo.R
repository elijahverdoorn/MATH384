library(ggplot2)
library(tidyr)
library(dplyr)
library(class)

n <- 200 # number of points in training and testing data sets
x <- runif(n, 0, 1) # random data between 0 and 1, (random uniform)
y <- x < 0.5 # function to describe if x is greater or less that 1/2

data.df <- data.frame(x, y) # building a data frame
data.df # printing the data frame to console

kval <- 3 # number of neighbors
test <- runif(n, 0, 1) # the testing data, generated randomly
dim(test) <- c(n, 1) # tell the testing data how big the set is
response.predict <- knn(data.df[, "x"], test, data.df[,"y"], k = kval) # build the KNN model
response.predict # print the testing results

response.test <- test < 0.5 # the true values
response.test # print the error rate

mean(response.predict != response.test) # how did we do? calculate the error rate

# Add noise variable
data.df <- data.df %>% # some kind of mutation syntax
    mutate(w = runif(n, 0, 1)) # add a randomness variable to the data set, increasing dimension from 1 to 2 (linear to cartesian)
data.df # print it

response.test <- test[, 1] < 0.5 # compare the first column to .5
response.test # print to console
mean(response.predict != response.test) # how did we do? calculate the error rate