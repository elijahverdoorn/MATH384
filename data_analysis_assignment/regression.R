library(ggplot2)
library(tidyr)
library(dplyr)

# get the dataset
data <- read.csv("winequality-white.csv")

degrees <- 20 # the number of degrees to test

results.df <- data.frame("degree" = integer(), "train" = double(), "test" = double())

for(i in 1:degrees) {
  n <- nrow(data) # get the number of rows
  train <- sample(1:n, n/2, rep = F) # get subset of the data for training
  test <- setdiff(1:n, train) # and one for testing
  
  train.df <- data[train, c("pH", "alcohol")] # set up the data frame for training
  test.df <- data[test, c("pH", "alcohol")] # set up the data frame for testing
  
  mod <- lm(pH~poly(alcohol, i), data = train.df) # build the linear model with degree i
  pred.train <- predict(mod, newdata = train.df) # traing the model
  train.df <- mutate(train.df, pred = pred.train) # add the training data to the data frame
  
  pred.test <- predict(mod, newdata = test.df) # test the model
  test.df <- mutate(test.df, pred = pred.test) # add the test data to the data frame
  
  mse.train <- with(train.df, mean((pH - pred)^2)) # calculate information about the model
  mse.test <- with(test.df, mean((pH - pred)^2))
  c(mse.train, mse.test) # print the information about the model to the console
  results.df <- rbind(results.df, c(i, mse.train, mse.test))
}

head(results.df)