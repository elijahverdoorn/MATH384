library(dplyr)
library(ggplot2)
library(tidyr)


head(mtcars) # look at data
ggplot(mtcars, aes(disp, mpg)) + geom_point() # plot it

n <- nrow(mtcars) # get the number of rows
train <- sample(1:n, n/2, rep = F) # get subset of the data for training
test <- setdiff(1:n, train) # and one for testing

train.df <- mtcars[train, c("disp", "mpg")] # set up the data frame for training
test.df <- mtcars[test, c("disp", "mpg")] # set up the data frame for testing

mod <- lm(mpg~poly(disp, 1), data = train.df) # build the linear model with degree 1
pred.train <- predict(mod, newdata = train.df) # traing the model
train.df <- mutate(train.df, pred = pred.train) # add the training data to the data frame

pred.test <- predict(mod, newdata = test.df) # test the model
test.df <- mutate(test.df, pred = pred.test) # add the test data to the data frame

mse.train <- with(train.df, mean((mpg - pred)^2)) # calculate information about the model
mse.test <- with(test.df, mean((mpg - pred)^2))
c(mse.train, mse.test) # print the information about the model to the console
