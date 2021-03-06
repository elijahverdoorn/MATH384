library(ggplot2)
library(dplyr)
library(tidyr)

# function to rotate the matricies so that they are easier to work with
rotate <- function(x) t(apply(x, 2, rev))

recordData <- function(i, polyDegree, mse, bias, varience, eps) {
  mseVals[i, polyDegree] <<- mse
  biasVals[i, polyDegree] <<- bias
  varVals[i, polyDegree] <<- varience
  epsVals[i, polyDegree] <<- eps
  #browser()
}

runExperiment <- function(iteration, polyDegree) {
  ## Make the training set
  noise <- .5 # sigma
  epsilon <- rnorm(1, 0, noise)
  n <- 100 # training set size
  x <- runif(n, 0, 1) # the random numbers
  y <- trueFunction(x) + rnorm(n, 0, noise)
  #ggplot(NULL, aes(x, y)) + geom_point() + ggtitle("Training Set")
  
  # Predictions
  model <- lm(y~poly(x, polyDegree)) # a linear model using a first degree polynomial
  fhat_x0 <- predict(model, newdata=data.frame(x = x0))
  epsilon <- rnorm(1, 0, noise)
  y0 <- trueFunction(x0) + epsilon
  
  # Get the data that we want
  meanSquareError <- (y0-fhat_x0)^2
  bias <- trueFunction(x0) - fhat_x0
  varience <- fhat_x0
  epsilon # we also want to store this, even though we already have it
  recordData(i = iteration, polyDegree = polyDegree, mse = meanSquareError, bias = bias, varience = varience, eps = epsilon)
}

# Setup
x0 <- .5 # the x0
numDegrees <- 20
numIterations <- 100
trueFunction <- function(x) {
  sin(x * pi) # whatever we want to plot
}
mseVals <- matrix(nrow = numIterations, ncol = numDegrees)
biasVals <- matrix(nrow = numIterations, ncol = numDegrees)
varVals <- matrix(nrow = numIterations, ncol = numDegrees)
epsVals <- matrix(nrow = numIterations, ncol = numDegrees)
for (i in 1:numIterations) {
  for (degree in 1:numDegrees) {
    runExperiment(iteration = i, polyDegree = degree)     
    mean(mseVals[i, degree])
    var(varVals[i, degree])
    mean(biasVals[i, degree])^2
    var(epsVals[i, degree])
    
    mean(mseVals[i, degree])
    var(varVals[i, degree]) + mean(biasVals[i, degree])^2 + var(epsVals[i, degree])
  }
}

biasVals <- rotate(biasVals)

mv.df <- data.frame(deg = 1:numDegrees, bias = mean(biasVals))
ggplot(mv.df, aes(deg, bias)) + geom_point() + geom_line()