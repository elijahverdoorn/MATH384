library(dplyr)
library(glmnet)
library(ggplot2)
library(glmnet)
library(MASS)
library(class)

# randomize the sample
getSampleIndexes <- function(percent = 0.85, nrows) {
    return(floor(percent * nrows))
}

# bootstrap the data
bootstrapData <- function(dataFrame) {
    return(dataFrame[sample(nrow(dataFrame), size = nrow(dataFrame), replace = T),])
}

# perform ridge regression
# Args: 
# dataFrame = the data
# responseVariableIndex = the index of dataFrame that contians the response data
doRidgeRegression <- function(trainDataFrame, testDataFrame, responseVariableIndex) {
    xTrain.mat <- data.matrix(trainDataFrame[,4:responseVariableIndex])
    yTrain.mat <- data.matrix(trainDataFrame[,responseVariableIndex])
    xTest.mat <- data.matrix(testDataFrame[,4:responseVariableIndex])
    yTest.mat <- data.matrix(testDataFrame[,responseVariableIndex])
    
    numLambda <- 100
    expVals <- seq(-4, 4, length = numLambda)
    lambdaGrid <- 10^expVals
    
    crossValidatedRidgeModel <- cv.glmnet(xTrain.mat, yTrain.mat, alpha = 0, lambda = lambdaGrid, intercept = TRUE) # cross validate
    lambda0 <- crossValidatedRidgeModel$lambda.min
    model <- glmnet(xTrain.mat, yTrain.mat, alpha = 0, lambda = lambda0, intercept = TRUE) # use the optimal lambda
    prediction <- predict(model, type="response", newx = xTest.mat)

    return(prediction)
}

# the summary function described in the assignment
# Args:
# yhat = the prediction set
# comparison = the total number of elligible voters at each polling place
sumFunc <- function(yhat, comparison) {
    #predictionsCandidate <- sum(yhat)
    #totalVoters <- sum(comparison)
    
    return(mean(yhat / comparison))
}

# Constant Parameters
sStar <- .5 # the number needed to win the election

# Data setup
countyFacts.df <- read.csv("county_facts.csv")
usElectCounty.df <- read.csv("us_elect_county.csv")

masterData.df <- merge(countyFacts.df, usElectCounty.df, by = "fips") # table join on the two CSVs so that we can train/test
masterData.df <- sapply(masterData.df, as.numeric) # for some reason all the response data was coming in as characters. This converts those to numeric.

# randomize the selection of the data
sampleSize <- getSampleIndexes(nrows = nrow(masterData.df))
trainIndex <- sample(seq_len(nrow(masterData.df)), size = sampleSize)

trainingData.df <- masterData.df[trainIndex,]
testingData.df <- masterData.df[-trainIndex,]

# record the population of each location for use later
trainingPopulations.mat <- matrix(nrow = nrow(testingData.df), ncol = 1)
trainingPopulations.mat <- testingData.df[,57]
trainingPopulations.mat <- trainingPopulations.mat + testingData.df[,59]

# Ridge Regression
numberIterations <- 50
predictions.mat <- matrix(nrow = nrow(testingData.df), ncol = numberIterations)
for(i in 1:numberIterations) {
    bootstrappedData.df <- data.frame(bootstrapData(trainingData.df))
    prediction <- doRidgeRegression(bootstrappedData.df, testingData.df, 57)
    predictions.mat[,i] <- prediction # store a matrix of the predictions
}

k <- 100 # the number of times to do the sampling/calculating
sList <- matrix(nrow = k, ncol = 1)
for(j in 1:k) {
    yhatSample <- matrix(nrow = nrow(predictions.mat), ncol = 1) # the yhat in (2) in the assignment
    for(i in 1:nrow(predictions.mat)) {
        yhatSample[i,1] <- predictions.mat[i, sample(1:ncol(predictions.mat), 1)] # randomly sampling from the existing prediction set
    }
    
    sList[j,1] <- sumFunc(yhatSample, trainingPopulations.mat) # run the sample function on this set
}


victoryPercent <- sum(sList) / k # average the scores to get a % chance of victory
victoryPercent
