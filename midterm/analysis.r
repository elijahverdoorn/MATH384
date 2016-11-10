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
doRidgeRegression <- function(dataFrame, responseVariableIndex) {
    xTrain.mat <- data.matrix(dataFrame[,4:responseVariableIndex])
    yTrain.mat <- data.matrix(dataFrame[,responseVariableIndex])
    xTest.mat <- data.matrix(dataFrame[,4:responseVariableIndex])
    yTest.mat <- data.matrix(dataFrame[,responseVariableIndex])
    
    numLambda <- 100
    expVals <- seq(-4, 4, length = numLambda)
    lambdaGrid <- 10^expVals
    
    crossValidatedRidgeModel <- cv.glmnet(xTrain.mat, yTrain.mat, alpha = 0, lambda = lambdaGrid, intercept = TRUE) # cross validate
    
    (lambda0 <- crossValidatedRidgeModel$lambda.min)

    return(glmnet(xTrain.mat, yTrain.mat, alpha = 0, lambda = lambda0, intercept = TRUE)) # use the optimal lambda
}

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

# Ridge Regression
numberIterations <- 5
for(i in 1:numberIterations) {
    bootstrappedData.df <- data.frame(bootstrapData(trainingData.df))
    model <- doRidgeRegression(bootstrappedData.df, 58)
}

