library(ggplot2)
library(tidyr)
library(dplyr)
library(FNN)
library(MASS) # for LDA

allData.df <- read.csv("MLB_1985_2013.csv") # read all the data
allData.df <- allData.df[complete.cases(allData.df),]
trainingData.df <- subset(allData.df, yearID < 2011 & !is.null(Allstar.next) & !is.null(salary) & !is.null(AB)) # get the training data
testingData.df <- subset(allData.df, yearID == 2011 & !is.null(Allstar.next) & !is.null(salary) & !is.null(AB)) # get the data to use to test the models
newData.df <- subset(allData.df, yearID == 2012 & !is.null(Allstar.next) & !is.null(salary) & !is.null(AB)) # get the 2012 testing data

# predict on something with logistic
logisticRegrssionModel <- glm(Allstar.next~salary, family = binomial, data = testingData.df)
summary(logisticRegrssionModel)
logisticRegressionProb <- predict(logisticRegrssionModel, data = testingData.df) # why does this give 9728 results, rather than 383? I'm using testing data, right?
allStarThreshold <- .5
logisticTesting.df <- testingData.df
logisticTesting.df <- mutate(testingData.df, logisticBinaryPrediction = ifelse(logisticRegressionProb > allStarThreshold, TRUE, FALSE))

with(logisticTesting.df, table(Allstar.next, logisticBinaryPrediction))
truePositives <- sum(logisticTesting.df$Allstar.next == T)
falsePositves <- sum(logisticTesting.df$Allstar.next == F & logisticTesting.df$logisticBinaryPrediction == T)
trueNegatives <- sum(logisticTesting.df$Allstar.next == F)
falseNegatives <- sum(logisticTesting.df$Allstar.next == T & logisticTesting.df$logisticBinaryPrediction == F)
correctPositives <- sum(logisticTesting.df$Allstar.next == T & logisticTesting.df$logisticBinaryPrediction == T)

totalValueLogistic <- (falseNegatives * 20) + (falsePositves * 50) - (correctPositives * 10)

# predict on something with knn
maxNeighbors <- 150
features <- "salary"
knnResults <- matrix(ncol = 2, nrow = maxNeighbors) # some data structures to hold the results of the work
totalValueknn <- 9999999999999 # so that the first time we always replace it

for (i in 1:maxNeighbors) {
    knnTesting.df <- testingData.df
        
    knnPredictions <- knn(trainingData.df[features],
                          knnTesting.df[features], 
                          trainingData.df[,"Allstar.next"], k = i) # make a model with knn
    
    knnTesting.df <- mutate(knnTesting.df, knnPredictions)
    with(knnTesting.df, table(Allstar.next, knnPredictions))
    truePositives <- sum(knnTesting.df$Allstar.next == T)
    falsePositves <- sum(knnTesting.df$Allstar.next == F & knnTesting.df$knnPredictions == T)
    trueNegatives <- sum(knnTesting.df$Allstar.next == F)
    falseNegatives <- sum(knnTesting.df$Allstar.next == T & knnTesting.df$knnPredictions == F)
    correctPositives <- sum(knnTesting.df$Allstar.next == T & knnTesting.df$knnPredictions == T)
    
    currentValueknn <- (falseNegatives * 20) + (falsePositves * 50) - (correctPositives * 10)
    if (currentValueknn < totalValueknn) {
        totalValueknn <- currentValueknn
    }
        
    knnResults[i, 2] <- mean(c(knnPredictions) != testingData.df["Allstar.next"]) # how did we do? calculate the error rate
    knnResults[i, 1] <- i
    # someday I'll learn to use ggplot. not today.
#    knnGraphData.df <- knnGraphData.df %>%
#        mutate(class = knnPredictions) %>%
#        # some probabilities
#        mutate(knn.prob = attr(knnPredictions, "prob"),
#               knn.prob2 = ifelse(class == "A", knn.prob, 1 - knn.prob))
#    gg.knn <- ggplot() +
#        geom_point(data = knnGraphData.df, aes(x, y, color = class), shape = 15, size = 5, alpha = .05) +
#        geom_point(data = train.df, aes(x, y, color = factor(class)), size = 2) +
#        stat_contour(data = knnGraphData.df, aes(x, y, z = knn.prob), breaks = c(.5)) +
#        scale_color_manual(values = c("red", "blue")) +
#        ggtitle(sprintf("KNN neighbor classification regions (k=%s)", i))
#    gg.knn
}

knnResults.df <- data.frame(knnResults)
colnames(knnResults.df) <- c("neighbors", "error")
ggplot(knnResults.df, aes(neighbors, error)) + 
    geom_point() + geom_line() + 
    geom_smooth(se = F) +
    ggtitle("Test Error rate by number of neighbors")

# predict on something with LDA
ldaTesting.df <- testingData.df
ldaModel <- lda(Allstar.next~salary, data = trainingData.df)
ldaModel
summary(ldaModel)
ldaPredictions <- predict(ldaModel, testingData.df)

preds <- ldaPredictions$class
ldaTesting.df <- testingData.df %>% 
  mutate(ldaPredictions = preds)

with(ldaTesting.df, table(Allstar.next, ldaPredictions))
truePositives <- sum(ldaTesting.df$Allstar.next == T)
falsePositves <- sum(ldaTesting.df$Allstar.next == F & ldaTesting.df$ldaPredictions == T)
trueNegatives <- sum(ldaTesting.df$Allstar.next == F)
falseNegatives <- sum(ldaTesting.df$Allstar.next == T & ldaTesting.df$ldaPredictions == F)
correctPositives <- sum(ldaTesting.df$Allstar.next == T & ldaTesting.df$ldaPredictions == T)

totalValueLDA <- (falseNegatives * 20) + (falsePositves * 50) - (correctPositives * 10)

totalValueLDA
totalValueknn
totalValueLogistic