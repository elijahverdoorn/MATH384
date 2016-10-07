library(ggplot2)
library(tidyr)
library(dplyr)
library(FNN)

allData.df <- read.csv("MLB_1985_2013.csv") # read all the data
allData.df <- allData.df[complete.cases(allData.df),]
trainingData.df <- subset(allData.df, yearID < 2012 & !is.null(Allstar.next) & !is.null(salary) & !is.null(AB)) # get the training data
testingData.df <- subset(allData.df, yearID == 2012 & !is.null(Allstar.next) & !is.null(salary) & !is.null(AB)) # get the testing data

# predict on something with logistic



# predict on something with knn
maxNeighbors <- 10
features <- "salary"
knnResults <- matrix(ncol = 2, nrow = maxNeighbors) # some data structures to hold the results of the work
for (i in 1:maxNeighbors) {
    knnPredictions <- knn(trainingData.df[features],
                          testingData.df[features], 
                          trainingData.df[,"Allstar.next"], k = i) # make a model with knn

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

