library(ggplot2)library(dplyr)
library(tree)
library(ISLR)
library(rpart)
library(glmnet)
library(randomForest)
library(gbm)

# Get the data
#als.df <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)
alsTrain.df <- filter(als.df, !testset)
alsTest.df <- filter(als.df, testset)

numIter <- 10
mse.mat <- matrix(nrow = numIter, ncol = 3)


# Do random forest
prediction.mat <- matrix(nrow = nrow(alsTest.df), ncol = numIter)
for (i in 1:numIter) {
    p <- ncol(alsTrain.df) - 1
    numTrees <- i
    als.bag <- randomForest(dFRS ~ .,
                            data = alsTrain.df,
                            mtry = p/3, ## For Random Forest
                            ntree = 1)
    preds.bag <- predict(als.bag, newdata = alsTest.df)
    prediction.mat[,numTrees] <- preds.bag
}
for (i in 1:numIter) {
    mseForest <- mean((prediction.mat[,1:i] - alsTest.df$dFRS)^2)
    mseForest 
    mse.mat[i, 1] <- mseForest
}

for(i in 1:numIter) {
    # Do boosting
    numTrees <- i
    als.boost <- gbm(dFRS ~  . ,
                    data = alsTrain.df[2:ncol(alsTrain.df)],
                    n.trees = numTrees,
                    distribution = "gaussian",
                    interaction.depth = 4,
                    shrinkage = 0.05,
                    cv.folds = 5,
                    verbose = T)

    als.boost
    summary(als.boost)
    best.trees <- gbm.perf(als.boost)
    best.trees
    prob.boost <- predict(als.boost, newdata = alsTest.df,
                      n.trees = best.trees, repsonse = "response")  
    mseBoost <- mean((preds.bag - alsTest.df$dFRS)^2)
    mseBoost 
    mse.mat[i, 2] <- mseBoost
}

# Do lasso
# TODO: add lasso
als.lasso <- glmnet(as.matrix(alsTrain.df[, -2]), alsTrain.df$dFRS, alpha = 1, lambda = 1, intercept = TRUE)
als.lasso
summary(als.lasso)
preds.lasso <- predict(als.lasso, newx = as.matrix(alsTest.df[, -2]))
mse.lasso <- mean((preds.lasso - alsTest.df$dFRS)^2)
mse.lasso

# plot it
ggplot(data = data.frame(mse.mat), aes(1:numIter)) + geom_line()
# TODO: add a loop around all these models, changing the number of trees. store the MSE from each, then graph them all