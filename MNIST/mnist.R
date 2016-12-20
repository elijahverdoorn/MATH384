library(tidyr)
library(tidyverse)
library(tree)
library(randomForest)
library(e1071)
library(moments)
library(glmnet)
library(doParallel)
library(MASS)
library(ROCR)

# import CMD implementation
source("cmd.R")

# enable parallel computation
registerDoParallel(cores = 8)

# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org, modified by Elijah Verdoorn (elijahverdoorm.com) 11/30/16
load_mnist <- function() {
    load_image_file <- function(filename) {
        ret = list()
        f = file(filename,'rb')
        readBin(f,'integer',n=1,size=4,endian='big')
        ret$n = readBin(f,'integer',n=1,size=4,endian='big')
        nrow = readBin(f,'integer',n=1,size=4,endian='big')
        ncol = readBin(f,'integer',n=1,size=4,endian='big')
        x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
        ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
        close(f)
        ret
    }
    load_label_file <- function(filename) {
        f = file(filename,'rb')
        readBin(f,'integer',n=1,size=4,endian='big')
        n = readBin(f,'integer',n=1,size=4,endian='big')
        y = readBin(f,'integer',n=n,size=1,signed=F)
        close(f)
        y
    }
    # Modified to handle changed file names, ERV 11/30/16
    train <<- load_image_file('train_set_images')
    test <<- load_image_file('test_set_images')
    
    train$y <<- load_label_file('train_set_labels')
    test$y <<- load_label_file('test_set_labels')  
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
    image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

load_mnist()

# Make data frames from the imported data described above
train.df <- data.frame(train)
test.df <- data.frame(test)

# print some to make sure that it's working, and find out how to access the data from the data frame
train$x[5,]
as.vector(t(train.df[5,2:785]))

# use both the lists and data frames to access the data and display images
show_digit(train$x[7,])
show_digit(as.vector(t(train.df[7,2:785])))

# average the images to get an idea of the distribution
uniqueCols <- unique(train.df$y) # get the unique digits in the response col (y)
avgVals <- matrix(nrow = length(uniqueCols), ncol = 784) # the matrix that will hold the averages
for (i in 1:length(uniqueCols)) { # for each unique digit
    # get values for the current digit, dropping the n and response columns
    currentVals.df <- train.df[train.df$y == uniqueCols[i],]
    drops <- c("n", "y")
    currentVals.df <- currentVals.df[ , !(names(currentVals.df) %in% drops)]
    for(j in 1:784) { # for each pixel (the images are 28x28, 28^2 = 784)
        avgVals[i,j] <- mean(currentVals.df[,j])
    } 
}

# display the image averages
for (i in 1:nrow(avgVals)) {
    show_digit(avgVals[i,])
}

# Build train and test data matrices. This is the form that glmnet uses.


#######################
## The Training Data ##
#######################
# we need to do some precomputation here so that our models actually finish training. This is where CMD comes in.
c <- 78
r <- 6000
# drop the n column cause it's useless
drops <- c("n")
train.df <- train.df[, !(names(train.df) %in% drops)]
# add an id column
train.df$id <- seq.int(nrow(train.df))
decompMatrix <- as.matrix(train.df[,1:784])
decompTrain <- cmd_decomposition(decompMatrix, c, r)

responses <- train.df[decompTrain$rows, "y"]
cDecomp.df <- data.frame(decompTrain$C)
uDecomp.df <- data.frame(decompTrain$U)
rDecomp.df <- data.frame(decompTrain$R)

rTilde <- sample(1:nrow(decompMatrix), r, replace = FALSE)
cTilde <- sample(1:ncol(decompMatrix), c, replace = FALSE)
cDecompSubset <- decompTrain$C[rTilde, ]
uDecomp <- decompTrain$U
rDecompSubset <- decompTrain$R[, cTilde]
rDecompSubset[is.na(rDecompSubset)] <- 0
trainDecomp.df <- data.frame(cDecompSubset %*% uDecomp %*% rDecompSubset)
trainDecomp.df$y <- responses[rTilde]
# in theory we can build models on trainDecomp.df with y as the response variable

# this is most likely the wrong solution, but let's try anyway
testingData.df <- data.frame(t(decompTrain$U))
testingData.df$y <- train.df[decompTrain$rows, "y"] # can I build a model on this?

x.train <- as.matrix(testingData.df[, ! colnames(trainDecomp.df) %in% c("y")])
y.train <- as.matrix(testingData.df[,"y"])

######################
## The Testing Data ##
######################
# we need to do some precomputation here so that our models actually finish training. This is where CMD comes in.
c <- 78
r <- 6000
# drop the n column cause it's useless
drops <- c("n")
test.df <- test.df[, !(names(test.df) %in% drops)]
# add an id column
test.df$id <- seq.int(nrow(test.df))
decompMatrix <- as.matrix(test.df[,1:784])
decompTest <- cmd_decomposition(decompMatrix, c, r)

responses <- test.df[decompTest$rows, "y"]
cDecomp.df <- data.frame(decompTest$C)
uDecomp.df <- data.frame(decompTest$U)
rDecomp.df <- data.frame(decompTest$R)

rTilde <- sample(1:nrow(decompMatrix), r, replace = FALSE)
cTilde <- sample(1:ncol(decompMatrix), c, replace = FALSE)
cDecompSubset <- decompTest$C[rTilde, ]
uDecomp <- decompTest$U
rDecompSubset <- decompTest$R[, cTilde]
rDecompSubset[is.na(rDecompSubset)] <- 0
testDecomp.df <- data.frame(cDecompSubset %*% uDecomp %*% rDecompSubset)
testDecomp.df$y <- responses[rTilde]
# in theory we can build models on trainDecomp.df with y as the response variable

# this is most likely the wrong solution, but let's try anyway
testingData.df <- data.frame(t(decompTest$U))
testingData.df$y <- train.df[decompTest$rows, "y"] # can I build a model on this?

# Test
x.test <- as.matrix(testingData.df[, ! colnames(testingData.df) %in% c("y")])
y.test <- as.matrix(testingData.df[,"y"])

# We need to find the optimal lambda. To do so, we cross-validate the
# mse across all the values of lambda. glmnet will do this
# automatically

numLambda <- 10
expVals <- seq(-4,4,length=numLambda)
lambda.grid <- 10^expVals
plot(expVals,lambda.grid)

cv.ridge <- cv.glmnet(x = x.train, y = as.factor(y.train), lambda = lambda.grid, family = "multinomial", type.logistic = "modified.Newton", parallel = TRUE)
plot(cv.ridge)

bestLambda <- cv.ridge$lambda.min
mod.bestLambda <- glmnet(x.train, y.train, alpha = 0, lambda = bestLambda, intercept = TRUE)

# Ridge prediction for optimal lambda
phat <- predict(cv.ridge, newx = x.test, type = "class")
plot(cv.ridge$glmnet.fit, xvar = "lambda")

correct <- 0
for(i in 1:nrow(phat)) {
    if (phat[i] == y.test[i]) {
        correct <- correct + 1
    }
}
correct

# That wasn't so good. Let's try another model
K <- 10
N <- nrow(df)
folds <- sample(1:K, N, rep = T)
errs <- rep(0, K)
for(k in 1:K){
    train <- !folds == k
    test <-  folds == k
    svmfit <- svm(class ~ x + y, data = df[train,], kernel = "linear", cost = C, scale = T)
    pred.svm <- predict(svmfit, newdata = df[test,])
    pred.svm
    errs[k] <- with(dat.df[test,], mean(class != pred.svm))
}
mean(errs)

