library(tidyr)
library(tidyverse)
library(tree)
library(randomForest)
library(e1071)
library(moments)
library(EBImage) # for rotation
library(glmnet)
library(doParallel)

# enable parallel computation
registerDoParallel(cores = 4)


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

calc_theta <- function(mu11, mu20, mu02) {
  return(.5 * tan((2 * mu11) / ((mu20 - mu02)^-1)))
}

construct_rotation_matrix <- function(theta, size) {
  rotation_mat <- matrix(nrow = 2, ncol = 2)
#  for (i in 1:size) {
#    for (j in 1:size) {
#      rotation_mat[i,j] <- 0
#    }
#  }
#  for(i in 1:size) {
#    rotation_mat[i,i] <- 1
#  }
  rotation_mat[1,1] <- cos(theta)
  rotation_mat[1,2] <- -sin(theta)
  rotation_mat[2,1] <- sin(theta)
  rotation_mat[2,2] <- cos(theta)
  return(rotation_mat)
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

testImage <- as.vector((t(train.df[7,2:785])))
testImage.matrix <- matrix(testImage, nrow = 28, ncol = 28)
imageForRotation <- Image(data = testImage.matrix, dim = c(28,28))
display(getFrame(imageForRotation, 1)/255, method = "raster", interpolate = FALSE)

show_digit(testImage)

# deskewing
moments <- all.moments(testImage, order.max = 3, central = TRUE, absolute = FALSE, na.rm = TRUE)
moments


theta <- calc_theta(moments[2], moments[3], moments[4])
fixedImage.image <- rotate(imageForRotation, angle = theta, output.dim = dim(imageForRotation)[1:2]) 
display(getFrame(fixedImage.image, 1)/255, method = "raster", interpolate = FALSE)
fixedImage.matrix <- as.data.frame(cbind(as.numeric(dimnames(fixedImage.image)[[3]]), do.call(rbind, lapply(getFrames(fixedImage.image), as.matrix))))
show_digit(as.matrix(fixedImage.matrix))
show_digit(testImage.matrix)

# some kind of classification
# I'm going to do lasso, since that'll drop out variables and there are a lot of variables that'll be all zero

#######################################################
## Build full,train, and test data matrices. This is the form
## that glmnet uses.
#######################################################
numLambda <- 10
expVals <- seq(-4,4,length=numLambda)
lambda.grid <- 10^expVals
plot(expVals,lambda.grid)

## Train
x.train <- as.matrix(train.df[,2:785])
y.train <- as.matrix(train.df[,786])

##Test
x.test <- as.matrix(test.df[,2:785])
y.test <- as.matrix(test.df[,786])

#################################
## We need to find the optimal lambda. To do so, we cross-validate the
## mse across all the values of lambda. glmnet will do this
## automatically

## Use grid of lambda values with cv.glmnet. The plot shows
## cross-validated with error bars as a function of log(lambda) base=2.
cv.ridge <- cv.glmnet(x = x.train, y = as.factor(y.train), lambda = lambda.grid, family = "multinomial", type.logistic = "modified.Newton", parallel = TRUE)
plot(cv.ridge)

## Ridge prediction for optimal lambda
phat <- predict(mod.ridge, newx = x.test, s = cv.ridge$lambda.1se, type = "response")
yhat <- apply(phat, 1, which.max) - 1
ot <- table(yhat, y.test)
print(ot)
sum(diag(ot)) / 100
plot(cv.ridge$glmnet.fit, xvar = "lambda")
