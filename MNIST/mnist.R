library(tidyr)
library(tidyverse)
library(tree)
library(randomForest)
library(e1071)

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

testImage <- as.vector((t(train.df[5,2:785])))
# deskewing
calulate_moments(image) {
    totalImage <- sum(image)
    mu_x <-     
}