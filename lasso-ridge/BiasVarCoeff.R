library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)

#######################################################
## Looking at Ridge Regression vs Linear Regression
## Bias/Variance Effect on coefficient estimation
## Note: This example uses lm.ridge. ISLR uses glmet. We will use
## both.
#######################################################
##Set up some paraments
## p is the number of predictors
## N is the sample size
p <- 2
N <- 3
#######################################################
## Build the model with regression coefficients b1 and b2 (no
## intercept) and with error term with sd
sd <- 4
b1 <- 2
b2 <- 2

#######################################################
## Build a both a ridge regression (with lambda.val) and linear model
## Keep track of the coefficient estimates for each of K iterations
## Note: the data has mean = 0
#######################################################
mod.ridge <- lm.ridge(y~x1 + x2 + 0,data = data.df, lambda = 1)
mod.ridge

lambda.val <- 10
K <- 200
coef.est <- matrix(0, nrow = K, ncol = 4)
for(k in 1:K){
    x1 <- rnorm(N, 0, 1)
    x2 <- rnorm(N, 0, 1)
    x1 <- x1 - mean(x1)
    x2 <- x2 - mean(x2)
    ## no constant term
    y <- b1 * x1 + b2 * x2 + rnorm(N, 0, sd)
    data.df <- data.frame(x1, x2, y)
    mod.lm <- lm(y~x1 + x2 + 0, data = data.df)
    mod.ridge<- lm.ridge(y~x1 + x2 + 0, data = data.df, lambda = lambda.val)
    cc.lm <- coefficients(mod.lm)
    cc.ridge <- coefficients(mod.ridge)
    coef.est[k,] <- c(cc.lm, cc.ridge)
}

coef.df <- data.frame(b1.est = c(coef.est[,1], coef.est[,3]),
                      b2.est = c(coef.est[,2], coef.est[,4]),
                      type = rep(c("lm", "ridge"), each = K))
head(coef.df)


#######################################################
## Plot the coefficients.
## From this we can see a "bias/variance" tradeoff in the coefficient
## estimations
## The black dot in the middle is the true value of the coefficients
## Repeat this with different values of lambda.val, N, b1.est, b2
## Blue are the OLR model, red is the ridge regression model

ggplot(coef.df, aes(b1.est, b2.est, color = type)) +
    geom_point() +
    scale_color_manual(values = c(lm = "blue", ridge = "red")) +
    geom_point(aes(0, 0), size = 3, color = "black") +
    geom_point(aes(b1, b2), size = 3, color = "black") +
    ggtitle("Bias and Variance: LM vs Ridge")

coef.df%>%
    group_by(type)%>%
    summarize(mu1 = mean(b1.est),
              mu2 = mean(b2.est),
              sd1 = var(b1.est),
              sd2 = var(b2.est))


coef.df %>%
    group_by(type)%>%
    summarize(err1 = mean((b1 - b1.est)^2),
              err2 = mean((b2 - b2.est)^2))




#######################################################
## Assignment
##
## Write a function that will take as its arguments
## p and n
##
## keep everything else the same..b1, b2, sd, etc
## For these values of p and n, find the value of lambda (lambda.val)
## which minimizes the total error (err1+err2) for the ridge estimate.
## For this optimal lambda, how does the ridge err1+err2 compare with
## the linear model err1+err2. Note the linear model err1+err2 is
## equivalent to the ridge regression with lambda=0.
##
## Note: lm.ridge allows you to assign a sequence of lambda values
## lambda.vals <- seq(0,100,2)
## mod.ridge<- lm.ridge(y~x1+x2+0,data=data.df,lambda=lambda.vals)
## coefficients(mod.ridge)
## this means you don't have to explicitely loop to find the optimal
## lambda value. If you are careful, you might be able to do it one
## big dplyr command!
#######################################################

assignment <- function (p, n) { 
    # p is the number of predictors
    # n is the sample size
    
    lambda.vals <- seq(0, 100, 2)
    mod.lm <- lm(y~x1 + x2 + 0, data = data.df)
    mod.ridge<- lm.ridge(y~x1 + x2 + 0, data = data.df, lambda = lambda.vals)
    coefficients(mod.ridge)
    
    lambda.vals
}
