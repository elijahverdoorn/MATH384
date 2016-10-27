library(ISLR)
library(glmnet)
library(ggplot2)
library(tidyr)
library(dplyr)

#######################################################
## A comparison of OLS, Ridge Regression, and Lasso
## On Prostate data set
## https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
#######################################################
prostate.df=read.csv("Prostate.csv",row.names=1,sep="\t")
prostate.df$train
nrow(prostate.df)
names(prostate.df)

#######################################################
## Training and Testing data frames. The train/test selectors are part
## of the data set
#######################################################
train.rows <- with(prostate.df,train)
train.rows
nn <- nrow(prostate.df)
train <- (1:nn)[train.rows]
train
## Drop the train selector

prostate.df <- select(prostate.df,-train)

train.df <- prostate.df[train,]
test.df <-  prostate.df[-train,]

nrow(train.df)
nrow(test.df)

############################################
##1 Ordinary Least Squares model
## Train/test combo

mod.lm <- lm(lpsa~.,data=train.df)

summary(mod.lm)
coef.lm <- cbind(coef(mod.lm))
coef.lm


#######################################################
## MSE on test set
############################################
pred.lm <- predict(mod.lm,newdata=test.df)
(mse.lm <- with(test.df,mean( (pred.lm-lpsa)^2)))


############################################
## Ridge Regression with glmnet
############################################
prostate.mat <- data.matrix(prostate.df)
dim(prostate.mat)


#######################################################
## Build full,train, and test data matrices. This is the form
## that glmnet uses.
#######################################################
dim(prostate.df)
head(prostate.mat)
x.full <- prostate.mat[,-9]
y.full <- prostate.mat[,9]

## Train

x.train <- x.full[train,]
y.train <- y.full[train]

##Test
x.test <- x.full[-train,]
y.test <- y.full[-train]


#######################################################
##1  Ridge Regression
#######################################################


#######################################################
## Quick ridge regression test with a single lambda using the training
## data set
mod.ridge <- glmnet(x.train,y.train,alpha=0,lambda=1,intercept=TRUE)
coef.ridge <- coef(mod.ridge)


## Compare ridge coefficients with linear model coefficients
cbind(coef.ridge,coef.lm)

##Make a prediction
pred.ridge <- predict(mod.ridge,newx=x.test)
## How did we do??
mse.rid0 <- mean( (y.test-pred.ridge)^2)
## Not so bad, but not any better than OLS
c(mse.lm,mse.rid0)


#######################################################
## Now repeat with a  grid of lambdas
## Build ridge regression models for a grid of lambda values
## a good way to do this is to size the lambda values geometrically
## (vs arithmetically)
numLambda <- 100
expVals <- seq(-4,4,length=numLambda)
lambda.grid <- 10^expVals
plot(expVals,lambda.grid)

############################################
## apply glmnet, alpha=0 means use ridge. For Lasso, alpha=1
mod.ridge <-
    glmnet(x.train,y.train,alpha=0,lambda=lambda.grid,intercept=TRUE)

##glmnet has a built-in plot function. This shows the values of the
##coefficients (no intercept) as the the overall size of the
##coefficients shrinks to 0.
plot(mod.ridge)

#######################################################
### Another look at the shrinkage of the coefficients
## In this case, we see the shrinkage as the (log of) lambda increases
## also include the intercept, which eventually approaches
## he mean of the response variable.

############################################
coefs.ridge <- coef(mod.ridge)
nms <- names(train.df)[1:8]
coef.df <- data.frame(coef=matrix(coefs.ridge,ncol=1),
                      varName=rep(c("Intercept",nms),numLambda),
                      loglambda=rep(rev(expVals),each=9))

ridge.gg <- ggplot(coef.df,aes(loglambda,coef,color=varName))+
    geom_line() +
    scale_x_continuous("Log(Lambda)")+
    scale_color_brewer(palette="Set1")
ridge.gg

ridge.gg+
    geom_hline(yintercept=mean(y.train),color="black")

#######################################################
## Here's another version, closer to the structure of the built-in
## glmnet plot command
############################################
c.df <- coef.df%>%
    group_by(loglambda)%>%
    summarize(bsum=sum((varName!="Intercept")*coef^2))

bsum0 <- with(c.df,max(bsum))
c.df <- mutate(c.df,bsum=bsum/bsum0)

coef.df%>%
    inner_join(c.df)%>%
    ggplot(aes(bsum,coef,color=varName))+
    geom_line() +
    scale_x_continuous("L1")+
    scale_color_brewer(palette="Set1")



#################################
## We need to find the optimal lambda. To do so, we cross-validate the
## mse across all the values of lambda. glmnet will do this
## automatically

## Use grid of lambda values with cv.glmnet. The plot shows
## cross-validated with error bars as a function of log(lambda) base=2.
cv.ridge <- cv.glmnet(x.train,y.train,alpha=0,lambda=lambda.grid,intercept=TRUE)
plot(cv.ridge)

##Note the vertical lines correspond to these
(lamb0 <- cv.ridge$lambda.min)
(lamb1 <- cv.ridge$lambda.1se)


###add info on lambda opt to our previous plot
ridge.gg+
    geom_vline(xintercept=log(lamb0,10),size=.5,linetype="dashed")+
    geom_vline(xintercept=log(lamb1,10),size=.5,linetype="dashed")
## Now we can see how the shrinkage worked here. lcp svi and gleason
## seem to be affected the most.

#######################################################
## Now compute mse for the optimal lambda value
#######################################################
mod.ridge <-
    glmnet(x.train,y.train,alpha=0,lambda=lamb0,intercept=TRUE)
coef.ridge <- as.matrix(coef(mod.ridge))


## Compare ridge coefficients to liner model coefficients
cbind(coef.lm,coef.ridge)

## Ridge prediction for optimal lambda
pred.ridge <- predict(mod.ridge,newx=x.test)
mse.ridge <- with(test.df,mean( (pred.ridge-lpsa)^2))

##comparison of mse...slight improvement
c(mse.lm, mse.ridge)
#######################################################


#######################################################
##2 Assignment
## Recreate plot(cv.ridge)
##  without using cv.glmnet. In other words, do your own
## 10-fold cross-validation.  To do so, loop over glmnet with a single
## lambda value. Each time, do a k-fold cross validation (on the
## training set only) and save
## both the mean and standard deviation for the mse (for the
## particular lambda value). From these you should be able to recreate
## a version of plot(cv.ridge) above. (see my version)
## Note: the standard error of the mean is sd/sqrt(n) where sd is the
## sample standard deviation and n=sample size. Hence if you have K=10
## mse values  mse.vals then the se=sd(mse.vals)/sqrt(10). These give
## the error bars.
#######################################################

for (i in 1:10) {
    lambdaIter <- lambda.grid[i]
    K <- 10 # number of folds
    N <- nrow(train.df) # number of rows in the training set
    folds <- sample(1:K, N, rep = T) # make the folds matrix
    for(k in 1:K) {
        # use the folds matrix on the x and y to make test/train for each
        xTrain.df <- x.train[folds != k,]
        xTest.df <- x.train[folds == k,]
        yTrain.df <- y.train[folds != k]
        yTest.df <- y.train[folds == k]
    
        # build the model
        mod.ridge <- glmnet(xTrain.df, yTrain.df, alpha = 0, lambda = i, intercept = TRUE)
        
        # Make a prediction
        prediction <- predict(mod.ridge, newx = xTest.df)
        # How did we do?
        mse <- mean((yTest.df - prediction)^2)
    }
}

#######################################################
##1  Repeat the analysis using lasso. Now alpha=1
mod.lasso <- glmnet(x.train,y.train,alpha=1,lambda=1,intercept=TRUE)
coef(mod.lasso)

## Use the grid of lambda values
mod.lasso <-
    glmnet(x.train,y.train,alpha=1,lambda=lambda.grid,intercept=TRUE)
## The built-in plot function. We can see how the coefficients drop out
plot(mod.lasso)

#######################################################
## Again, we can add more detail
#######################################################
coefs.lasso <- coef(mod.lasso)
coef.df <- data.frame(coef=matrix(coefs.lasso,ncol=1),
                      varName=rep(c("Intercept",nms),numLambda),
                      loglambda=rep(rev(expVals),each=9))
#######################################################
## We get a similar look at the lasso coefficeints
lasso.gg <- ggplot(coef.df,aes(loglambda,coef,color=varName))+geom_line()    +
    scale_color_brewer(palette="Set1")
lasso.gg

############################################
c.df <- coef.df%>%
    group_by(loglambda)%>%
    summarize(bsum=sum((varName!="Intercept")*coef^2))

bsum0 <- with(c.df,max(bsum))
head(c.df)
head(coef.df)

lasso.gg1 <- c.df %>%
    mutate(bsum=bsum/bsum0)%>%
    inner_join(coef.df)%>%
    filter(varName!="Intercept")%>%
    ggplot(aes(bsum,coef,color=varName))+
    geom_line(size=1.5) +
    scale_x_continuous("L1")+
    scale_color_brewer(palette="Spectral")+
    theme_bw()
lasso.gg1

############################################
## Now we can go full bore and cross-validate
cv.lasso <- cv.glmnet(x.train,y.train,alpha=1,lambda=lambda.grid,intercept=TRUE)
plot(cv.lasso)


#######################################################
## Extract the optimal lambda
(lamb0 <- cv.lasso$lambda.min)
(lamb1 <- cv.lasso$lambda.1se)
mod.lasso <-
    glmnet(x.train,y.train,alpha=1,lambda=lamb0,intercept=TRUE)

lasso.gg +
    geom_vline(xintercept=log(lamb0,10),size=.5,linetype="dashed")+
    geom_vline(xintercept=log(lamb1,10),size=.5,linetype="dashed")

## Also get the more conservative estimate
mod.lasso1 <-
    glmnet(x.train,y.train,alpha=1,lambda=lamb1,intercept=TRUE)

#######################################################
## Coefficients
coef.lasso <- coef(mod.lasso)
coef.lasso1 <- coef(mod.lasso1)
cbind(coef.lm,coef.ridge,coef.lasso1)

#######################################################
## And finally, the mse
pred.lasso <- predict(mod.lasso,newx=x.test)
pred.lasso1 <- predict(mod.lasso1,newx=x.test)
mse.lasso <- with(test.df,mean( (pred.lasso-y.test)^2))
mse.lasso1 <- with(test.df,mean( (pred.lasso1-y.test)^2))


#######################################################
## Comparison of errors
c(mse.lm,mse.ridge,mse.lasso,mse.lasso1)

#######################################################


