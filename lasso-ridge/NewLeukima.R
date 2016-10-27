#######################################################
### Micro Array data and penalized regression
## Check Paper: ElasticNet_Talk.pdf
## Also...very interesting webinar embedded
## http://blog.revolutionanalytics.com/2013/05/hastie-glmnet.html
#######################################################
library(ISLR)
library(glmnet)
library(ggplot2)
library(dplyr)


#######################################################
## Load the data and inspect dimensions
load("Leukemia.RData")
dim(Leukemia$x)
table(Leukemia$y)
#######################################################
## Look at the data nxp x<<p
#######################################################

nn <- nrow(Leukemia$x)
nn
train.vals <- 1:38
train.vals <- sample(1:nn,38,rep=F)
x.train <- Leukemia$x[train.vals,]
y.train <- Leukemia$y[train.vals]
table(y.train)
dim(x.train)
summary(x.train[2,])

##For later
x.test <- Leukemia$x[-train.vals,]
y.test <- Leukemia$y[-train.vals]

dim(x.test)
table(y.test)
length(y.test)

#######################################################
## Logistic regression????
## Too many predictor variables 3700
## Subset selection is out of the question
#######################################################
train.df <- data.frame(x.train,y=y.train)
fit.log <- glm(y~.,family="binomial",data=train.df)

pred.log <- predict(fit.log,newdata=data.frame(x.test),type="response")
hist(pred.log)
log.class <- 1.0*(pred.log>0.5)
log.class
table(y.test,log.class)

#######################################################
## Ridge
#######################################################
numLambda <- 100
ee <- seq(-6,0,length=numLambda)
lambda.grid <- 10^ee

#######################################################
## Use glmnet with cross-validation
cv.ridge <- cv.glmnet(x.train,y.train,alpha=0,family="binomial",lambda=lambda.grid)
plot(cv.ridge)

lamb.cv.r <- cv.ridge$lambda.min

fit.rid.all <- glmnet(x.train,y.train,family="binomial",alpha=0,lambda=lambda.grid)
plot(fit.rid.all)

fit.rid <-
    glmnet(x.train,y.train,family="binomial",alpha=0,lambda=lamb.cv.r)

pred.rid <- predict(fit.rid,newx=x.test,type="response")
class.rid <- 1.0*(pred.rid>0.5)
table(y.test,class.rid)


#######################################################
## Lasso
#######################################################
cv.lasso <- cv.glmnet(x.train,y.train,alpha=1,family="binomial",lambda=lambda.grid)
plot(cv.lasso)
lamb.cv.l <- cv.lasso$lambda.min

fit.lasso.all <-
    glmnet(x.train,y.train,family="binomial",alpha=1,lambda=lambda.grid)
plot(fit.lasso.all)

fit.lasso <-
    glmnet(x.train,y.train,family="binomial",alpha=1,lambda=lamb.cv.l)

pred.lass <- predict(fit.lasso,newx=x.test,type="response")
class.lass <- 1.0*(pred.lass>0.5)

table(y.test,class.lass)

coef(fit.lasso)

#######################################################

