#######################################################
##
#######################################################
library(ggplot2)
library(dplyr)
library(tidyr)


#######################################################
## Start by building some data
#######################################################
b0 <- 1
b1 <- 2
b2 <- -2
b3 <- 3.5
############################################
## a "true" underlying function. In reality, we never know what this
## is or if it even exists
############################################
true.fnc <- function(x){b0+b1*x+b2*sin(b3*pi*x)}

############################################
## put some data together
############################################
sd <- 1.25
N <- 1000
x <- seq(0,1,length=N)
y <- true.fnc(x)+rnorm(N,0,sd)
##this is  Y=F(X)+eps
true.df <- data.frame(x,y)


############################################
## Plots


ggplot(true.df,aes(x,y))+
  geom_point(color="blue",size=1.25) +
  ggtitle("f(X) + eps")

gplot(true.df,aes(x,y))+
  geom_point(aes(x,y.model),color="black",size=1.25)+
  geom_point(color="blue",size=1) +
  ggtitle("f(x) and f(X) + eps")


#######################################################
## Training
## all we get is sample of size n
#######################################################
n <- 40
train <- sample(1:N,n,rep=F)
train.df <- true.df[train,]

## create test set of the same size
test <- sample(1:N,n,rep=F)
test.df <- true.df[test,]


############################################
## look at these
############################################
ggplot(train.df,aes(x,y))+
    geom_point(color="black",size=3)+
    ggtitle("Training set")

ggplot(test.df,aes(x,y))+
    geom_point(color="blue",size=3)+
    ggtitle("Testing set")


############################################
##Use a Linear Model to fit the data
## we will look more closely at linear regression soon
############################################
mod.lm1 <- lm(y~x,data=train.df)

#######################################################
##predict on training set
#######################################################
pred.test <- predict(mod.lm1,data=train.df)
train.df <- mutate(train.df,pred1 = pred.test)

#######################################################
## Here is fhat, our approximation to the "true" function f
############################################
fhat <- function(x0){
    predict(mod.lm,data.frame(x=x0))
}


ggplot(train.df,aes(x,y))+
  geom_point(color="black",size=3)+#geom_line(color="black")+
  geom_point(aes(x,pred1),color="red",size=3)+geom_line(aes(x,pred1),color="red")+
  ggtitle("Training set\nBlack=data Red=Prediction")



#######################################################
##predict on testing
############################################
pred.test <- predict.lm(mod.lm1,newdata=test.df)
##equivalently...
##pred.test <- fhat(test.df$x)
test.df <- mutate(test.df,pred1 = pred.test)



ggplot(test.df,aes(x,y))+
  geom_point(color="black",size=3)+#geom_line(color="black")+
  geom_point(aes(x,pred1),color="red",size=3)+geom_line(aes(x,pred1),color="red")+
  ggtitle("Testing set\nBlack=data Red=Prediction")


############################################
## Compare MSEs
############################################
with(train.df,mean((y-pred1)^2))
with(test.df,mean((y-pred1)^2))
##Not bad.

#######################################################
## Flexibility
## Repeat with higher degree fit
##orthogonal poly's
#######################################################
mod.lm2 <- lm(y~poly(x,2),data=train.df)
mod.lm3 <- lm(y~poly(x,3),data=train.df)
mod.lm5 <- lm(y~poly(x,5),data=train.df)
mod.lm10 <- lm(y~poly(x,10),data=train.df)

pred.train2 <- predict(mod.lm2)
pred.train3 <- predict(mod.lm3)
pred.train5 <- predict(mod.lm5)
pred.train10 <- predict(mod.lm10)


train.df$pred2  <- pred.train2
train.df$pred3  <- pred.train3
train.df$pred5  <- pred.train5
train.df$pred10 <- pred.train10

#######################################################
## reshape the data a bit
#######################################################
t.df <- train.df%>%
    gather(type,val,y:pred10)

#######################################################
## The training data
#######################################################
gg1 <-
    ggplot(filter(t.df,type=="y"),aes(x,val))+geom_point(size=3)+geom_line()
gg1

#######################################################
## Add on the different models
#######################################################
gg1 +
    geom_line(data=filter(t.df,type!="y"),aes(x,val,color=type),size=1.5)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Models with greater flexibility")


#######################################################
## Cosemetic change...don't like the labels
#######################################################
type.df <- data.frame(type=paste0("pred",1:10),Model=paste0("deg=",1:10))
t2.df <- t.df%>%
    inner_join(type.df)%>%
    mutate(Model=factor(Model,paste0("deg=",1:10)))

gg1 +
    geom_line(data=filter(t2.df,type!="y"),aes(x,val,color=Model),size=1.5)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Models with greater flexibility")

#######################################################
## For each of these models, we can compare the train and test mse.
#######################################################
test.df$pred1 <- predict(mod.lm1,newdata=test.df)
test.df$pred2 <- predict(mod.lm2,newdata=test.df)
test.df$pred3 <- predict(mod.lm3,newdata=test.df)
test.df$pred5 <- predict(mod.lm5,newdata=test.df)
test.df$pred10 <- predict(mod.lm10,newdata=test.df)

c(with(train.df,mean((y-pred1)^2)),with(test.df,mean((y-pred1)^2)))
c(with(train.df,mean((y-pred2)^2)),with(test.df,mean((y-pred2)^2)))
c(with(train.df,mean((y-pred3)^2)),with(test.df,mean((y-pred3)^2)))
c(with(train.df,mean((y-pred5)^2)),with(test.df,mean((y-pred5)^2)))
c(with(train.df,mean((y-pred10)^2)),with(test.df,mean((y-pred10)^2)))





############################################
## Plan: Recreate Figure 2.9
## Increase flexibility, ie degree of the fit
## Track train/test mse as function of flexibilty
## plot both values
#############################################
## create train set of the same size
train <- sample(1:N,n,rep=F)
train.df <- true.df[train,]
## create test set of the same size
test <- sample(1:N,n,rep=F)
test.df <- true.df[test,]

maxDeg <- 10
mseVals <- matrix(nrow=maxDeg,ncol=3)
deg <- 6
for(deg in 1:maxDeg){
    ## Train: Build model on training data
    mod <- lm(y~poly(x,deg),data=train.df)
    ##training predictions
    preds <- predict(mod,data=train.df)
    train.df <- mutate(train.df,pred = preds)
    ## Testing predictions
    preds <- predict.lm(mod,newdata=test.df)
    test.df <- mutate(test.df,pred = preds)
    ##train and test mse
    mseTrain <- with(train.df,mean((y-pred)^2))
    mseTest <- with(test.df,mean((y-pred)^2))
    print(mseTrain)
    mseVals[deg,] <- c(deg,mseTrain,mseTest)
}
############################################
mseVals.df <- data.frame(mseVals)
names(mseVals.df) <- c("deg","mseTrain","mseTest")
mseVals.df <- mseVals.df%>%
    gather(type,val,-deg)
############################################
ggplot(mseVals.df,aes(deg,val,color=type)) +
    geom_point()+geom_line()+
    scale_color_manual(values=c(mseTrain="red",mseTest="blue"))+
    geom_hline(yintercept=sd,linetype="dashed")+
    scale_x_continuous(breaks=1:10)+
    ggtitle("MSE and Flexibility: Training vs Testing")




#######################################################
## Almost want we want.  But the training data is the same every time
## Add randomness to training  and test data
##
#######################################################
M <- 500
mseTrain <- matrix(nrow=M,ncol=1)
mseTest <- matrix(nrow=M,ncol=1)
for(deg in 1:maxDeg){
    print(deg)
    ## Train: Build model on training data
    for(j in 1:M){
        train <- sample(1:N,n,rep=F)
        train.df <- true.df[train,]
        ## create test set of the same size
        test <- sample(1:N,n,rep=F)
        test.df <- true.df[test,]
        ##
        mod.lm <- lm(y~poly(x,deg),data=train.df)
        ##training predictions
        pred.test <- predict(mod.lm,data=train.df)
        train.df <- mutate(train.df,pred = pred.test)
        ## Testing predictions
        pred.test <- predict.lm(mod.lm,newdata=test.df)
        test.df <- mutate(test.df,pred = pred.test)
        ##train and test mse
        mseTrain[j] <- with(train.df,mean((y-pred)^2))
        mseTest[j] <- with(test.df,mean((y-pred)^2))
    }
    mseVals[deg,] <- c(deg,mean(mseTrain),mean(mseTest))
}
############################################
mseVals.df <- data.frame(mseVals)
names(mseVals.df) <- c("deg","mseTrain","mseTest")
mseVals.df <- mseVals.df%>%
    gather(type,val,-deg)

############################################
ggplot(mseVals.df,aes(deg,val,color=type)) +
    geom_point()+geom_line()+
    scale_color_manual(values=c(mseTrain="red",mseTest="blue"))+
    geom_hline(yintercept=sd,linetype="dashed")+
    scale_x_continuous(breaks=1:10)+
    scale_y_continuous(limits=c(0,10))+
    ggtitle("MSE and Flexibility: Training vs Testing")



#######################################################
## From this it appears that a deg=6 polynomial is the best model to
## use on this sort of data
#######################################################
