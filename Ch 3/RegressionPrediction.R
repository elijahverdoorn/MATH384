library(ggplot2)
library(dplyr)
library(tidyr)
library(ISLR)
help(Carseats)
##
## ggplot(Carseats,aes(Price,Sales))+
##     geom_point()
##
##
## ggplot(Carseats,aes(CompPrice,Sales))+
##     geom_point()
##
##
##
## ggplot(Carseats,aes(Income,Sales))+
##     geom_point()
##
## ggplot(Carseats,aes(Advertising,Sales))+
##     geom_point()
##
## ggplot(Carseats,aes(Education,Sales))+
##     geom_point()
##
## ggplot(Carseats,aes(Population,Sales))+
##     geom_point()
##
# head(Carseats)

#######################################################
## Add a revenue field
#######################################################
Carseats <- Carseats%>%
    mutate(Revenue=Price*Sales)

Carseats2 <- Carseats%>%
    select(c(Sales:Price,Revenue))%>%
    gather(indicator,value,CompPrice:Price)

head(Carseats2)

ggplot(Carseats2,aes(value,Revenue,color=indicator))+
    facet_wrap(~indicator,scales="free_x",nrow=2)+
    scale_x_continuous("")+
    geom_point()


#######################################################
## Regression
#################################
## Models with one predictor
#################################
mod1.1 <- lm(Revenue~CompPrice,data=Carseats)
summary(mod1.1)

mod1.2 <- lm(Revenue~Income,data=Carseats)
summary(mod1.2)

mod1.3 <- lm(Revenue~Advertising,data=Carseats)
summary(mod1.3)

mod1.4 <- lm(Revenue~Population,data=Carseats)
summary(mod1.4)

mod1.5 <- lm(Revenue~Price,data=Carseats)
summary(mod1.5)

############################################
## include more predictors
############################################
mod3<- lm(Revenue~CompPrice+Price+Advertising,data=Carseats)
summary(mod3)


mod4 <- lm(Revenue~CompPrice+Price+Advertising+Income,data=Carseats)
summary(mod4)

mod5 <- lm(Revenue~CompPrice+Price+Advertising+Population+Income,data=Carseats)
summary(mod5)


############################################
## Inference versus prediction
############################################
nn <- nrow(Carseats)
train <- sample(1:nn,nn/2,rep=T)
test <- setdiff(1:nn,train)

train.df <- Carseats[train,]
test.df <- Carseats[test,]
head(train.df)

#######################################################
mod1 <- lm(Revenue~CompPrice,data=train.df)
summary(mod1)
train.pred <- predict(mod1)
train.df <- mutate(train.df,pred1 = train.pred)

test.pred <- predict(mod1,newdata=test.df)
test.df <- mutate(test.df,pred1 = test.pred)

mse.train1 <- with(train.df,mean((Revenue-pred1)^2))
mse.test1 <- with(test.df,mean((Revenue-pred1)^2))
c(mse.train1,mse.test1)


#######################################################
## Four predictors
mod4 <- lm(Revenue~CompPrice+Price+Advertising+Income,data=train.df)
summary(mod4)

train.pred <- predict(mod4)
str(train.pred)
nrow(train.df)

train.df <- mutate(train.df,pred4 = train.pred)

test.pred <- predict(mod4,newdata=test.df)
test.df <- mutate(test.df,pred4 = test.pred)

mse.train4 <- with(train.df,mean((Revenue-pred4)^2))
mse.test4 <- with(test.df,mean((Revenue-pred4)^2))
c(mse.train4,mse.test4)


############################################
mod5 <-
    lm(Revenue~CompPrice+Price+Advertising+Population+Income,data=train.df)

train.pred <- predict(mod5)
train.df <- mutate(train.df,pred5 = train.pred)

test.pred <- predict(mod5,newdata=test.df)
test.df <- mutate(test.df,pred5 = test.pred)

mse.train5 <- with(train.df,mean((Revenue-pred5)^2))
mse.test5 <- with(test.df,mean((Revenue-pred5)^2))

c(mse.train5,mse.test5)
c(mse.train4,mse.test4)

#######################################################
## Question: How well does one predictor vs five predictors work?
## when we are considering PREDICTION???
##
## Repeat this M times
## Construct test and train each time
## track mse.test1 and mse.test5
## How do the average performances compare
## Do this for Carseats
## If prediction is your concern, what do you observe about the effect of adding 
## more predictors into the data? Do more (relevant) predictors seem
## to help, or is there a limit and eventual downside to adding more?
#######################################################

iterations <- 10
mse <- matrix(nrow = iterations, ncol = 4)
for (i in 1:iterations) {
  # Do it for just 2 predictors
  mod1 <- lm(Revenue~CompPrice, data = train.df)
  train.pred <- predict(mod1)
  train.df <- mutate(train.df, pred1 = train.pred)
  
  test.pred <- predict(mod1, newdata = test.df)
  test.df <- mutate(test.df, pred1 = test.pred)
  
  mse[i,1] <- with(train.df, mean((Revenue - pred1)^2))
  mse[i,2] <- with(test.df, mean((Revenue - pred1)^2))
  
  # Do it for 5 predictors
  mod5 <- lm(Revenue~CompPrice + Price + Advertising + Population + Income, data = train.df)
  train.pred <- predict(mod5)
  train.df <- mutate(train.df, pred5 = train.pred)
  
  test.pred <- predict(mod5, newdata = test.df)
  test.df <- mutate(test.df, pred5 = test.pred)
  
  mse[i,3] <- with(train.df, mean((Revenue - pred5)^2))
  mse[i,4] <- with(test.df, mean((Revenue - pred5)^2))
  
}




############################################
## Adding more features....help or hurt?
## additional predictors tends to help out
## what about additional features??
############################################
## Best one preditor model.
#mod1.1 <- lm(Revenue~CompPrice,data=Carseats)
#summary(mod1.1)

#Carseats <- mutate(Carseats, CompPrice2=CompPrice*CompPrice)
#mod1.2 <- lm(Revenue~CompPrice+CompPrice2,data=Carseats)
#summary(mod1.2)


#Carseats <- mutate(Carseats, CompPrice3=CompPrice2*CompPrice)
#mod1.3 <- lm(Revenue~CompPrice+CompPrice2+CompPrice3,data=Carseats)
#summary(mod1.3)

#######################################################
## what about prediction??
#######################################################
