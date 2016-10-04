library(ggplot2)
library(dplyr)
library(tidyr)
library(FNN)
library(ISLR)

Carseats <- Carseats%>%
    mutate(Revenue=Price*Sales)

mse <- matrix(nrow = iterations, ncol = 4)
iterations <- 10

nn <- nrow(Carseats)
train <- sample(1:nn,nn/2,rep=T)
test <- setdiff(1:nn,train)

train.df <- Carseats[train,]
test.df <- Carseats[test,]


for(i in 1:iterations) {
    # the linear model, created from a string that gets converted to a function
    maxDeg <- 1
    formStr <- "Strength~Cement" #starter string
    for(d in 1:maxDeg) {
        formStr <- sprintf("%s+I(Cement^%s)", formStr, d)
    }
    linearModel <- lm(formula(formStr), data = Carseats)

    concrete.df$pred.lm <- predict(linearModel) # plot the linear model, just a check
    ggplot(Carseats2,aes(value,Revenue,color=indicator))+
        facet_wrap(~indicator,scales="free_x",nrow=2)+
        scale_x_continuous("")+
        geom_point()
    
    train.pred <- predict(linearModel)
    train.df <- mutate(train.df, pred1 = train.pred)
    
    test.pred <- predict(linearModel, newdata = test.df)
    test.df <- mutate(test.df, pred1 = test.pred)
    
    mse[i,1] <- with(train.df, mean((Revenue - pred1)^2))
    mse[i,2] <- with(test.df, mean((Revenue - pred1)^2))
    
}
