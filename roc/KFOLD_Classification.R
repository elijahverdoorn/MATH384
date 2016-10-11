#######################################################
## K-fold Cross Validation
##  with KNN models
library(ggplot2)
library(dplyr)
##the R knn, only for classification
library(class)
##R knn.reg, for regression
library(FNN)


b0 <- 1
b1 <- -1
b2 <- -1
b3 <- -1
b4 <- 4


N <- 1000
x1 <- runif(N,-1,2)
x2 <- runif(N,-1,2)
y <- b0+b1*x1+b2*x2+b3*x1^2+b4*x2^2+rnorm(N,0,2)
#y <- b0+b1*x1+b2*x2
p <- exp(y)/(1+exp(y))

z <- rbinom(N,1,prob=p)
dat.df <- data.frame(x1,x2,z=factor(z),p,z.bayes=p>0.5)

##Bayes Error Rate
with(dat.df,mean((z==1) != z.bayes))

## with(dat.df,mean( z.bayes))
## with(dat.df,mean( z==1))
##

ggplot(dat.df,aes(x1,x2,color=z))+geom_point()+
    scale_color_manual(values=c("red","blue"))

ggplot(dat.df,aes(x1,x2,color=p))+geom_point(size=3)+
    scale_color_gradient2(low="red",high="blue",midpoint=0.5,mid="white")

mod1 <- glm(z~x1+x2,data=dat.df,family="binomial")
summary(mod1)

pred <- predict(mod1,type="response")

##Bayes Error Rate
(err.bayes <- with(dat.df,mean((z==1) != z.bayes)))

##model error rate
with(dat.df,mean((pred>0.5)!=(z==1)))


#######################################################
## estimate Error Rate Using K-Fold Cross-validation
#######################################################
names(dat.df)
dat.df <- select(dat.df,-p,-z.bayes)

model.strs <- c(
    "z~x1",
    "z~x2",
    "z~x1+x2",
    "z~x1+x2+I(x1^2)",
    "z~x1+x2+I(x2^2)",
    "z~x1+x2+I(x1^2)+I(x2^2)",
    "z~x1+x2+I(x1^2)+I(x2^2)+x1*x2")

K<-10
cv.errRate <- matrix(nrow=K)
folds <- sample(1:K,N,rep=T)
modVal <- 1
for(k in 1:K){
    test <- folds==k
    test.df <- dat.df[test,]
    train.df <- dat.df[!test,]
    mod1 <- glm(formula(model.strs[modVal]),data=train.df,family="binomial")
    pred <- predict(mod1,newdata=test.df,type="response")
    test.df <- mutate(test.df,z.pred=ifelse(pred>0.5,1,0))
    cv.errRate[k,] <- with(test.df,mean(z !=z.pred))
}
mean(cv.errRate)

cv.errRate
summary(mod1)


