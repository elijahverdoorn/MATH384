#######################################################
## K-fold Cross Validation
##  with KNN models
library(ggplot2)
library(dplyr)
##the R knn, only for classification
library(class)
##R knn.reg, for regression
library(FNN)


airfoil.df0<-read.csv("airfoil_noise.csv",sep='\t')
summary(airfoil.df0)

airfoil.df<-scale(airfoil.df0)
summary(airfoil.df)

#######################################################
### K Fold Cross-validation
## Idea: Split data in K classes, 1,2,...K
#######################################################
K<-10
N<-nrow(airfoil.df)
folds<-sample(1:K,N,rep=T)
folds

#######################################################
## extract train and test data
##(K-1)/K of the data
train.df<-airfoil.df[folds!=1,]
## 1/K of the data
test.df<-airfoil.df[folds==1,]

#####knn likes matrices
train.mat<-matrix(train.df[,1:5],ncol=5)
resp<-matrix(train.df[,6],ncol=1)
test.mat<-matrix(test.df[,1:5],ncol=5)

train.mat <- data.matrix(train.df[,1:5])
resp<- data.matrix(train.df[,6])
test.mat <- data.matrix(test.df[,1:5])

knn.fit<-knn.reg(train.mat,test.mat,resp,k=3)


knn.fit<-knn.reg(train.mat,test.mat,resp,k=2)
##mean squared error on test data
mean((knn.fit$pred-test.df[,6])^2)
plot(knn.fit$pred,test.df[,6])

#######################################################
## fix the knn value of k. Repeat this over all the "folds"
#######################################################
Kval.knn<-3
K <- 10

cv.mse<-rep(0,K)
folds<-sample(1:K,N,rep=T)
folds

#######################################################
for(k in 1:K){
  train.df<-airfoil.df[folds!=k,]
  test.df<-airfoil.df[folds==k,]
  #####
  train.mat<-matrix(train.df[,1:5],ncol=5)
  resp<-matrix(train.df[,6],ncol=1)
  test.mat<-matrix(test.df[,1:5],ncol=5)
  knn.fit<-knn.reg(train.mat,test.mat,resp,k=Kval.knn)
  cv.mse[k]<-mean((knn.fit$pred-test.df[,6])^2)
}

cv.mse
##the  mean value of all the cross-validated terms
mean(cv.mse)
dim(train.mat)
dim(test.mat)
dim(resp)
knn.fit<-knn.reg(train.mat,test.mat,c(resp),k=2)


#######################################################
## Need a handy-dandy function to repeat this for different k values.

doK_Fold<-function(kval.knn,df,K=10){
    mse<-rep(0,K)
    N <- nrow(df)
    folds<-sample(1:K,N,rep=T)
    for(k in 1:K){
        train.df <- df[folds!=k,]
        test.df <- df[folds==k,]
        ## ###
        train.mat<-matrix(train.df[,1:5],ncol=5)
        resp<-c(matrix(train.df[,6],ncol=1))
        test.mat<-matrix(test.df[,1:5],ncol=5)
        ##For some reason, knn.reg doesn't like k=2
        ans<-knn.reg(train.mat,test.mat,resp,k=kval.knn)
        mse[k]<-mean((ans$pred-test.df[,6])^2)
    }
    mean(mse)
}
ans
doK_Fold(1,airfoil.df)
doK_Fold(2,airfoil.df)
doK_Fold(3,airfoil.df)
doK_Fold(4,airfoil.df)

#######################################################
## plot the result to see the best value of k
#######################################################
K<-10
cv.mse<-rep(0,K)
for(t in 1:K){
     cv.mse[t]<-doK_Fold(t,airfoil.df)
}
#cv.mse[2] <- (cv.mse[1]+cv.mse[3])/2
#######################################################
## A plot of the cross validated mse as a function of k
## Looks like a k=2 is the optimal value
#######################################################
ggplot(data.frame(k=1:K,mse=cv.mse),aes(k,mse))+geom_point()+geom_line()+
    scale_x_continuous(breaks=1:K)+
    ggtitle("K-Fold Cross-validated MSE for Airfoil Data")


#######################################################
