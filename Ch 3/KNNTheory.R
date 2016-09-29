library(ggplot2)
library(dplyr)
library(tidyr)
library(FNN)
######################################
## 
## fitting theoretical models with KNN vs Linear model
####################################
f1<-function(x) -1+2*x
f2<-function(x) sin(.7*x)
f3<-function(x){
  2*x*sin(3*x)+2+x
}

f<-f3
n<-100
x<-seq(-2,2,len=100)
plot(x,f(x),type="l")
sd<-.5
y<-f(x)+rnorm(n,0,sd)
plot(x,y)

#######################
##KNN vs a Linear Model
## train/test mse comparision
######################
N<-30

x.train<-runif(N,-2,2)
y.train<-f(x.train)+rnorm(N,0,sd)

x.test<-runif(N,-2,2)
y.test<-f(x.test)+rnorm(N,0,sd)

train.df<-data.frame(x=x.train,y=y.train)
test.df<-data.frame(x=x.test,y=y.test)

#############################
## Linear Model
############################
mod.lm<-lm(y~x,data=train.df)
summary(mod.lm)


test.df$pred.lm<-predict(mod.lm,newdata=test.df)
mse.lm<-with(test.df,mean( (y-pred.lm)^2))
mse.lm


###########################
## KNN
###########################
kval<-5
dim(x.train)<-dim(y.train)<-dim(x.test)<-c(N,1)

mod.knn<-knn.reg(x.train,x.test,y.train,k=kval)
test.df$pred.knn<-mod.knn$pred
mse.knn <- with(test.df,mean( (y-pred.knn)^2))
mse.knn
c(mse.lm,mse.knn)

##############################################
test.df %>% 
  ggplot(aes(x,y))+
  geom_point()+
  geom_line(aes(x,pred.lm),color="blue")+
  geom_step(aes(x,pred.knn),color="red")+
  geom_point(data=train.df,aes(x,y),
             color="brown",size=2,shape=5,alpha=0.5)+
  ggtitle(sprintf("Comparision of Linear Model and KNN (k=%s)
                  Training data shown",kval))

##################################
## Construct version of Figure 3.18
## Do the same for Figures 3.19 (4 total)
## There are multiple values of k, each point represents
## an average MSE
#####################################