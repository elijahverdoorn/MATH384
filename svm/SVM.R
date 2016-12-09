library(e1071)
library(MASS)
library(ROCR)
library(tidyverse)
library(dplyr)

#######################################################
##build a grid to show the region classifications
## grid size
#######################################################
build.grid <- function(df,GS){
    rng.x <- with(df,range(x))
    rng.y <- with(df,range(y))
    xvals=seq(rng.x[1],rng.x[2],length=GS)
    yvals=seq(rng.y[1],rng.y[2],length=GS)
    grid.xy=expand.grid(xvals,yvals)
    grid.df <- data.frame(x=grid.xy[,1],y=grid.xy[,2],class=0)
}
#######################################################


#######################################################
##simple data
#######################################################
mu0 <- c(1,1)
mu1 <- c(-1,-1)
N <- 100
std.dev<- 4*sqrt(1/5)
vals0 <- mvrnorm(N,mu0,diag(2)*std.dev)
vals1 <- mvrnorm(N,mu1,diag(2)*std.dev)
vals <- rbind(vals0,vals1)
dat.df <- data.frame(label=1:(2*N),x=vals[,1],y=vals[,2],class=factor(rep(c(-1,1),each=N)))

## tune.out=tune(svm,y~.,data=dat.df,kernel="linear",
##               ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
##
##plot the data
gg.train <- ggplot(dat.df,aes(x,y,color=class))+
    geom_point(size=4)+
    scale_color_brewer(palette="Set1")+
  ggtitle("Simple Data")
gg.train



#######################################################
### create the SVM plot
## build a grid
grid.df <- build.grid(dat.df,70)

##cost is the inverse of budget. It's gamma is our second
##minimization, that is, the penality for a total cost of violating
##the margin.


##large C: flexible, small margin. High variance, small bias
C <- 10000
svmfitBigC <-
  svm(class~x+y,data=dat.df,kernel="linear",cost=C,scale=F)

##
svmfit <- svmfitBigC
grid.class <- predict(svmfit,newdata=grid.df)
grid.df <- within(grid.df,class <- factor(grid.class))
dat.df <- within(dat.df,Support <- label %in% svmfit$index)
dat.df <- within(dat.df,Size <- ifelse(Support,5,1))

##
gg.big.svm <-ggplot()+
  geom_point(data=grid.df,aes(x,y,color=factor(class)),shape=15,alpha=.10,size=3)+
  geom_point(data=dat.df,aes(x,y,color=factor(class),shape=Support,size=Size))+
  scale_color_manual(values=c("blue","red"))+
  scale_size(range=c(2,4))+  guides(size=FALSE,shape=F,color=F)+
  ggtitle(sprintf("SVM for Simulated Data: Cost = %s",C))
gg.big.svm


##small C, not flexible, large marign. Low Variance, High Bias
C <- .001000
svmfitSmallC <-
  svm(class~x+y,data=dat.df,kernel="linear",cost=C,scale=F)

svmfit <- svmfitSmallC
##
grid.class <- predict(svmfit,newdata=grid.df)
grid.df <- within(grid.df,class <- factor(grid.class))
dat.df <- within(dat.df,Support <- label %in% svmfit$index)
dat.df <- within(dat.df,Size <- ifelse(Support,5,1))
##
gg.small.svm <- ggplot()+
  geom_point(data=grid.df,aes(x,y,color=class),shape=15,alpha=.10,size=3)+
  geom_point(data=dat.df,aes(x,y,color=factor(class),shape=Support,size=Size))+
  scale_color_manual(values=c("blue","red"))+
  scale_size(range=c(2,4))+ guides(size=FALSE,shape=F,color=F)+
    ggtitle(sprintf("SVM for Simulated Data: Cost = %s",C))
gg.small.svm

#######################################################
## Performance
## Estimate error with CV
#######################################################
cv.svm <- function(C,K,df){
    N <- nrow(df)
    folds <- sample(1:K,N,rep=T)
    errs <- rep(0,K)
    for(k in 1:K){
        train <- !folds==k
        test <-  folds==k
        svmfit <-
            svm(class~x+y,data=df[train,],kernel="linear",cost=C,scale=T)
        pred.svm <- predict(svmfit,newdata=df[test,])
    pred.svm
        errs[k] <- with(dat.df[test,],mean(class!=pred.svm))
    }
    mean(errs)
}

cv.svm(10,5,dat.df)
cv.svm(100,5,dat.df)
cv.svm(1000,5,dat.df)

cost.vals <- 10^seq(-3,4,length=30)
cv.errs <- lapply(cost.vals,function(x) cv.svm(x,5,dat.df))
plot(log(cost.vals),unlist(cv.errs),type="l")

idx <- which.min(cv.errs)
cost.vals[idx]

#######################################################
## Tuning...
## svm will cross-validate to find optimnal choice of cost
#######################################################
svm.tuned <- tune.svm(class~x+y,data=dat.df,kernel="linear",scale=T,
                      ##include a s
                     cost=cost.vals)
##extract the optimal cost
(cost.opt <- svm.tuned$best.model$cost)

##extract the best model
svmfit.best <- svm.tuned$best.model

##try this on our cross-validation function
cv.svm(cost.opt,10,dat.df)


#######################################################
## ROC Curve ..... trick is to return the fitted values
#######################################################
C <- .0100000
##note the extra argument
svmfit <-
    svm(class~x+y,data=dat.df,kernel="linear",cost=C,scale=F,decision.values=T)
##get the fitted values
fitted <- svmfit$decision.values

##ROC curve
M <- 100
rng <- range(fitted)
fp <- rep(0,M)
tp <- rep(0,M)

t.vals <- seq(rng[1],rng[2],length=M)
for(i in 1:M){
  pred <- fitted>=t.vals[i]
  tp[i]<-with(dat.df,sum(class== -1 & pred)/sum(class== -1))
  fp[i]<-with(dat.df,sum(class== +1 & pred)/sum(class== +1))
}

df.roc <- data.frame(tp,fp)
##remove redundant values
df.roc <- ddply(df.roc,.(fp),summarize,tp=min(tp))

df.roc <- df.roc%>%
    group_by(fp)%>%
    summarize(tp=min(tp))

ggplot(df.roc,aes(fp,tp))+geom_point(color="red")+geom_step(color="red")+
  ggtitle(sprintf("ROC Cost=%s",C))

############################################
## Cross Validate to get a better ROC curve
############################################
roc.svm.cv <- function(df,C){
    N <- nrow(df)
    folds <- sample(1:K,N,rep=T)
    M <- 100
    fp <- matrix(nrow=K,ncol=M)
    tp <- matrix(nrow=K,ncol=M)
    for(k in 1:K){
        train <- !folds==k
        test <-  folds==k
        svmfit <-
            svm(class~x+y,data=df[train,],kernel="linear",cost=C,scale=T)
        pred.svm <-
            predict(svmfit,newdata=df[test,],decision.values=T)
        ##this was annoying...here's how to ge the fitted values from predict
        fitted <-attr(pred.svm,"decision.values")
        rng <- range(fitted)
        t.vals <- seq(rng[1],rng[2],length=M)
        for(i in 1:M){
            pred <- fitted>t.vals[i]
            tp[k,i]<-with(df[test,],sum(class== -1 & pred)/sum(class== -1))
            fp[k,i]<-with(df[test,],sum(class== +1 & pred)/sum(class== +1))
        }
    }
    ##columns to average over all the CV folds
    tp<- colMeans(tp)
    fp <- colMeans(fp)
    ##return
    df.ret <- data.frame(fp,tp)
    ddply(df.ret,.(fp),summarize,tp=min(tp))
}

df.roc<- roc.svm.cv(dat.df,1000)

ggplot(df.roc,aes(fp,tp))+geom_point(color="red")+geom_step(color="red")+
  ggtitle(sprintf("ROC (Cross Validated) Cost=%s",C))

##compute area under the curve as a measuro of ROC
df.roc <- within(df.roc,fp2 <- c(fp[-1],fp[nrow(df.roc)]))
df.roc <- within(df.roc,del.fp <- fp2-fp)
(auc <- with(df.roc,sum(tp*del.fp)))


#######################################################
## More complicated example
##
## generate values according to ESL example page 21
## n means at (1,0) and 10 at (-1,0) with sigma=Id
#######################################################
##fixed std.dev for samples
std.dev<- sqrt(1/5)
#######################################################
## generate means for classes 0 and 1
#######################################################
n <- 10
mu0 <- mvrnorm(n,c(0,1),diag(1,2))
mu1 <- mvrnorm(n,c(1,0),diag(1,2))

#######################################################
## Create a training data frame for classificaton
## sample the means  N times each to build the training data set for
## each of the classes
#######################################################
N <- 200
rnds <- sample(1:n,N,rep=T)

vals0<- t(apply(mu0[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*std.dev)))
vals1 <- t(apply(mu1[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*std.dev)))
vals <- rbind(vals1,vals0)

##put in a data frame
dat.df <-
    data.frame(label=factor(1:(2*N)),x=vals,class=factor(rep(c(1,-1),each=N)))
names(dat.df)[2:3] <- c("x","y")


##plot the data
gg.train <- ggplot(dat.df,aes(x,y,color=class))+
    geom_point(size=3)+
    scale_color_brewer(palette="Set1")+
  ggtitle("Mixed Gaussian Data--Train")
gg.train

grid.df <- build.grid(dat.df,50)
names(grid.df)

#######################################################
## Bayes Classifier Following ESL description pages 17.
## pt is an (x, y) point
## returns the Bayes Classifier probabilityn of Class==1
## mu1, mu0  n means defining centers of Gaussians for classes 0,1
## fixed std.dev s. No correlation in covariance matrix
#######################################################
bayes.class <- function(pt){
  p0 <- mean(dnorm(pt[1],mu0[,1],std.dev)*dnorm(pt[2],mu0[,2],std.dev))
  p1 <- mean(dnorm(pt[1],mu1[,1],std.dev)*dnorm(pt[2],mu1[,2],std.dev))
  p1/(p0+p1)
}


grid.xy <- with(grid.df,matrix(c(x,y),nrow=nrow(grid.df)))
bayes.prob=apply(grid.xy,1,bayes.class)
##
grid.df <- within(grid.df,class <-
                  factor(ifelse(bayes.prob>0.5,1,-1)))
grid.df <- within(grid.df,prob <-bayes.prob)


gg.bayes <- ggplot()+
    geom_point(data=grid.df,aes(x,y,color=class),shape=15,size=5,alpha=.50)+
    stat_contour(data=grid.df,aes(x,y,z=prob),breaks=c(.5))+
    geom_point(data=dat.df,aes(x,y,color=factor(class)),size=4)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Bayes Decision Bounndary")
gg.bayes

##Bayes Error Rate...this is the theoretical optimal classification
bayes.prob=with(dat.df,apply(cbind(x,y),1,bayes.class))
bayes.pred=bayes.prob>0.5
err.bayes <- with(dat.df,mean((class==1)!=bayes.pred))
err.bayes


#################################
## SVM with a linear kernel
################################
cost.vals <- 10^seq(-2,4,length=10)
svmfit.tune <- tune.svm(class~x+y,data=dat.df,kernel="linear",cost=cost.vals,scale=T)

svmfit.lin <- svmfit.tune$best.model
svmfit.lin

### recreate the SVM plot
grid2.df <- build.grid(dat.df,70)
##
grid.class <- predict(svmfit.lin,newdata=grid2.df)
grid2.df <- within(grid2.df,class <- factor(grid.class))

dat.df <- within(dat.df,Support <- label %in% svmfit.lin$index)
dat.df <- within(dat.df,Size <- ifelse(Support,5,1))

gg.bayes <- ggplot()+
  geom_point(data=grid2.df,aes(x,y,color=class),shape=15,size=5,alpha=.20)+
  stat_contour(data=grid.df,aes(x,y,z=prob),breaks=c(.5))+
  geom_point(data=dat.df,aes(x,y,color=factor(class),shape=Support,size=Size))+
  scale_size(range=c(2,4))+ guides(size=FALSE,shape=F,color=F)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Bayes Decision Bounndary and SVM (Linear Kernel)")
gg.bayes

svm.pred <- predict(svmfit.lin,data=dat.df)
err.lin <- with(dat.df,mean( (class != svm.pred)))
err.lin

c(err.bayes,err.lin)
#######################################################
## Use a radial kernel
#######################################################
svm.tuned <- tune.svm(class~x+y,data=dat.df,kernel="radial",scale=T,
                     cost=cost.vals,gamma=.25)

svmfit.rad <- svm.tuned$best.model

### recreate the SVM plot
grid2.df <- build.grid(dat.df,70)
##
grid.class <- predict(svmfit.rad,newdata=grid2.df)
grid2.df <- within(grid2.df,class <- factor(grid.class))

dat.df <- within(dat.df,Support <- label %in% svmfit.rad$index)
dat.df <- within(dat.df,Size <- ifelse(Support,5,1))

gg.bayes <- ggplot()+
  geom_point(data=grid2.df,aes(x,y,color=class),shape=15,size=5,alpha=.20)+
  stat_contour(data=grid.df,aes(x,y,z=prob),breaks=c(.5))+
  geom_point(data=dat.df,aes(x,y,color=factor(class),shape=Support,size=Size))+
  scale_size(range=c(2,4))+ guides(size=FALSE,shape=F,color=F)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Bayes Decision Bounndary SVM (Radial Kernel)")
gg.bayes

svm.pred <- predict(svmfit.rad,data=dat.df)
err.rad <- with(dat.df,mean( (class != svm.pred)))
err.rad

c(err.bayes,err.lin,err.rad)