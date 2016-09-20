#######################################################
## Reconstructing ESL Figure 2.5 (page 21)
## Bayes Classifier for a mixed Gaussian model.
## Also a home-grown KNN function.
#######################################################
library(ggplot2)
library(dplyr)
library(tidyr)
##
library(MASS) ##for mvnorm
library(class) ##for knn function
#######################################################

#######################################################
## Create a synthetic training data frame for classificaton
## sample the means  N times each to build the training data set for
## each of the classes
#######################################################
#######################################################
## generate values according to ESL example page 21
## start with multivariate normal means for classes 0 and 1.
## n means at (1,0) and 10 at (-1,0) with sigma=Id
#######################################################
##fixed sd for samples
sd<- sqrt(1/5)

##center
n <- 10
n <- 3
mu0 <- mvrnorm(n,c(0,1),diag(1,2))
mu1 <- mvrnorm(n,c(1,0),diag(1,2))

############################################
## build a training data set
#######################################################
N <- 200 ## pretty big training set
rnds <- sample(1:n,N,rep=T)
vals1 <- t(apply(mu1[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*sd)))
rnds <- sample(1:n,N,rep=T)
vals0<- t(apply(mu0[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*sd)))
vals <- rbind(vals1,vals0)
##put in a data frame
train.df <-
    data.frame(row=1:(2*N),x=vals[,1],y=vals[,2],class=rep(c("A","B"),each=N))

##plot the data
gg.train <- ggplot(train.df,aes(x,y,color=class))+
    geom_point(size=2)+
    scale_color_manual(values=c("red","blue"))+
  ggtitle("Mixed Gaussian Data--Train")
gg.train

#######################################################
##   Show and tell time
##  build a grid to show the region classifications
#######################################################
## grid size
gridSize <- 100
#gridSize <- 15
xmin <- with(train.df,min(x))
xmax <- with(train.df,max(x))
ymin <- with(train.df,min(y))
ymax <- with(train.df,max(y))
##
xvals=seq(xmin,xmax,length=gridSize)
yvals=seq(ymin,ymax,length=gridSize)
grid.xy=expand.grid(xvals,yvals)
grid.df <- data.frame(x=grid.xy[,1],
                      y=grid.xy[,2],
                      ##randomly assign classes...we will do more with
                      ##these later
                      class=sample(c("A","B"),rep=T,gridSize*gridSize))

ggplot()+
  geom_point(data=grid.df,aes(x,y,color=class),shape=15,size=2,alpha=1)
#######################################################


#######################################################
## Build KNN model using built-in R knn
?knn
#################################
## set up the data for knn
train.dat <- train.df[c("x","y")]
test.dat <- grid.df[c("x","y")]
cl <-    train.df[,c("class")]
#################################
head(train.df)
kval <- 15
mod.knn <- knn(train.dat,test.dat,cl,k=kval,prob=T)

##value of mod.knn is the classification
table(mod.knn)

##probabilities are the proportion in nbhd that correspond to
##prediction
## In other words, how sure we are of the prediction. 0.5 is a coin flip
head(attr(mod.knn,"prob"))


############################################
## add in some extra info
############################################
grid.df <- grid.df %>%
    mutate(class=mod.knn)%>%
    ##some probabilities
    mutate(knn.prob=attr(mod.knn,"prob"),
           knn.prob2=ifelse(class=="A",knn.prob,
                            1-knn.prob))


#######################################################
## how are we doing??
## Classification Regions
#######################################################
gg.knn <- ggplot()+
  geom_point(data=grid.df,aes(x,y,color=class),shape=15,size=5,alpha=.05)+
  geom_point(data=train.df,aes(x,y,color=factor(class)),size=2)+
  stat_contour(data=grid.df,aes(x,y,z=knn.prob),breaks=c(.5))+
  scale_color_manual(values=c("red","blue"))+
  ggtitle(sprintf("KNN neighbor classification regions (k=%s)",kval))
gg.knn

#######################################################
## Classification Regions Probabilites
gg.knn2 <- ggplot()+
    geom_point(data=grid.df,aes(x,y,color=knn.prob),shape=15,size=5,alpha=.25)+
    scale_color_gradient2(low="green",high="brown",midpoint=0.75)+
    geom_point(data=filter(train.df,class=="A"),
               aes(x,y),size=2,color="red")+
    geom_point(data=filter(train.df,class=="B"),
               aes(x,y),size=2,color="blue")+
    labs(color="Prob")+
    ggtitle(sprintf("KNN neighbor classification probabilities (k=%s)",kval))
gg.knn2





#######################################################
##error rate on training set
#######################################################
train.dat <- train.df[c("x","y")]
cl <-    train.df[,c("class")]
kval <- 15
mod.knn <- knn(train.dat,train.dat,cl,k=kval,prob=T)

train.df <- train.df%>%
    mutate(class.pred=mod.knn)
head(train.df)

#################################
## confusion matrix
#################################
with(train.df,
     table(class,class.pred))

err.train <- with(train.df,mean(!class==class.pred))
err.train

#######################################################
## Test data

############################################
## build a training data set of same size
#######################################################
N <- 200
rnds <- sample(1:n,N,rep=T)
vals1 <- t(apply(mu1[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*sd)))
rnds <- sample(1:n,N,rep=T)
vals0<- t(apply(mu0[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*sd)))
vals <- rbind(vals1,vals0)

##put in a data frame
test.df <-
    data.frame(row=1:(2*N),x=vals[,1],y=vals[,2],class=rep(c("A","B"),each=N))

test.dat <- test.df[c("x","y")]
mod.knn2 <- knn(train.dat,test.dat,cl,k=kval,prob=T)

test.df <- test.df%>%
    mutate(class.pred=mod.knn2)
head(test.df)
##confusion matrix
with(test.df,
     table(class,class.pred))

err.test <- with(test.df,mean(!class==class.pred))
err.test
c(err.train,err.test)

#######################################################
## repeat with same train+test, but different values of k
#######################################################
maxK<-100
errVals <- matrix(nrow=maxK,ncol=2)
for(kval in 1:maxK){
    print(kval)
    mod.train <- knn(train.dat,train.dat,cl,k=kval,prob=T)
        mod.test  <- knn(train.dat,test.dat,cl,k=kval,prob=T)
    train.df <- train.df%>%
        mutate(class.pred=mod.train)
    test.df <- test.df%>%
        mutate(class.pred=mod.test)
    err.train <- with(train.df,mean(!class==class.pred))
    err.test <- with(test.df,mean(!class==class.pred))
    errVals[kval,] <- c(err.train,err.test)
}
errVals

err.df<-data.frame(k=1:maxK,errVals)
names(err.df) <- c("k","trainError","testError")

err.df <- err.df %>%
    gather(type,val,2:3)
head(err.df)

ggplot(err.df,aes(k,val,color=type))+
    geom_point()+geom_line()+
    geom_smooth(se=F)+
    labs(color="")+
  ggtitle("Test/Train Error Rate")



#######################################################
## Bayes classifer regions
## This is the theoretical optimal classification rate
#######################################################
##data frame of mu values with classes
mu.df <-  data.frame(x=c(mu1[,1],mu0[,1]),
                     y=c(mu1[,2],mu0[,2]),
                     class=rep(c("A","B"),each=n))
##Plot the means
ggplot()+
  geom_point(data=mu.df,aes(x,y,color=class),size=4)+
    scale_color_manual(values=c("blue","red"))

#######################################################
## Bayes Classifier Following ESL description pages 17.
## pt is an (x, y) point
## returns the Bayes Classifier probabilityn of Class==1
## mu1, mu0  n means defining centers of Gaussians for classes 0,1
## fixed SD s. No correlation in covariance matrix
#######################################################
bayes.class <- function(pt){
  p0 <- mean(dnorm(pt[1],mu0[,1],sd)*dnorm(pt[2],mu0[,2],sd))
  p1 <- mean(dnorm(pt[1],mu1[,1],sd)*dnorm(pt[2],mu1[,2],sd))
  p1/(p0+p1)
}

## Add the bayes classifier to the grid
bayes.prob=apply(grid.xy,1,bayes.class)
grid.df <- data.frame(x=grid.xy[,1],y=grid.xy[,2],class=factor(ifelse(bayes.prob>0.5,"A","B")),prob=bayes.prob)


ggplot()+
  geom_point(data=grid.df,aes(x,y,color=class),shape=15,size=5,alpha=.10)+
  scale_color_manual(values=c("blue","red"))+
  geom_point(data=mu.df,aes(x,y,color=factor(class)),size=4)+
  scale_x_continuous(limits=c(xmin,xmax))+
  stat_contour(data=grid.df,aes(x,y,z=prob),breaks=c(.5))+
  ggtitle("Bayes Classifer: ESL Figure 2.5")

#######################################################

#######################################################
## Irreducible Bayes Error rate
## look at test.df or train.df and see what the Bayes Classification predicts.
#######################################################

#######################################################
##Estimate Bayes Error Rate
## 1-E(max_j(P(Y=j|X))
#######################################################
prob.max <- apply(matrix(grid.df$prob,nrow=nrow(grid.df)),1,
                  function(x) max(x,1-x))
1-mean(prob.max)
#######################################################n



############################################
## Computational example
#######################################################
diamonds.df <- read.csv("Diamonds.csv")
ggplot(diamonds,aes(carat,table,color=cut))+
    geom_point(size=2)

#########################################################
## Split into train and test (same size each). Determine optimal number
## of nearest neighbors to predict cut from carat and table values.
#########################################################

n <- nrow(diamonds)
train <- sample(1:n, n/2, rep=F)
test <- setdiff(1:n, train)
train
test

train.df <- diamonds[train,]
test.df <- diamonds[test,]

kval <- 10
tr <- train.df[c("carat", "table")]
dim(tr)
cl <- train.df[,"cut"]
dim(cl)

mod.train <- knn(train.df[c("carat", "table")],
                 test.df[c("carat", "table")],
                 train.df[,"cut"], k = kval, prob = T)
test.df <- mutate(test.df, class.pred = mod.train)
head(test.df)
with(test.df, mean(class == class.pred))

# see moodle for the complete working file

