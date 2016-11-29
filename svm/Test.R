library(ggplot2)
library(dplyr)
N <- 200
b <- 1
f1 <- function(x1,x2) b+x1-x2
f2 <- function(x1,x2) -b+x1-x2


eps <- 2
Xa <- matrix(runif(2*N,0,4),nrow=N)
c1 <- apply(Xa,1,function(x) sign(f1(x[1],x[2])-eps))  <0
Xb <- matrix(runif(2*N,0,4),nrow=N)
c2 <- apply(Xb,1,function(x) sign(f2(x[1],x[2])+eps))>0

dat.df <- data.frame(rbind(Xa[c1,],Xb[c2,]),class=c(rep("A",sum(c1)),rep("B",sum(c2))))
names(dat.df) <- c("x1","x2","class")
nrow(dat.df)

ggplot(dat.df,aes(x1,x2,color=class))+
    geom_point(size=2)+
    scale_color_manual(values=c("red","blue"))+
    geom_abline(slope=1,intercept=b,color="black",linetype="dashed",size=1.5)+
    geom_abline(slope=1,intercept=-b,color="black",linetype="dashed",size=1.5)+
    geom_abline(slope=1,intercept=0,color="black",linetype="dashed",size=1.5)

mod1 <- glm(class~x1+x2,family="binomial",data=dat.df)

library(e1071)

C <- .0001000
svmfit <-
    svm(class~x1+x2,data=dat.df,cost=C,
        kernel="linear",scale=F)
svmfit

str(svmfit)
sv.df <- data.frame(svmfit$SV,Support=T)

dat.df0 <- dat.df%>%
    left_join(sv.df)%>%
    mutate(Support=!is.na(Support))
with(dat.df0,table(Support))

ggplot(dat.df0,aes(x1,x2,color=class,size=.25*Support+2))+
    geom_point()+
    scale_color_manual(values=c("red","blue"))

preds.svm <- predict(svmfit,newdata=dat.df)
with(dat.df,table(class,preds.svm))
#######################################################

N <- 200
b <- 1
f1 <- function(x1,x2) b+x1-x2-.2*x1^2
f2 <- function(x1,x2) -b+x1-x2+x2^2


eps <- .02
Xa <- matrix(runif(2*N,0,4),nrow=N)
c1 <- apply(Xa,1,function(x) sign(f1(x[1],x[2])-eps))  <0
Xb <- matrix(runif(2*N,0,4),nrow=N)
c2 <- apply(Xb,1,function(x) sign(f2(x[1],x[2])+eps))>0

dat.df <- data.frame(rbind(Xa[c1,],Xb[c2,]),class=c(rep("A",sum(c1)),rep("B",sum(c2))))
names(dat.df) <- c("x1","x2","class")
nrow(dat.df)

ggplot(dat.df,aes(x1,x2,color=class))+
    geom_point(size=2)+
    scale_color_manual(values=c("red","blue"))

mod1 <- glm(class~x1+x2,family="binomial",data=dat.df)

dat.mat <- data.matrix(dat.df)
head(dat.mat)
X1 <- dat.mat[,1]
X2 <- dat.mat[,2]

X12 <- X1*X2
X11 <- X1*X1
X22 <- X2*X2

dat.mat2 <- cbind(X11,X22,X12,dat.mat)
dim(dat.mat2)

dat.df2 <- data.frame(dat.mat2)
dat.df2 <- mutate(dat.df2,class=factor(class))
names(dat.df2)


C <- 1
svmfit <-
    svm(class~.,data=dat.df2,cost=C,
        kernel="linear",scale=F)

preds.svm <- predict(svmfit,newdata=dat.df2)
with(dat.df2,table(class,preds.svm))


C <- 1
svmfit <-
    svm(class~.,data=dat.df,cost=C,
        kernel="linear",scale=F)

preds.svm <- predict(svmfit,newdata=dat.df)
with(dat.df,table(class,preds.svm))


ggplot(dat.df2,aes(x1,x2,color=preds.svm))+
    geom_point(size=2)+
    scale_color_manual(values=c("red","blue"))


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
mu0 <- mvrnorm(n,c(0,1),diag(1,2))
mu1 <- mvrnorm(n,c(1,0),diag(1,2))

############################################
N <- 200 ## pretty big training set
rnds <- sample(1:n,N,rep=T)
vals1 <- t(apply(mu1[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*sd)))
rnds <- sample(1:n,N,rep=T)
vals0<- t(apply(mu0[rnds,],1,function(mu) mvrnorm(1,mu,diag(2)*sd)))
vals <- rbind(vals1,vals0)
##put in a data frame
train.df <-
    data.frame(row=1:(2*N),x=vals[,1],y=vals[,2],class=rep(c("A","B"),each=N))

#######################################################
##build a grid to show the region classifications
## grid size
GS <- 100
#GS <- 15
xmin <- with(train.df,min(x))
xmax <- with(train.df,max(x))
ymin <- with(train.df,min(y))
ymax <- with(train.df,max(y))
##
xvals=seq(xmin,xmax,length=GS)
yvals=seq(ymin,ymax,length=GS)
grid.xy=expand.grid(xvals,yvals)
grid.df <- data.frame(x=grid.xy[,1],y=grid.xy[,2])


############################################
C <- 1
svmfit <-
    svm(class~x+y,data=train.df,cost=C,
        kernel="linear",scale=F)

svmfit <-
    svm(class~x+y,data=train.df,cost=C,
        kernel="polynomial",degree=3,scale=F)


svmfit <-
    svm(class~x+y,data=train.df,cost=C,
        kernel="polynomial",degree=10,scale=F)

svmfit <-
    svm(class~x+y,data=train.df,cost=C,
        kernel="radial",gamma=50,scale=F)

#######################################################
grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm
##
ggplot()+
    geom_point(data=grid.df,aes(x,y,color=pred),shape=15,size=5,alpha=.05)+
    geom_point(data=train.df,aes(x,y,color=factor(class)),size=3)+
    scale_color_manual(values=c("red","blue"))

