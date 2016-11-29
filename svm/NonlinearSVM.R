#######################################################
## A synthetic demonstration of SVM for classification

#######################################################
## Build a wild and wooly data set
#######################################################
## two function choices


f <- function(x)
    sqrt(1/4-(x-0.5)^2)
x <- seq(0,1,length=100)
plot(x,f(x))

f <- function(x)
    .5*x+.25*sin(20*x)

x <- seq(0,1,length=100)
plot(x,f(x))



#######################################################
## Generate data with this curve as the approximate dividing line
n <- 1000
Xa <- matrix(runif(2*n,0,1),nrow=n)
c1 <- apply(Xa,1,function(x) x[2] - f(x[1]))
c1 <- sign(c1+rnorm(n,0,.25))

dat.df <- data.frame(Xa,class=factor(c1))
names(dat.df) <- c("x1","x2","class")
nrow(dat.df)


ggplot(dat.df,aes(x1,x2,color=class))+
    geom_point(size=2)+
    scale_color_manual(values=c("red","blue"))

#######################################################
##build a grid to show the region classifications
## the grid size
GS <- 100
xmin <- with(dat.df,min(x1))
xmax <- with(dat.df,max(x1))
ymin <- with(dat.df,min(x2))
ymax<- with(dat.df,max(x2))
##
xvals=seq(xmin,xmax,length=GS)
yvals=seq(ymin,ymax,length=GS)
grid.xy=expand.grid(xvals,yvals)
grid.df <- data.frame(x1=grid.xy[,1],x2=grid.xy[,2])
##

#######################################################
## SVM Classification
#######################################################

#######################################################
## Start with linear classifier. Note. Our data is already scaled
## Cost is the only parameter in play
C <- 1
svmfit <-
    svm(class~x1+x2,
        data=dat.df,
        kernel="linear",
        cost=C,
        scale=F)

grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
  scale_color_manual(values=c("red","blue"))

#######################################################
## Cross-validate to find best C
tune.lin <-
    tune(svm,class~x1+x2,
        data=dat.df,
        kernel="linear",
        scale=F,
        ranges=list(cost=10^seq(-2,2,length=10)),
        ##use cross=3 to speed up
        tunecontrol=tune.control(cross=3))

summary(tune.lin)
str(tune.lin)
tune.lin$best.parameters

#######################################################
## Use best value of C from CV
C <- .6
svmfit <-
    svm(class~x1+x2,
        data=dat.df,
        kernel="linear",
        cost=C,
        scale=F)

grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
  scale_color_manual(values=c("red","blue"))

#######################################################
## SVM with polynomial kernel
#######################################################
C <- 1
D=2
svmfit <-
    svm(class~x1+x2,data=dat.df,
        kernel="polynomial",
        degree=D,
        cost=C,
        scale=F)
grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
  scale_color_manual(values=c("red","blue"))


###########################
## CV with cost and degree..will take longer because there are two parameters
tune.lin <-
    tune(svm,class~x1+x2,
        data=dat.df,
        kernel="polynomial",
        scale=F,
        ranges=list(cost=10^seq(-2,2,length=10),
                    d=seq(1,5)),
        ##use cross=3 to speed up
        tunecontrol=tune.control(cross=3))

tune.lin
plot(tune.lin)
(best <- tune.lin$best.parameters)


C <- best[1]
D=best[2]
svmfit <-
    svm(class~x1+x2,data=dat.df,
        kernel="polynomial",
        degree=D,
        cost=C,
        scale=F)
grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm


ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
  scale_color_manual(values=c("red","blue"))


#######################################################
## Radial Kernel...gamma is the "precision"
C=10
g=1
svmfit <-
    svm(class~x1+x2,data=dat.df,
        kernel="radial",
        gamma=g,
        cost=C,
        scale=F)

grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
    scale_color_manual(values=c("red","blue"))+
    scale_y_continuous(limits=c(0,1))


#######################################################
## CV in gamma and cost
tune.lin <-
    tune(svm,class~x1+x2,
        data=dat.df,
        kernel="radial",
        scale=F,
        ranges=list(cost=10^seq(-2,2,length=10),
                   gamma=10^seq(-2,3)),
        ##use cross=3 to speed up
        tunecontrol=tune.control(cross=3))


tune.lin
plot(tune.lin)
(best <- tune.lin$best.parameters)

C=best[1]
g=best[2]
svmfit <-
    svm(class~x1+x2,data=dat.df,
        kernel="radial",
        gamma=g,
        cost=C,
        scale=F)

grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
    scale_color_manual(values=c("red","blue"))+
    scale_y_continuous(limits=c(0,1))


ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
    scale_color_manual(values=c("red","blue"))+
    scale_y_continuous(limits=c(0,1))+
    geom_line(aes(x,f(x)),size=1)
#######################################################


