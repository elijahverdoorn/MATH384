
n <- 1000
f <- function(x)
    sqrt(1/4-(x-0.5)^2)

Xa <- matrix(runif(2*n,0,1),nrow=n)
#Xa[,2] <- 2*Xa[,2]
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
## grid size
GS <- 100
#GS <- 15
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


C <- 1
svmfit <-
    svm(class~x1+x2,data=dat.df,cost=C,
        kernel="polynomial",degree=4,scale=F)
grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
  scale_color_manual(values=c("red","blue"))


###########################
g <- .1
g <- 1
g <- 10
g <- 100
g<-1000


svmfit <-
    svm(class~x1+x2,data=dat.df,cost=C,
        kernel="radial",gamma=g,scale=F)

grid.svm <- predict(svmfit,newdata=grid.df)
grid.df$pred <- grid.svm

ggplot()+
  geom_point(data=grid.df,aes(x1,x2,color=pred),shape=15,size=5,alpha=.05)+
  geom_point(data=dat.df,aes(x1,x2,color=factor(class)),size=1)+
  scale_color_manual(values=c("red","blue"))

