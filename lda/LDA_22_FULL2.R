remove(list=ls())
##lda example
library(MASS)
library(ggplot2)
library(dplyr)
##for pairs etc
library(GGally)
##for side-by-side
library(gridExtra)
##for contours
library(graphics)
##for knn
library(class)
##as usual

#######################
## LDA with p=2, K=3


##generate data p=2
## for binary classification K=2
N1=50
N2=N1*2
N=N1+N2
##classes (binary)
K=2
##Features
p=2



#################################
## SIGMA for multivariate normal
#################################
############################################
## same covariances QDA
## #########################################
## Random covariance matrix
##controls the convariance
# k=1.5
#x1=matrix(runif(4,-k,k),nrow=2)
#(S=t(x1)%*%x1)
## Fixed covariance
S <- matrix(c(1,.5,.5,1),nrow=2)
##data for the two classes. different means, same S
dat1=mvrnorm(N1,c(1,1),S)
dat2=mvrnorm(N2,c(0,.5),S)
dat=rbind(dat1,dat2)
##make a data frame and add class information
sameCOV.df=data.frame(x=dat[,1],y=dat[,2],
                      cat=factor(c(rep(1,N1),rep(2,N2))))

#######################################################
##Use lda function
## linear discriminant
#######################################################
data.df <- sameCOV.df
head(data.df)
nrow(data.df)

##plot the grid values versus the original values
dat.gg<-ggplot()+
  geom_point(data=data.df,aes(x,y,color=cat),size=2)+
    scale_color_brewer(palette="Set1")
dat.gg

###Estimate Sigma,mu,pi

#################################
## Linear discriminant analysis model
lda.fit=lda(cat~x+y,data=data.df)
##the predicted categories
##training
lda.pred <- predict(lda.fit,data.df)

val<-lda.pred$class
data.df <- mutate(data.df,cat.pred.lda = val)
with(data.df,table(cat,cat.pred.lda))

########
# Use logistic...both do exactly the same on this sort of data
str(data.df)
glm.fit<-glm(cat~x+y,data=data.df,family="binomial")
val<-(predict(glm.fit,type="response")>0.5)
data.df<-mutate(data.df,cat.pred.glm=ifelse(val,1,0) )
with(data.df,table(cat,cat.pred.glm))

with(data.df,table(cat.pred.lda,cat.pred.glm))


## make a plotting grid in x and y
GS <- 60
x.vals0<-with(data.df,seq(min(x),max(x),length=GS))
y.vals0<-with(data.df,seq(min(y),max(y),length=GS))
grid.df <- expand.grid(x.vals0,y.vals0)
names(grid.df) <- c("x","y")
###make predictions for grid xy vals
grid.lda <- predict(lda.fit,grid.df)
grid.df <- within(grid.df,cat.lda <- grid.lda$class)
##the probabilities
grid.df <- within(grid.df,probs <- grid.lda$posterior[,1])
head(grid.df)

##plot the grid values versus the original values
grid.gg <- ggplot()+
  geom_point(data=data.df,aes(x,y,color=cat),size=2)+
  geom_point(data=grid.df,aes(x,y,color=cat.lda),size=1,shape=9,alpha=0.6)+
##  stat_contour(data=grid.df,aes(x,y,z=probs),breaks=c(0.5))+
  scale_color_brewer(palette="Set1")+
  ggtitle("LDA")
grid.gg


#######################################################
##LDA....project onto 1 dimensional space with separation
#######################################################
##look at lda.fit
lda.fit
##this is the projection onto the line of maximal separation
PROJ <-  lda.fit$scaling
PROJ
(w <- PROJ[,1])
##Normalize w. Calculate the length of w
p <- as.numeric((t(w)%*% w))
w<-w/sqrt(p)
w
##try with any other line
##theta <- .6
#w <- matrix(c(cos(theta),sin(theta)),nrow=2)

##compute  the projections of the puts (x,y) onto normal P
dat.proj <- dat%*%w
##Data frame of points on projected line
proj.df <- data.frame(
           proj= dat.proj,
           x=dat.proj*w[1],
           y=dat.proj*w[2],
           x0=dat[,1],
           y0=dat[,2],
           cat=data.df$cat)
head(proj.df)

sum0.df <- proj.df%>%
    group_by(cat)%>%
    summarize(mu=mean(proj),
              s2=var(proj))
sum0.df

temp <-cbind(sum0.df[1,2:3],sum0.df[2,2:3])
Jval<-abs(temp[1]-temp[3])/(temp[2]+temp[4])
Jval

J <- function(theta){
    w0 <- matrix(c(cos(theta),sin(theta)),nrow=2)
    ##compute  the projections of the puts (x,y) onto normal P
    dat.proj <- dat%*%w0
    ##Data frame of points on projected line
    proj.df <- data.frame(
        proj= dat.proj,
           cat=data.df$cat)        
    sum0.df <- proj.df%>%
        group_by(cat)%>%
        summarize(mu=mean(proj),
                  s2=var(proj))
    temp <-cbind(sum0.df[1,2:3],sum0.df[2,2:3])
    abs(temp[1]-temp[3])/(temp[2]+temp[4])
}
J(0)
tval <- seq(-pi/2,pi/2,length=100)
vals <- unlist(lapply(tval,J))
max(vals)
plot(tval,vals,type="l")


##add the projections onto the line of maximal separation
proj.gg <- grid.gg+
    geom_point(data=proj.df,aes(x,y,color=cat),size=3)+
    geom_line(data=proj.df,aes(x,y),size=.5,color="black")+
    coord_fixed()+
    ggtitle("LDA with projection onto line of max separation")
proj.gg

##Just for fun...show the projections of the data onto the LDA vector
proj.gg2 <- ggplot(data=proj.df)+
  geom_point(aes(x,y,color=cat),size=3)+
  geom_point(aes(x0,y0),size=1)+
  geom_line(aes(x,y),size=.5,color="black")+
 coord_fixed()+
  ##scale_x_continuous(limits=c(-3,3))+
  ##scale_y_continuous(limits=c(-3,3))+
##  geom_segment(aes(x=x,xend=x0,y=y,yend=y0,color=cat),
##               size=3,alpha=.25)+
  ggtitle("LDA with projection onto line of max separation")
proj.gg2


##One dimensional reduction
oneD.gg <- ggplot(proj.df,aes(proj,fill=cat))+
    geom_density(alpha=.5)+
    scale_fill_brewer(palette="Set1")+
    ggtitle("LDA: Dimension Reduction for LDA")
oneD.gg

##put both together...assuming grid.arrange is available.
grid.arrange(oneD.gg,proj.gg,nrow=1)


mod.glm1<-glm(cat~proj,data=proj.df,family="binomial")
pred.glm1<-predict(mod.glm1,type="response")>0.5
table(pred.glm1,proj.df$cat)

mod.glm2<-glm(cat~y,data=proj.df,family="binomial")
pred.glm2<-predict(mod.glm2,type="response")>0.5
table(pred.glm2,proj.df$cat)

summary(mod.glm1)
summary(mod.glm2)
