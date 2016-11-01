library(MASS)
mvrnorm(n,c(1,2),diag(2,3))
b0<-3
b1<-1
b2<-2
n<-100


X<-mvrnorm(n,c(0,0),diag(c(10,1),2))
dim(X)
y<-b0+X%*% c(b1,b2)+rnorm(n,0,1)
dat.df<-data.frame(X,y)
names(dat.df)

ggplot(dat.df,aes(X1,X2))+geom_point()+
  coord_fixed()

### Can see that X1 and X2 have very different variances
ggplot(dat.df)+
  geom_density(aes(X1),color="black",fill="red",alpha=.5)+
  geom_density(aes(X2),color="black",fill="blue",alpha=.5)


##########################
##Principal Component Model
## The plan is to identify orthogonal directions which successively
## maximize the variance after project onto the directions

mod.pca1<-prcomp(X)
summary(mod.pca1)

##THe rotation to optimize the variances in the identity (approximately)!
rot<-mod.pca1$rotation
rot


#############################################
## Rotate the data
## define a rotation matrix with angle theta
theta=pi/3
rotMat<-matrix( c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2)
rotMat

### Rotate the data
Xrot<-X %*% rotMat
datRot.df<-data.frame(Xrot,y)
names(datRot.df)
ggplot(datRot.df,aes(X1,X2))+geom_point()+
  coord_fixed()

##Now X1, X2 have about the same variance
ggplot(datRot.df)+
  geom_density(aes(X1),color="black",fill="red",alpha=.5)+
  geom_density(aes(X2),color="black",fill="blue",alpha=.5)

######################
## Principal components model
mod.pca<-prcomp(Xrot)
## Now the rotation to optimize the variances is the inverse of rot
rot2<-mod.pca$rotation
rot2

###Note...the rotates back to the rot=idenity (approximately)
rot2%*% rotMat

X2<-Xrot %*% rot2
dat2.df<-data.frame(X2)
names(dat2.df)<-c("X1","X2")
##look familar???
ggplot(dat2.df)+
  geom_density(aes(X1),color="black",fill="red",alpha=.5)+
  geom_density(aes(X2),color="black",fill="blue",alpha=.5)
