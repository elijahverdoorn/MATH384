library(tidyverse)

#######################################################
## Construct a simple example for PCA
#######################################################
n <- 40
b0 <- 1
b1 <- 3
eps <- 4

x <- rnorm(n,1,2)
y <- b1*x+rnorm(n,0,eps)

#######################################################
X <- matrix(c(x,y),ncol=2)
colnames(X) <- c("x","y")
##mean zero in columns
mus <- apply(X,2,mean)
X <- t(t(X)-mus)

ggplot(data=NULL,aes(X[,1],X[,2]))+
    geom_point(color="blue")

#######################################################
## Covariance
cov(X)

var(X[,1],X[,1])
sum(X[,1]^2)/(n-1)
#######################################################
## Singular Value Decomposition

X.svd <- svd(X)
U <- X.svd$u
D <- diag(X.svd$d)
V <- X.svd$v

#Check dimensions
dim(U)
dim(D)
dim(V)
D
##check orthogonality of U and V
round(t(U)%*% U,4)
round(t(V)%*% V,4)


#######################################################
## Compare to eigenvalues of covariance matrix
eigenStuff <- eigen(cov(X))
evals <- eigenStuff$values
##check values
(D%*%D)/(n-1)
evals
####
eigenStuff$vectors
V
v1 <- V[,1]
v2 <- V[,2]

#######################################################
eScale=10
data.df <- data.frame(x1=X[,1],x2=X[,2],label=1:n)
ggplot(data.df,aes(x1,x2))+
    geom_label(aes(label=label))+
    geom_segment(aes(x=0,xend=eScale*v1[1],y=0,yend=eScale*v1[2]),color="red",
                 size=2,
                 arrow = arrow(length = unit(0.03, "npc")))+
    geom_segment(aes(x=0,xend=eScale*v2[1],y=0,yend=eScale*v2[2]),color="red",
                 size=2,
                 arrow = arrow(length = unit(0.03, "npc")))+
    coord_fixed()


#######################################################
## Change to eigen coordinate system
X_v <- X %*% eigenStuff$vectors
dataV.df <- data.frame(z1=X_v[,1],z2=X_v[,2],label=1:n)

ggplot(dataV.df,aes(z1,z2))+
    geom_label(aes(label=label))+
    geom_segment(aes(x=0,xend=1,y=0,yend=0),color="red",
                 size=2,
                 arrow = arrow(length = unit(0.03, "npc")))+
    geom_segment(aes(x=0,xend=0,y=0,yend=1),color="red",
                 size=2,
                 arrow = arrow(length = unit(0.03, "npc")))+
    coord_fixed()


#######################################################
## Principal Components does all this
pr.mod <- prcomp(X,scale=T)
summary(pr.mod)
pr.mod$rotation

biplot(pr.mod)



#######################################################

#######################################################
## Add more variables with correlations
n <- 50
eps <- 5
x <- rnorm(n,1,2)

### y,z,w
y <- rnorm(n,1,2)
z <- b1*y+rnorm(n,0,eps)
w <- y+z+rnorm(n,0,eps)
X <- matrix(c(x,y,z,w),ncol=4)
colnames(X) <- c("x","y","z","w")

pr <- prcomp(X,scale=T)
biplot(pr)

#######################################################
## Create biplot by hand
lambda <- pr$sdev*sqrt(n)
##X in the new basis..same as X_v
x_v <- pr$x
##The rotation matrix...save as V above
rot <- pr$rotation

##scale everything by sd
n <- nrow(X_v)
lambda <- pr$sdev*sqrt(n)
##scale each column by lambda
x_v2<-t(t(x_v)/lambda)
##resscale rotation
rot2 <- t(t(rot)*lambda)
biplot(x_v2,rot2)

pca.df <- data.frame(rot1=rot2[,1]/5,rot2=rot2[,2]/5)
coords.df <- data.frame(xv1=x_v2[,1],xv2=x_v2[,2],nm=paste0("v",1:nrow(x_v2)))

ggplot()+
    geom_segment(data=pca.df,aes(x=0,y=0,xend=rot1,yend=rot2),color="red",
                 arrow = arrow(length = unit(0.03, "npc")))+
#    geom_label(data=pca.df,aes(rot1,rot2))+
    geom_label(data=coords.df,aes(xv1,xv2,label=nm))

biplot(pr)



