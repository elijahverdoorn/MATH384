library(MASS)
library(ggplot2)
library(dplyr)


##generate some data
N <- 40
K <- 3
mu1 <- c(-2,0)
mu2 <- c(2,0)
mu3 <- c(0,4)
sd0 <- 1/sqrt(5)

x1 <- mvrnorm(N,mu1,diag(c(1,1)*sd0))
x2 <- mvrnorm(N,mu2,diag(c(1,1)*sd0))
x3 <- mvrnorm(N,mu3,diag(c(1,1)*sd0))
X <- rbind(x1,x2,x3)


#######################################################
##
data.df <-
    data.frame(id=1:(K*N),
               x=X[,1],
               y=X[,2],
               ##randomly assign class
               Class=factor(sample(1:K,K*N,rep=T)))

dists2 <- function(pt) which.min(apply(centers.df[,2:3],1,
                              function(cent) (cent[1]-pt[1])^2+(cent[2]-pt[2])^2))

#######################################################
## Now do the interation the creates the k-means
centers.df <- data.df%>%
    group_by(Class)%>%
    summarize(x0=mean(x),
              y0=mean(y))
centers.df
ggplot(data.df,aes(x,y,color=Class))+
    geom_point(size=2)+
    geom_point(data=centers.df,aes(x0,y0,color=Class),size=5)+
    geom_point(data=centers.df,aes(x0,y0),size=5,shape=1,color="black")


#######################################################
data.df$Class <- factor(apply(data.df[,2:3],1,dists2))

