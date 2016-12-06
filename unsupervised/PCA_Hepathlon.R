library(ggplot2)
library(tidyverse)



##################################################################
## The first combined Olympic event for women was the pentathlon,
## first held in Germany in 1928. Initially this consisted of the shot
## putt, long jump, 100m, high jump and javelin events held over two
## days. The pentathlon was first introduced into the Olympic Games in
## 1964, when it consisted of the 80m hurdles, shot, high jump, long
## jump and 200m. In 1977 the 200m was replaced by the 800m and from
## 1981 the IAAF brought in the seven-event heptathlon in place of the
## pentathlon, with day one containing the events-100m hurdles, shot,
## high jump, 200m and day two, the long jump, javelin and 800m. A
## scoring system is used to assign points to the results from each
## event and the winner is the woman who accumulates the most points
## over the two days. The event made its first Olympic appearance in
## 1984.
##
## In the 1988 Olympics held in Seoul, the heptathlon was won by one
## of the stars of women's athletics in the USA, Jackie
## Joyner-Kersee. The results for all 25 competitors are given here.
##
##################################################################
##data("heptathlon", package = "HSAUR")
hep.df <- read.csv("heptathlon.csv")
pairs(hep.df)


#######################################################
## Fix these up a bit
hep.df <- hep.df%>%
    mutate(longjump = max(longjump)-longjump,
           shot =  max(shot)-shot,
           javelin=max(javelin)-javelin)


#######################################################
## Drop the name and the score
hep0.df <- hep.df%>%
    select(-Name,-score)
hep0.df

##Scale the data
hep.scale.df <- scale(hep0.df)
summary(hep.scale.df)

#######################################################
## Singular Value Decomposition
#######################################################
X <- as.matrix(hep.scale.df)
dim(X)
apply(X,2,mean)

## Covariance matrix (remember, mean=0)
n <- nrow(X)
##Compute X^t times X divided by n-1
C <- t(X) %*% X/(n-1)
## same the covariance matrix
cov(X)
C


#######################################################
## SVD for data matrix X
#######################################################
dim(X)
X.svd <- svd(X)


#######################################################
## Pull off the components again
U<- X.svd$u
dim(U)

V <- X.svd$v
dim(V)

d<- X.svd$d
D <- diag(d)
dim(D)
D


############################################
## Check orthogonality of U and V
round(t(U) %*% U)
round(t(V) %*% V)
V

#######################################################

#######################################################
## Principal Components
hep.pca <- prcomp(hep.scale.df)
summary(hep.pca)

x_v <- hep.pca$x
rot <- hep.pca$rotation
biplot(hep.pca)
biplot(x_v,rot)

##Parts of PCA
## How much variablility in each component
hep.pca$sdev
###same as (scaled by 1/(n-1))diagonal entries of SVD
n <- nrow(hep.scale.df)
d/sqrt(n-1)

## The principal component directions
hep.pca$rotation
#same as V fro SVD
V

#######################################################
## PC regression
pca.dat <- hep.pca$x
score <- hep.df$score
plot(pca.dat[,1],score)


mod.pca1 <- lm(score~pca.dat[,1])
summary(mod.pca1)



#######################################################
library(ISLR)
names(Hitters)
head(Hitters)
with(Hitters, hist(AtBat))
players <- rownames(Hitters)
Hitters$player <- players

hitters.df <- Hitters%>%
    filter(AtBat>400)%>%
    select(-NewLeague,-League,-Division,
           -PutOuts,-Assists,-Errors)

players <- with(hitters.df,player)
Salary <- with(hitters.df,Salary)

hitters.df <- select(hitters.df,-player,-Salary)


hitters.df <- scale(hitters.df)

cc <- complete.cases(hitters.df)
hitters.df <- hitters.df[cc,]
nrow(hitters.df)
head(hitters.df)

hitters.pc <- prcomp(hitters.df,retx=T)
summary(hitters.pc)
plot(hitters.pc)
biplot(hitters.pc)

H <- data.matrix(hitters.df)
dim(H)


rot <- hitters.pc$rota
dim(rot)
H.rot <- H %*% rot

#######################################################
## Linear model on original data
mod1 <- lm(Salary~data.matrix(hitters.df))
summary(mod1)

## linear model with just first few PCs
mod.pc <- lm(Salary~H.rot[,1:2])
summary(mod.pc)
names(mod.pc)


############################################
## Adjusted R^2 comparision
.4322/.4925
summary(hitters.pc)


players[113]
players[37]
players[39]
players[74]
