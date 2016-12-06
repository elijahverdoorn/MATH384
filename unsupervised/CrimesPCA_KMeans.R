library(ggplot2)
library(dplyr)

#######################################################
## Crime Data set from UCI Communities and Crime
## http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime
#######################################################
crime.df0 <- read.csv("communities.data.csv")
city0 <-  crime.df0$communityn
##only numeric fields
crime.df0 <- crime.df0[,6:100]

cr.names <- names(crime.df0)
cr.names
nr <- nrow(crime.df0)
nc <- ncol(crime.df0)

#######################################################
## get random sample of rows and columns..just to keep under control
numCities <- 100
numPred <- 30
sampR <- sample(nr,numCities)
sampC <- sample(nc,numPred)
crime.df <- crime.df0[sampR,sampC]
city <- city0[sampR]
nrow(crime.df)

#######################################################
## Check for NAs
noNA <- complete.cases(crime.df)
sum(!noNA)
crime.df <- crime.df[noNA,]
city <- city[noNA]
row.names(crime.df) <- city


#######################################################
## Principal Compoments
crime.pca <- prcomp(crime.df,scale=T)
summary(crime.pca)
biplot(crime.pca)


#######################################################
## Build rotations
rots <- crime.pca$rotation

crime.dat <- as.matrix(crime.df[,1:ncol(crime.df)])
crime.dat <- as.matrix(crime.dat)

#################################
## Check dims
dim(rots)
dim(crime.dat)

## Get first few components
pca1 <- crime.dat %*% rots[,1]
pca2 <- crime.dat %*% rots[,2]
pca3 <- crime.dat %*% rots[,3]



############################################
## K-means
crime.km <- kmeans(crime.df,5,nstart=100)

crime.km$tot.withinss
crime.km$cluster
nrow(crime.df)
crime.df$cluster.km <- crime.km$cluster
with(crime.df,table(cluster.km))


############################################
## look at clusers on top of the any two crime coordinates
## Not surprisingly, we don't see much of the clustering
coord1 <- 13
coord2 <- 21
crimeCluster.df <-
    data.frame(x=crime.dat[,coord1],y=crime.dat[,coord2],
               cluster.km=factor(crime.df$cluster.km),
               name=city)

ggplot(crimeCluster.df,aes(x,y,color=cluster.km))+
    geom_point(size=2)+
    geom_text(aes(label=name),size=3)+
    scale_color_brewer(palette="Set1")

#######################################################
## Do the same on the first two PCA components
crimeClusterPC.df <-
    data.frame(pca1,pca2,
               cluster.km=factor(crime.df$cluster.km),
               name=city)

ggplot(crimeClusterPC.df,aes(pca1,pca2,color=cluster.km))+
    geom_point(size=2)+
    geom_text(aes(label=name),size=3)+
    scale_color_brewer(palette="Set1")




