library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


#######################################################
## Some nifty county-level data
county.df <-
    read.csv("county_facts.csv")

## info on the fields

countyInfo.df <- read.csv("county_facts_dictionary.csv")
head(countyInfo.df)

county.df <- county.df%>%
    filter(state_abbreviation != "")%>%
    droplevels()

county.df0 <- county.df%>%
    select(-area_name,-state_abbreviation,-fips)


#######################################################
## County-level election data from 2014
elect.df <-
    read.csv("US_elect_county.csv")

head(elect.df)

elect.df <- elect.df%>%
    select(fips,ObamaPerc)%>%
    filter(ObamaPerc!="#DIV/0!")%>%
    droplevels()%>%
    mutate(ObamaPerc=as.numeric(as.character(ObamaPerc)))

countyElect.df <- county.df%>%
    inner_join(elect.df)%>%
    filter(state_abbreviation !="",!is.na(ObamaPerc))%>%
    select(-fips,-area_name,-state_abbreviation)



#######################################################
##  Principal Components  elementary modeling
#######################################################
nn <- nrow(countyElect.df)
train <- sample(nn,nn/2)
train.df <- countyElect.df[train,]
test.df <- countyElect.df[-train,]

## matrices
nc <- ncol(train.df)
train.mat <- scale(data.matrix(train.df[,-nc]))
train.resp.mat <- data.matrix(train.df[,nc])

test.mat <- scale(data.matrix(test.df[,-nc]))
test.resp.mat <- data.matrix(test.df[,nc])


#######################################################
## Principal Components
#######################################################
##install.packages("pls")
library(pls)

mod.pcr <- pcr(train.resp.mat~train.mat,validation="CV")
validationplot(mod.pcr,val.type="MSEP")
summary(mod.pcr)


preds.pcr <- predict(mod.pcr,newdata=test.mat,ncomp=50)
head(preds.pcr)
str(preds.pcr)
pp<-as.numeric(preds.pcr)
str(pp)

test.df<-test.df %>%
  ungroup() %>%
  mutate(pred=pp)

ggplot(test.df,aes(ObamaPerc,pred))+geom_point()
with(test.df,mean( (ObamaPerc-preds.pcr)^2))

#######################################################
## What does PCR mean??
#######################################################
dat.pca <- prcomp(train.mat)
plot(dat.pca)

summary(dat.pca)
rots <- dat.pca$rot
dim(rots)


#######################################################
## Rots is a rotation matrix, hency orthonormal
rots2 <- round((t(rots) %*% rots),4)
rots2[4,]
rots2

#######################################################
## Vary the number of principal components to use for rotation
numRotsRots <- 20
train.new <- train.mat %*% rots[,1:numRots]
newtrain.df <- data.frame(ObamaPerc=train.resp.mat,train.new)
mod.lm <- lm(ObamaPerc~.,data=newtrain.df)
test.new <- test.mat  %*% rots[,1:numRots]
newtest.df <- data.frame(test.new)
preds <- predict(mod.lm,newdata=newtest.df)
with(test.df,mean( (ObamaPerc-preds)^2) )


#######################################################
## find the best number of components
mse.rot <- matrix(nrow=ncol(rots))
for(r in 2:ncol(rots)){
    print(r)
    #######################################################
    ##K-Fold here
    #######################################################
    train.new <- train.mat %*% rots[,1:r]
    newtrain.df <- data.frame(ObamaPerc=train.resp.mat,train.new)
    mod.lm <- lm(ObamaPerc~.,data=newtrain.df)
    ##
    test.new <- test.mat  %*% rots[,1:r]
    newtest.df <- data.frame(test.new)
    dim(newtest.df)
    preds <- predict(mod.lm,newdata=newtest.df)
    ##
    mse.rot[r] <- with(test.df,mean( (ObamaPerc-preds)^2) )
}

which.min(mse.rot)
plot(mse.rot,type="l")


#######################################################
## select subset of fields for better pca
#######################################################
ncol(countyElect.df)
nms <- names(countyElect.df)
countyElect.dat <- data.matrix(countyElect.df)
#countyElect.dat <- scale(countyElect.dat)



#######################################################
## Clustering and the Covariance matrix and clustering...
#######################################################
pairs(countyElect.dat[,1:20],cex=.2)

############################################
## compute the covariance matrix
countyElect0.dat <- scale(countyElect.dat[,-52])

cov.dat <- cov(countyElect0.dat)
cov.dat[1,12]

#######################################################
## Singular Value Decompositon
cSVD <- svd(cov.dat)
cSVD <- svd(countyElect0.dat)
summary(countySVD)

U=cSVD$u
V=cSVD$v
D=cSVD$d
dim(U)
dim(V)


pc <- prcomp(countyElect0.dat)
pcRot <- pc$rotation
##these are the same
round(pcRot/V,3)

data.frame(D)%>%
    mutate(n=row_number(),
           totVar=sum(D),
           cumVar=cumsum(D)/totVar)%>%
    ggplot(aes(n,cumVar))+
    geom_point()+geom_step()+
    ggtitle("Cumulative Variance of Principal Component Directions")

#######################################################
## visualizing the covariances
heatmap(cov.dat,Rowv=NA,Colv=NA)

#######################################################
install.packages("d3heatmap")
library(d3heatmap)

d3heatmap(cov.dat,scale="column",colors = "Reds")
d3heatmap(cov.dat,colors = "Reds",Rowv=NA,Colv=NA)


#######################################################
## Another way to cluster...hierarchical clusters (more on this later)
plot(hclust(dist(t(countyElect0.dat)),method="complete"))
plot(hclust(dist(t(countyElect0.dat)),method="average"))
plot(hclust(dist(t(countyElect0.dat)),method="single"))

heatmap(cov.dat,Rowv=NA,Colv=NA)

heatmap(cov.dat)


