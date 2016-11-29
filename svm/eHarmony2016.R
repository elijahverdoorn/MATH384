##use either RMySQL or RODBC to connect to the mysql server
library(ggplot2)
library(e1071)
library(glmnet)
library(dplyr)



features.df <- read.csv("eHarmonyFeatures.csv")
nrow(features.df)
str(features.df)
features.df <- features.df%>%
    select(-id)

matches.df <- read.csv("eHarmonyMatches.csv")
str(matches.df)
matches.df <- matches.df%>%
    select(-id)

persons1 <- with(matches.df,unique(person1))
persons2 <- with(matches.df,unique(person2))

length(persons1)
length(persons2)

nrow(filter(features.df,person %in% persons1))
nrow(filter(features.df,person %in% persons2))

personsBoth <- intersect(persons1,persons2))

tot.sum <- matches.df%>%
    group_by(person1)%>%
    summarize(aveMatch=mean(matched),
              totMatch=n())

head(tot.sum)

with(tot.sum,hist(aveMatch,breaks=50))

features2.df <- filter(features.df,person %in% persons2)%>%
    rename(person2=person)
head(features2.df)

features1.df <- filter(features.df,person %in% persons1)%>%
    rename(person1=person)
head(features1.df)


head(matches.df)
mm.df <- matches.df%>%
    inner_join(features1.df,by="person1")%>%
    inner_join(features2.df,by="person2")
head(mm.df)


cols <- names(features1.df)
cols <- cols[-1]
cols1 <- paste0(cols,".x")
cols2 <- paste0(cols,".y")

mm1 <- data.matrix(mm.df[cols1])
mm2 <- data.matrix(mm.df[cols2])
dim(mm1)
dim(mm2)
mm3 <- abs(mm1-mm2)
dim(mm3)
mm1[1,]
mm2[1,]
mm3[1,]


mm4 <- cbind(mm.df[,1:3],mm3)
dim(mm4)

str(mm4)
mm4 <- mm4[,-c(2,3)]
names(mm4) <- c("matched",cols)
str(mm4)
mm4 <- mm4%>%
    select(-id)
str(mm4)
#######################################################


features.df <- tmp.df
df1 <- merge(matches.df,features.df,by.x="person1",by.y="person")

df1 <- matches.df%>%
    inner_join(features.df,by=c("person1"="person"))

df2 <- df1%>%
    inner_join(features.df,by=c("person2"="person"))

head(df2)
nrow(df2)
df2 <- within(df2,matched <- factor(matched))


#######################################################
############################################
## Get some differences
vals1 <- df2[,5:62]
vals2 <- df2[,64:121]
names(df2)
names(features.df)
offset.x <- 3
offset.y <- 62
names(df2[,fac.cols+offset.x])
names(df2[,fac.cols+offset.y])
vals.fac.x <- df2[,fac.cols+offset.x]
vals.fac.y <- df2[,fac.cols+offset.y]
vals.fac <- ifelse(vals.fac.x==vals.fac.y,1,0)

col.x <- 5:62
length(col.x)
col.y <- 64:121
length(col.y)
fac.cols+offset.x
(cols.o.x <- setdiff(col.x,(fac.cols+offset.x)))
(cols.o.y <- setdiff(col.y,(fac.cols+offset.y)))

vals.num.x <- df2[,cols.o.x]
vals.num.y <- df2[,cols.o.y]
vals.num <- abs(vals.num.x-vals.num.y)

combo.df1 <- data.frame(matched=df2$matched,person1=df2$person1,vals.fac,vals.num)
head(combo.df1)

##combo.df <- data.frame(matched=df2$matched,person1=df2$person1,vals1,vals2)
##names(combo.df)
##
##sample all the rows
## (N <- nrow(combo.df))
## M <- 15000
##
## samp <- sample(1:N,M,rep=F)
## train.df <- combo.df[samp,]
##
## samp2 <- sample(1:N,M,rep=F)
## test.df <- combo.df[samp2,]
##
## dim(train.df)
##
## write.csv(train.df,"train.csv",row.names=F)
## write.csv(test.df,"test.csv",row.names=F)
## ## sample individuals
##

M <- 100
NP <- length(personsMatched)
samp <- sample(1:NP,M,rep=F)
samp0 <- sample(1:NP,M,rep=F)


people.samp <- personsMatched[samp]
train.df <- subset(combo.df1,person1 %in% people.samp)
nrow(train.df)

people.samp <- personsMatched[samp0]
test.df <- subset(combo.df1,person1 %in% people.samp)
nrow(test.df)

####make sure test/train have >1 features and also  same number of features
nc <- ncol(test.df)
nc

cols.x <- c()
for(i in 1:nc){
    len1 <- length(unique(test.df[,i]))
    len2 <- length(unique(train.df[,i]))
    if(len1==1){
        print(c(i,len1))
        cols.x <- c(cols.x,i)
    }
    if(len2==1){
        print(c(i,len2))
        cols.x <- c(cols.x,i)
    }
}
cols.x <- unique(cols.x)
cols.x

##get rid of these
test.df0 <- test.df[,-cols.x]
train.df0 <- train.df[,-cols.x]

#################################
## get rid of factors with different values
nc <- ncol(test.df0)

cols <- matrix(nrow=nc,ncol=2)
for(i in 1:nc){
    len1 <- length(unique(test.df0[,i]))
    len2 <- length(unique(train.df0[,i]))
    cols[i,] <- c(len1,len2)
}
##select features with <=5 values and don't match
sel <- cols[,1]<=2   & cols[,1]!=cols[,2]
col.bad <- (1:nc)[sel]
col.bad

test.df0 <- test.df0[,-col.bad]
train.df0 <- train.df0[,-col.bad]

##lose person
test.df0 <- test.df0[,-2]
train.df0 <- train.df0[,-2]
#######################################################
with(train.df0,table(matched))
wgts <- nrow(train.df0)/with(train.df0,table(matched))
wgts

## Radial
svmfit <-
 svm(matched~.,data=train.df0,kernel="radial",cost=10^0,gamma=.035,scale=T,class.weights=wgts)
##svmfit <-
##  svm(matched~.,data=train.df0,kernel="radial",
##      cost=10^4,gamma=.001,scale=T,class.weights=wgts,
##      decision.values=T)
##
pred.fit <- predict(svmfit,newdata=test.df0)
with(test.df0,table(factor(matched),pred.fit))
with(test.df0,mean(matched!=pred.fit))

pred.fit <- predict(svmfit,newdata=train.df0)
with(train.df0,table(factor(matched),pred.fit))

with(train.df0,mean(matched!=pred.fit))


##not bad...
svmfit <-
  svm(matched~.,data=train.df0,kernel="linear",cost=10^(4),scale=T,class.weights=wgts)

svmfit <-
  svm(matched~.,data=train.df0,kernel="sigmoid",cost=10^(8),scale=T,class.weights=wgts)

svmfit <-
  svm(matched~.,data=train.df0,kernel="polynomial",degree=3,cost=10^(4),scale=T,class.weights=wgts)



#######################################################
svmtune <-
  tune.svm(matched~.,data=train.df0,kernel="radial",cost=10^seq(1,5),gamma=seq(.01,.21,.025),scale=T,class.weights=wgts)


summary(svmtune)
str(svmtune)
g.opt <- svmtune$best.parameter[["gamma"]]
c.opt <- svmtune$best.parameter[["cost"]]
g.opt
c.opt
fit.best <- svmtune$best.model
summary(fit.best)

## Radial
svmfit <-
  svm(matched~.,data=train.df0,kernel="radial",cost=c.opt*100,gamma=g.opt,scale=T,class.weights=wgts)

pred <- predict(svmfit,newdata=test.df0)
with(test.df0,table(factor(matched),pred))


#######################################################
