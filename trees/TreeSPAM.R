#######################################################
## Predict SPAM values. Need train/test data. We will use
## cross-validated tree with pruning
#######################################################

#######################################################
## Read in the  SPAM data set (from UCI)
spam.df <- read.csv("SPAM.csv")
head(spam.df)

#######################################################
## Make sure response is a factor
#######################################################
spam.df <- mutate(spam.df,IsSpam=factor(IsSpam))


#######################################################
## Build  train and test data
#######################################################
nn <- nrow(spam.df)
train <- sample(1:nn,nn/2,rep=F)
spamTrain.df <- spam.df[train,]
spamTest.df <- spam.df[-train,]


#######################################################
## Our first tree...make is sorta deep since we're going to prune it
## later
#######################################################
spam.tree <- tree(IsSpam~.,data=spamTrain.df,
                  control=tree.control(nrow(spamTrain.df),mindev=0.005))
plot(spam.tree)
text(spam.tree,cex=0.5)


#######################################################
## Cross validate to find the optimal size
#######################################################
spam.cv <- cv.tree(spam.tree,FUN=prune.misclass)
plot(spam.cv)
spam.cv

#######################################################
## Dig out the optimal values..need to be careful here to make sure
## getting the optimal value
## I like to reverse the values to make sure we get smallest
#######################################################
sz <- rev(spam.cv$size)
(best.sz <- sz[which.min(rev(spam.cv$dev))])


#######################################################
## Prune the tree and plot it.
#######################################################
spam.prune=prune.misclass(spam.tree,best=best.sz)
plot(spam.prune)
text(spam.prune,cex=0.5)

#######################################################
## Time for predictions on the Test data
preds.prune <- predict(spam.prune,newdata=spamTest.df,type="class")

#######################################################
## This will give you the probability of each class..Use this for an
## ROC curve or threshholding in general
#######################################################
probs.prune <- predict(spam.prune,newdata=spamTest.df,type="vector")
hist(probs.prune[,1],breaks=25)

#######################################################
## How well did we do?
#######################################################
with(spamTest.df,table(preds.prune,IsSpam))
with(spamTest.df,mean(preds.prune!=IsSpam))

#######################################################
## not bad


#######################################################
## On the Training Data...much better
#######################################################
preds.prune <- predict(spam.prune,newdata=spamTrain.df,type="class")
with(spamTrain.df,table(preds.prune,IsSpam))
with(spamTrain.df,mean(preds.prune!=IsSpam))


#######################################################
## How about....
## Bootstrap the data
#######################################################

m.train <- nrow(spamTrain.df)
m.test <- nrow(spamTest.df)
B <- 100
bootVals <- matrix(nrow=m.test,ncol=B)

for(b in 1:B){
    boots <- sample(m.train,m.train,rep=T)
    spamBoot.df <- spamTrain.df[boots,]
    spam.tree <- tree(IsSpam~.,data=spamBoot.df,
                      control=tree.control(nrow(spamBoot.df),mindev=0.005))
    spam.cv <- cv.tree(spam.tree)
    sz <- rev(spam.cv$size)
    bs <- sz[which.min(rev(spam.cv$dev))]
    spam.prune=prune.misclass(spam.tree,best=bs)
    preds.prune <- predict(spam.prune,newdata=spamTest.df,type="class")
    with(spamTest.df,table(IsSpam,preds.prune))
    bootVals[,b] <- as.numeric(as.character(preds.prune))
}

vals <- apply(bootVals,1,mean)
hist(vals)
preds.boot <- ifelse(vals>0.5,1,0)
with(spamTest.df,table(IsSpam,preds.boot))
with(spamTest.df,mean(IsSpam != preds.boot))



#######################################################
## This can be done with the built in library/function randomForest.
## This is slightly better.
#######################################################
library(randomForest)
nc <- ncol(spamTrain.df)-1
spam.bag <- randomForest(IsSpam~.,data=spamTrain.df,mtry=nc,ntree=100)
preds.bag <- predict(spam.bag,newdata=spamTest.df)
with(spamTest.df,table(IsSpam,preds.bag))
with(spamTest.df,mean(IsSpam!=preds.bag))
