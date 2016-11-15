library(tidyverse)
library(gbm)
## Very nice intro to gbm video
#https://vimeo.com/71992876

##############################################
## Boosting SPAM
spam.df <- read.csv("SPAM.csv",header=T)
head(spam.df)

#3 Response must be 0,1 for bernoulli
spam.df <- mutate(spam.df,IsSpam=ifelse(IsSpam=="TRUE",1,0))
with(spam.df,table(IsSpam))
str(spam.df$IsSpam)


##Build  train and test data


nn <- nrow(spam.df)
train <- sample(1:nn,nn/2,rep=F)
spamTrain.df <- spam.df[train,]
spamTest.df <- spam.df[-train,]




##############################
## Here we go....
spam.boost<-gbm(IsSpam ~  . ,
                data=spamTrain.df,
                n.trees=2000,
                distribution="adaboost",
                interaction.depth=4,
                shrinkage=0.05,
                cv.folds=5,
                verbose=T)


##The results
spam.boost
summary(spam.boost)
plot(spam.boost)


best.trees<-gbm.perf(spam.boost)
best.trees


prob.boost<-predict(spam.boost,newdata=spamTest.df,
                    n.trees=best.trees,  repsonse="response")  

pp<-1/(1+exp(-prob.boost))
hist(pp)
pred.boost<- as.numeric(pp>0.5)
with(spamTest.df,table(IsSpam,pred.boost))
with(spamTest.df,mean(IsSpam!=pred.boost))


##Marginal Influence Plots
flds<-names(spam.df)
flds
plot(spam.boost,which(flds=="dollar")-1,type="response")
