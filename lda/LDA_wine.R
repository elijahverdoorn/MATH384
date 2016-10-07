library(MASS)
library(ggplot2)
library(class)

##Wine classification.



wine.df <- read.csv("wine.csv")
with(wine.df,table(Class))
summary(wine.df)
wine.df <- within(wine.df,Class <- factor(Class))

ggplot(wine.df,aes(Alcohol,Ashm,color=Class))+
  geom_point()+
  scale_color_brewer(palette="Set1")


#######################################################
N <- nrow(wine.df)
N
train <- sample(1:N,N/2,rep=F)
train.df <- wine.df[train,]
test.df <- wine.df[-train,]

wine.lda <- lda(Class~Alcohol+Ashm+Proline,data=train.df)

train.pred <- predict(wine.lda,train.df)
preds <- train.pred$class
train.df <- within(train.df,Class.pred <- preds)
with(train.df,table(Class,Class.pred))

test.pred <- predict(wine.lda,data=test.df)
preds <- test.pred$class
test.df <- within(test.df,Class.pred <- preds)
with(test.df,table(Class,Class.pred))
print(predict)
#######################################################
projs <- wine.lda$scaling

vals <- as.matrix(train.df[,c("Alcohol","Ashm","Proline")]) %*%
  projs
train.df <- within(train.df,LDA1 <- vals[,1])
train.df <- within(train.df,LDA2 <- vals[,2])
ggplot(train.df,aes(LDA1,LDA2,color=Class))+geom_point()#+geom_density2d()
with(train.df,table(Class,Class.pred))


vals <- as.matrix(test.df[,c("Alcohol","Ashm","Proline")]) %*%
  projs
test.df <- within(test.df,LDA1 <- vals[,1])
test.df <- within(test.df,LDA2 <- vals[,2])
ggplot(test.df,aes(LDA1,LDA2,color=Class))+geom_point()#+geom_density2d()
with(test.df,table(Class,Class.pred))
