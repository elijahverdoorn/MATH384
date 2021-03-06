library(dplyr)
library(tree)
library(ISLR)
library(rpart)
library(randomForest)

# Get the data
wineData.df <- read.csv("winequality-red.csv", sep = ";")
wineData.df <- mutate(wineData.df, quality = factor(quality))
numRows <- nrow(wineData.df) # Reserve Train/Test
trainIndicies <- sample(1:numRows, numRows/2, rep = F)
wineDataTrain.df <- wineData.df[trainIndicies,] 
wineDataTest.df <- wineData.df[-trainIndicies,] 

# build the tree model
wine.tree <- tree(quality~., wineDataTrain.df,
                  control = tree.control(nrow(wineDataTrain.df), mindev=0.001))
plot(wine.tree) # plot it
text(wine.tree) # with the labels

# cross validate the tree
wine.treeCross <- cv.tree(wine.tree)
plot(wine.treeCross)

# get the best size
size <- rev(wine.treeCross$size)
(best.sz <- size[which.min(rev(wine.treeCross$dev))])

# build the optimal model
wine.prune <- prune.tree(wine.tree, best = best.sz)
plot(wine.prune)

preds.test <- predict(wine.prune, newdata = wineDataTest.df, type = "class")
with(wineDataTest.df, table(quality, preds.test))

mse <- mean((preds.test - wineDataTest.df$quality)^2)
mse # not so good
plot(preds.test, wineDataTest.df$quality)
with(wineDataTrain.df, table(quality))
with(wineDataTest.df, table(quality))

# do the optical character part
characterTrain.df <- read.csv("optdigitsTrain.csv") %>% 
    mutate(digit = factor(digit))
characterTest.df <- read.csv("optdigitsTest.csv") %>% 
    mutate(digit = factor(digit))
# build the tree model
character.tree <- tree(digit ~ ., data = characterTrain.df)
plot(character.tree) # plot it
text(character.tree) # with the labels
summary(character.tree)
# cross validate the tree
character.treeCross <- cv.tree(wine.tree)
plot(character.treeCross)

# get the best size
size <- rev(character.treeCross$size)
(best.sz <- size[which.min(rev(character.treeCross$dev))])

# build the optimal model
character.prune <- prune(character.tree, best = best.sz)
plot(character.prune)

preds.test <- predict(character.prune, characterTest.df, type = "class")
table(preds.test)
probs.test <- predict(character.prune, characterTest.df, type = "vector")
hist(probs.test[,1],breaks=25)
with(characterTest.df,table(preds.test,digit))
with(characterTest.df,mean(preds.test!=digit))


# bootstrapping the wine data  
m.train <- nrow(wineDataTrain.df)
m.test <- nrow(wineDataTest.df)
B <- 10
bootVals <- matrix(nrow=m.test,ncol=B)

for(b in 1:B){
    boots <- sample(m.train,m.train,rep=T)
    wineBoot.df <- wineDataTrain.df[boots,]
    wine.tree <- tree(quality~.,data=wineBoot.df,
                      control=tree.control(nrow(wineBoot.df),mindev=0.005))
    wine.cv <- cv.tree(wine.tree)
    sz <- rev(wine.cv$size)
    bs <- sz[which.min(rev(wine.cv$dev))]
    preds.prune <- predict(wine.prune,newdata=wineDataTest.df, type = "class", best = bs)
    bootVals[,b] <- as.numeric(as.character(preds.prune))
}

vals <- apply(bootVals,1,mean)

# bootstrapping the digits data
m.train <- nrow(characterTrain.df)
m.test <- nrow(characterTest.df)
B <- 100
bootVals <- matrix(nrow=m.test,ncol=B)

for(b in 1:B){
    boots <- sample(m.train,m.train,rep=T)
    characterBoot.df <- characterTrain.df[boots,]
    character.tree <- tree(digit~.,data=characterBoot.df,
                      control=tree.control(nrow(characterBoot.df),mindev=0.005))
    character.cv <- cv.tree(character.tree)
    sz <- rev(character.cv$size)
    bs <- sz[which.min(rev(character.cv$dev))]
    character.prune <- prune.misclass(character.tree,best=bs)
    preds.prune <- predict(character.prune,newdata=characterTest.df, type = "class", best = bs)
    bootVals[,b] <- as.numeric(as.character(preds.prune))
}

vals <- apply(bootVals,1,mean)
preds.boot <- as.numeric(vals>0.5)
with(characterTest.df,table(digit,preds.boot))
with(characterTest.df,mean(digit != preds.boot))

# using the random forest
p <- ncol(characterTrain.df) - 1

character.bag <- randomForest(digit ~ .,
                         data = characterTrain.df,
                         mtry = sqrt(p), ## For Random Forest
                         #mtry=p, ##For BAG
                         ntree = 100)
preds.bag <- predict(character.bag, newdata = characterTest.df)
with(characterTest.df, table(digit, character.bag))
with(characterTest.df, mean(digit != character.bag))