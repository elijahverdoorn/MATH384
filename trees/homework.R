library(dplyr)
library(tree)
library(ISLR)
library(rpart)

# Get the data
wineData.df <- read.csv("winequality-red.csv", sep = ";")

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

preds.test <- predict(wine.prune, newdata = wineDataTest.df)

mse <- mean((preds.test - wineDataTest.df$quality)^2)
mse # not so good


# do the optical character part
characterTrain.df <- read.csv("optdigitsTrain.csv")
characterTest.df <- read.csv("optdigitsTest.csv")
# build the tree model
character.tree <- rpart(digit ~ ., data = characterTrain.df)
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
character.prune <- prune(character.tree, cp = best.sz)
plot(character.prune)

preds.test <- predict(character.prune, characterTest.df, type = "class")
table(preds.test)
probs.test <- predict(character.prune, characterTest.df, type = "vector")


# bootstrapping part 

m.train <- nrow(wineDataTrain.df)
m.test <- nrow(wineDataTest.df)
B <- 100
bootVals <- matrix(nrow=m.test,ncol=B)

for(b in 1:B){
    boots <- sample(m.train,m.train,rep=T)
    wineBoot.df <- wineDataTrain.df[boots,]
    wine.tree <- tree(quality~.,data=wineBoot.df,
                      control=tree.control(nrow(wineBoot.df),mindev=0.005))
    wine.cv <- cv.tree(wine.tree)
    sz <- rev(wine.cv$size)
    bs <- sz[which.min(rev(wine.cv$dev))]
    preds.prune <- predict(wine.prune,newdata=wineDataTest.df)
    with(wineDataTest.df,table(quality,preds.prune))
    bootVals[,b] <- as.numeric(as.character(preds.prune))
}

vals <- apply(bootVals,1,mean)
mseBoot <- mean((vals - wineDataTest.df$quality)^2)
mseBoot # not so good
