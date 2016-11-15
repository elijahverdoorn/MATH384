#######################################################
## Fitting Regression Trees
library(dplyr)
library(tree)
library(ISLR)


#######################################################
## Use the Carseats dataset to get loose
#######################################################
names(Carseats)

#######################################################
## focus on three variables
Carseats0 <- Carseats[c("Sales","Income","Price")]


#######################################################
## Predict Sales from Income and Price
#######################################################
car.tree=tree(Sales~Income+Price,Carseats0)

#######################################################
## See what we have
#######################################################
car.tree
plot(car.tree)
text(car.tree,cex=0.9)

#######################################################
## the tree stops when the mindev of the node is 1% of the original
## We can grow it further by changing this value
## This results in a deeper tree
#######################################################
car.tree=tree(Sales~Income+Price,Carseats0,
              control=tree.control(nrow(Carseats0),mindev=.001))

#######################################################
## Take a look at this tree
#######################################################
car.tree
plot(car.tree)
text(car.tree,cex=0.9)


#######################################################
## Use CV to prune it back to a more stable size
#######################################################
car.cv <- cv.tree(car.tree)
plot(car.cv)
car.cv
#######################################################
## Dig out the optimal values..need to be careful here to make sure
## getting the optimal value
## I like to reverse the values to make sure we get smallest
#######################################################
sz <- rev(car.cv$size)
(best.sz <- sz[which.min(rev(car.cv$dev))])


car.tree0 <- prune.tree(car.tree,best=best.sz)
plot(car.tree0)
text(car.tree0,cex=0.9)


#######################################################
### Let's build this ourselves by hand
#######################################################
## Here's
car.tree=tree(Sales~Income+Price,Carseats0,
              control=tree.control(nrow(Carseats0),mindev=.005))

plot(car.tree)
text(car.tree,cex=0.9)


#######################################################
## Walk through process of finding the splits in the tree
#######################################################

## Need these values
inc.vals <- with(Carseats0,sort(Income))
pri.vals <- with(Carseats0,sort(Price))

############################################
## First min mse for Price
## Run though all possible values for Price, at each
## Price, split into <Price and >=Price. For each group, compute mean
## and RSS. Add to get the total RSS. Select the Price that minimizes
## the total RSS
############################################
mseVals <- matrix(ncol=length(pri.vals))
i <- 1
for(val in pri.vals){
    mseVals[i] <-Carseats0%>%
        mutate(class=Price<val)%>%
        group_by(class)%>%
        mutate(Sales0=mean(Sales))%>%
        summarize(tot=sum( (Sales-Sales0)^2))%>%
        with(sum(tot))
    i <- i+1
}
min(mseVals)
pri.vals[which.min(mseVals)]

############################################
## min mse for Income
############################################
mseVals <- matrix(ncol=length(inc.vals))
i <- 1
for(val in inc.vals){
    mseVals[i] <-Carseats0%>%
        mutate(class=Income<val)%>%
        group_by(class)%>%
        mutate(Sales0=mean(Sales))%>%
        summarize(tot=sum( (Sales-Sales0)^2))%>%
        with(sum(tot))
    i <- i+1
}

#######################################################
## Look at he
plot(pri.vals,mseVals,type="l")

#######################################################
## Pick out the min and the value of Price that produces the min
min(mseVals)
pri.vals[which.min(mseVals)]

#######################################################
## Make a function which does this for any data and variable
## Needs data.df and var
#######################################################
findMinMSE <- function(data.df){
    var.vals <- with(data.df,sort(var))
    mseVals <- matrix(ncol=length(var.vals))
    i <- 1
    for(val in var.vals){
    mseVals[i] <-data.df%>%
        mutate(class=var<val)%>%
        group_by(class)%>%
        mutate(Sales0=mean(Sales))%>%
        summarize(tot=sum( (Sales-Sales0)^2))%>%
        with(sum(tot))
    i <- i+1
    }
    c(min(mseVals),var.vals[which.min(mseVals)])
}

#######################################################
## Repeat the earlier calculation. Note we get the same split value
## for Price
#######################################################
data.df <-mutate(Carseats0,var=Price)
findMinMSE(data.df)


#######################################################
## Same for Income
#######################################################
data.df <-mutate(Carseats0,var=Income)
findMinMSE(data.df)

##use the Price for the first split
price1 <- 95


############################################
## One level deeper in the tree
## Left Branch
############################################
data.df <- Carseats0%>%
    filter(Price<price1)%>%
    mutate(var=Price)
findMinMSE(data.df)

data.df <- Carseats0%>%
    filter(Price<=price1)%>%
    mutate(var=Income)
findMinMSE(data.df)

##use price again
price21 <- 77

############################################
## Right Branch
############################################
data.df <- Carseats0%>%
    filter(Price>=price1)%>%
    mutate(var=Price)
findMinMSE(data.df)


data.df <- Carseats0%>%
    filter(Price>=price1)%>%
    mutate(var=Income)
findMinMSE(data.df)

############################################
## Price is best again
############################################
price22 <- 137


############################################
## Third level
## Right then Left
############################################
data.df <- Carseats0%>%
    filter(Price>=price1,
           Price<price22)%>%
    mutate(var=Price)
findMinMSE(data.df)

data.df <- Carseats0%>%
    filter(Price>=price1,
           Price<price22)%>%
    mutate(var=Income)
findMinMSE(data.df)

## Use income this time
income31 <- 61

#################################
## Predicted Values are just the means of the termimal nodes
#################################
filter(Carseats0,
       Price>=price1,
       Price<price22,
       Income<income31)%>%
    with(mean(Sales))

############################################
## Third level
## Right then Right
##
############################################
data.df <- Carseats0%>%
    filter(Price>=price1,
           Price>=price22)%>%
    mutate(var=Price)
findMinMSE(data.df)

data.df <- Carseats0%>%
    filter(Price>=price1,
           Price>=price22)%>%
    mutate(var=Income)
findMinMSE(data.df)

#################################
## We can see income split on the graph
##################################


