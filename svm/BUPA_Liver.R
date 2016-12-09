library(ggplot2)
library(e1071)
library(glmnet)
library(plyr)
library(dplyr)

##
## https://archive.ics.uci.edu/ml/datasets/Liver+Disorders
##7. Attribute information:
##   1. mcv	mean corpuscular volume
##   2. alkphos	alkaline phosphotase
##   3. sgpt	alamine aminotransferase
##   4. sgot 	aspartate aminotransferase
##   5. gammagt	gamma-glutamyl transpeptidase
##   6. drinks	number of half-pint equivalents of alcoholic beverages
##                drunk per day
##   7. selector  field used to split data into two sets
##
liver.df <-
  read.csv("bupaData.csv")

names(liver.df) <- c("mcv","alkphos","sgpt","sgot","gannagt","drinks","class")
head(liver.df)

#######################################################
## Assignment
## Classify using a SVM with linear, polynomial (d=2-5), and radial.
## Summarize  the results via 1) classification error rate and  2)
## ROC. Use Cross-validation in each case.
#######################################################

# Make test/train sets
nrows <- nrow(liver.df)
test <- sample(nrows,nrows/4)
test.df <- liver.df[test,]
train.df <- liver.df[-test,]

nrow(train.df)
head(train.df)

cost.vals <- 10^seq(-3,4,length=30)

# Linear

# models
svm.tuned.linear <- tune.svm(class ~ .,data = liver.df, kernel = "linear", scale = T, cost = cost.vals)
##extract the optimal cost
(cost.opt <- svm.tuned$best.model$cost)

##extract the best model
svmfit.linear.best <- svm.tuned$best.model

fitted <- svmfit.best$decision.values

##ROC curve
M <- 100
rng <- range(fitted)
fp <- rep(0,M)
tp <- rep(0,M)

t.vals <- seq(rng[1],rng[2],length=M)
for(i in 1:M){
  pred <- fitted >= t.vals[i]
  tp[i] <- with(liver.df, sum(class == 2 & pred) / sum(class == 2))
  fp[i] <- with(liver.df, sum(class == 1 & pred) / sum(class == 1))
}

df.roc.linear <- data.frame(tp,fp)

ggplot(df.roc.linear, aes(fp, tp)) + geom_point(color = "red") + geom_step(color = "red")
  
# Polynomial
svm.tuned.poly <- tune.svm(class~.,
                    data=liver.df,
                    kernel="polynomial",
                    d=1,
                    cost=cost.vals)
##extract the optimal cost
(cost.opt <- svm.tuned$best.model$cost)

##extract the best model
svmfit.poly.best <- svm.tuned$best.model

fitted <- svmfit.best$decision.values
##ROC curve
M <- 100
rng <- range(fitted)
fp <- rep(0,M)
tp <- rep(0,M)

t.vals <- seq(rng[1],rng[2],length=M)
for(i in 1:M){
  pred <- fitted >= t.vals[i]
  tp[i] <- with(liver.df, sum(class == 2 & pred) / sum(class == 2))
  fp[i] <- with(liver.df, sum(class == 1 & pred) / sum(class == 1))
}

df.roc.poly <- data.frame(tp,fp)

ggplot(df.roc.poly, aes(fp, tp)) + geom_point(color = "red") + geom_step(color = "red")
  
# Radial
svm.tuned.radial <- tune.svm(class~.,
                    data=liver.df,
                    kernel="radial",
                    d=1,
                    cost=cost.vals)
##extract the optimal cost
(cost.opt <- svm.tuned$best.model$cost)

##extract the best model
svmfit.radial.best <- svm.tuned$best.model

fitted <- svmfit.best$decision.values
##ROC curve
M <- 100
rng <- range(fitted)
fp <- rep(0,M)
tp <- rep(0,M)

t.vals <- seq(rng[1],rng[2],length=M)
for(i in 1:M){
  pred <- fitted >= t.vals[i]
  tp[i] <- with(liver.df, sum(class == 2 & pred) / sum(class == 2))
  fp[i] <- with(liver.df, sum(class == 1 & pred) / sum(class == 1))
}

df.roc.radial <- data.frame(tp,fp)

ggplot(df.roc.radial + df.roc.poly + df.roc.linear, aes(fp, tp)) + geom_point(color = "red") + geom_step(color = "red")
