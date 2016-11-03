library(dplyr)
library(glmnet)
library(ggplot2)
library(glmnet)
library(MASS)
library(class)

# Cost Matrix
costFalsePositive <- 5
costFalseNegative <- 1
costTrueNegative <- 0
costTruePositive <- 0

germanCreditNumeric.df <- read.csv("germanNumeric.csv", header = T) # read the numeric data
germanCredit.df <- read.csv("german.csv", header = F, sep = "") # read the original data
names(germanCredit.df) <- c("chk_ac_status_1",
                            "duration_month_2", "credit_history_3", "purpose_4",
                            "credit_amount_5","savings_ac_bond_6","p_employment_since_7",
                            "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10",
                            "present_residence_since_11","property_type_12","age_in_yrs_13",
                            "other_instalment_type_14", "housing_type_15",
                            "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                            "telephone_19", "foreign_worker_20",
                            "good_bad_21")

# the data comes with the response variable as a 1 or 2, I want T or F
germanCredit.df <- germanCredit.df %>%
  mutate(good_bad_21 = ifelse(germanCredit.df["good_bad_21"] == 1, F, T))


germanCreditNumeric.df <- germanCreditNumeric.df %>% # add the response variable to the numeric data
    mutate(response = germanCredit.df[,21])

# get the data into the form that we want it in

# there's almost certainly a better way to do this, but for now this'll work
numericTest.df <- germanCreditNumeric.df[1:50,] 
numericTrain.df <- germanCreditNumeric.df[51:1000,]

numericTest.mat <- data.matrix(numericTest.df)
numericTrain.mat <- data.matrix(numericTrain.df)

germanTest.df <- germanCredit.df[1:50,]
germanTrain.df <- germanCredit.df[51:1000,]

# Lasso
# TODO: use lasso to select variables, then feed those variables to LDA and logistic regression

# build the matrices of variables in the format that glmnet wants
xNumericTrain.mat <- data.matrix(numericTrain.df[,1:49])
yNumericTrain.mat <- data.matrix(numericTrain.df[,50])
xNumericTest.mat <- data.matrix(numericTest.df[,1:49])
yNumericTest.mat <- data.matrix(numericTest.df[,50])

# want a "grid" of lambda vals so that we can pick an ideal lambda
numLambda <- 100
expVals <- seq(-4, 4, length = numLambda)
lambdaGrid <- 10^expVals
plot(expVals, lambdaGrid) # make sure that it looks exponential

lassoModel <- glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 1, lambda = lambdaGrid, intercept = TRUE) # use the lambdas that we just made to find the ideal value
coef(lassoModel) # look at the coefficients of the model
plot(lassoModel) # see what the model looks like

# get the optimal lambda
crossValidatedLassoModel <- cv.glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 1, lambda = lambdaGrid, intercept = TRUE) # cross validate

(lambda0 <- crossValidatedLassoModel$lambda.min)
(lambda1 <- crossValidatedLassoModel$lambda.1se)
mod.lasso <- glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 1, lambda = lambda0, intercept = TRUE) # use the optimal lambda

mod.lassoConservative <- glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 1, lambda = lambda1, intercept = TRUE) # a more conservative estimate, for comparison

coef.lasso <- coef(mod.lasso)
coef.lassoConservative <- coef(mod.lassoConservative)

## And finally, the mse
pred.lasso <- predict(mod.lasso, newx = xNumericTest.mat)
pred.lassoConservative <- predict(mod.lassoConservative, newx = xNumericTest.mat)
mse.lasso <- with(numericTest.df, mean((pred.lasso - yNumericTest.mat)^2))
mse.lassoConservative <- with(numericTest.df, mean((pred.lassoConservative - yNumericTest.mat)^2))

## Comparison of errors
c(mse.lasso, mse.lassoConservative)

# Ridge Regression
# build the matrices of variables in the format that glmnet wants
xNumericTrain.mat <- data.matrix(numericTrain.df[,1:49])
yNumericTrain.mat <- data.matrix(numericTrain.df[,50])
xNumericTest.mat <- data.matrix(numericTest.df[,1:49])
yNumericTest.mat <- data.matrix(numericTest.df[,50])

# want a "grid" of lambda vals so that we can pick an ideal lambda
numLambda <- 100
expVals <- seq(-4, 4, length = numLambda)
lambdaGrid <- 10^expVals
plot(expVals, lambdaGrid) # make sure that it looks exponential

ridgeModel <- glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 0, lambda = lambdaGrid, intercept = TRUE) # use the lambdas that we just made to find the ideal value
coef(ridgeModel) # look at the coefficients of the model
plot(ridgeModel) # see what the model looks like

# get the optimal lambda
crossValidatedRidgeModel <- cv.glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 0, lambda = lambdaGrid, intercept = TRUE) # cross validate

(lambda0 <- crossValidatedRidgeModel$lambda.min)
(lambda1 <- crossValidatedRidgeModel$lambda.1se)
mod.ridge <- glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 0, lambda = lambda0, intercept = TRUE) # use the optimal lambda

mod.ridgeConservative <- glmnet(xNumericTrain.mat, yNumericTrain.mat, alpha = 0, lambda = lambda1, intercept = TRUE) # a more conservative estimate, for comparison

coef.ridge <- coef(mod.ridge)
coef.ridgeConservative <- coef(mod.ridgeConservative)

## And finally, the mse
pred.ridge <- predict(mod.ridge, newx = xNumericTest.mat)
pred.ridgeConservative <- predict(mod.ridgeConservative, newx = xNumericTest.mat)
mse.ridge <- with(numericTest.df, mean((pred.ridge - yNumericTest.mat)^2))
mse.ridgeConservative <- with(numericTest.df, mean((pred.ridgeConservative - yNumericTest.mat)^2))

## Comparison of errors
c(mse.ridge, mse.ridgeConservative)

# Linear Discrim. Analysis
coefRidge <- data.frame(data.matrix(coef(mod.ridge))) 
coefLasso <- data.frame(data.matrix(coef(mod.lasso))) # get the coefs into a data frame so that we can pick variables

# using the following variables since they work well for the other models:
# property_type_12A124
# credit_history_3A31
# purpose_4A46
# other_debtors_or_grantors_10A102

mod.lda <- lda(response ~ property_type_12A124 + credit_history_3A31 + purpose_4A46 + other_debtors_or_grantors_10A102, data = numericTrain.df)




# Logistic Regression
logisticRegrssionModel <- glm(response ~ property_type_12A124 + credit_history_3A31 + purpose_4A46 + other_debtors_or_grantors_10A102, data = numericTrain.df)
summary(logisticRegrssionModel)
logisticTesting.df <- numericTest.df
logisticRegressionProb <- predict(logisticRegrssionModel, newdata = logisticTesting.df)
maxThresh = 100
bestLogisitcValue <- 99999999
for (i in 1:maxThresh) {
  threshold <- i / maxThresh # so that it's a decimal
  logisticBinaryPrediction <- ifelse(logisticRegressionProb > threshold, TRUE, FALSE)
  
  with(logisticTesting.df, table(response, logisticBinaryPrediction)) # table of the prediction vs. actual
  
  truePositives <- sum(logisticTesting.df$response == T)
  falsePositves <- sum(logisticTesting.df$response == F & logisticBinaryPrediction == T)
  trueNegatives <- sum(logisticTesting.df$response == F)
  falseNegatives <- sum(logisticTesting.df$response == T & logisticBinaryPrediction == F)
  correctPositives <- sum(logisticTesting.df$response == T & logisticBinaryPrediction == T)
  
  totalValueLogistic <- (falseNegatives * costFalseNegative) + (falsePositves * costFalsePositive) - (correctPositives * costTruePositive)
  if(totalValueLogistic < bestLogisitcValue) { # if the current value is the best one, save it. if not, try the next thresh val
    bestLogisitcValue <- totalValueLogistic
    bestThreshValue <- threshold
  }
}





