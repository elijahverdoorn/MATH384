library(ggplot2)
library(e1071)
library(glmnet)
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
  read.csv("/Users/richeym/Dropbox/CLASSES/IntroStatLearn/DATA/BUPA_Liver.csv")

names(liver.df) <- c("mcv","alkphos","sgpt","sgot","gannagt","drinks","class")
head(liver.df)
#######################################################
## Assignment
## Classify using a SVM with linear, polynomial (d=2-5), and radial.
## Summarize  the results via 1) classification error rate and  2)
## ROC. Use Cross-validation in each case.
#######################################################
