library(dplyr)
# performs the process listed in Fig. 2
# mat is the data matrix
# c is the sample size
initial_subspace_construction <- function(mat, c) {
    columnDistribution <- vector(length = ncol(mat))
    matrixSum <- 0
    # sum the entrire matrix, we only do this once for efficiency
    for (i in 1:ncol(mat)) {
        columnSum <- 0
        for (j in 1:nrow(mat)) {
            element <- mat[j, i] ^ 2
            columnSum <- columnSum + element
        }
        matrixSum <- matrixSum + columnSum
    }
    # calculate the distribution for each column
    for (i in 1:ncol(mat)) {
        columnSum <- 0
        for (j in 1:nrow(mat)) {
            element <- mat[j,i] ^ 2
            columnSum <- columnSum + element
        }
        columnDistribution[i] <- columnSum / matrixSum
    }
    returner <- matrix(nrow = nrow(mat), ncol = c) # the matrix that we'll return, of dimension m x c
    # perform biased sampling
    for (i in 1:c) {
        j <- sample(1:ncol(mat), size = 1, replace = TRUE, prob = columnDistribution)
        returner[,i] <- mat[,j] / sqrt(c * columnDistribution[j])
    }
    return(returner)
}

# perfoms the process defined in figure 4
# args: mat, the data matrix
#       c, the sample size
cmd_subspace_construction <- function(mat, c) {
    initSubspace <- initial_subspace_construction(mat, c)
    initSubspace <- apply(initSubspace, c(1,2), round, digits = 5) # round the initSubspace so that unique() will work 
    uniqueCols <- t(unique(t(initSubspace)))
    numUniqueCols <- ncol(uniqueCols)
    returner <- matrix(nrow = nrow(initSubspace), ncol = numUniqueCols)
    for (i in 1:numUniqueCols) {
        numInstancesInInitSubspace <- 0
        for (j in 1:ncol(initSubspace)) { # count the instances of this column in the initial subspace
            if (identical(initSubspace[,j], uniqueCols[,i])) {
              numInstancesInInitSubspace <- numInstancesInInitSubspace + 1
            }
        }
        returner[,i] <- sqrt(numInstancesInInitSubspace) * t(uniqueCols[,i])
    }
    return(initSubspace)
}

# generate random testing matrix
make_test_data <- function (row, col) {
    testMatrix <- matrix(nrow  = row, ncol = col)
    for (i in 1:ncol(testMatrix)) {
        for (j in 1:nrow(testMatrix)) {
            testMatrix[j,i] <- sample(1:20, 1)
        }
    }
    return(testMatrix)
}

c <- 6
mat <- make_test_data(6,10)
testMatrix <- make_test_data(6, 6)
subspace <- cmd_subspace_construction(testMatrix, 3)
