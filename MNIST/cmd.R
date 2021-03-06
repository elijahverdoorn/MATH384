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
        print(paste(i))
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
    print(paste("Init Subspace returning matrix with ", nrow(returner), " rows and ", ncol(returner), "columns."))
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
    print(paste("Found ", numUniqueCols, " columns in the initial subspace when constructing the other subspace."))
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
    print(paste("CMD subspace returning matrix with ", nrow(returner), " rows and ", ncol(returner), "columns."))
    return(list("cmdSubspace" = returner, "initSubspace" = initSubspace))
}

# perform the process defined in fig 5
# ARGS: A, a c*m matrix
#       B, a m*n matrix
#       r, a sample size
appr_multiplication <- function(matA, matB, sampleSize) {
    q <- vector(length = nrow(matB)) # the row distribution (I think??? TODO: check this)
    # sum the entrire matB matrix, we only do this once for efficiency
    matrixSum <- 0
    print("point 1")
    # get the sum of the matrix so we only have to do it once
    for (i in 1:ncol(matB)) {
        columnSum <- 0
        for (j in 1:nrow(matB)) {
            element <- matB[j, i] ^ 2
            columnSum <- columnSum + element
        }
        matrixSum <- matrixSum + columnSum
    }
    print("point 2")
    # row distribution of matB
    for (x in 1:nrow(matB)) {
        rowSum <- 0
        for (i in 1:ncol(matB)) {
            rowSum <- rowSum + matB[x,i] ^ 2
        }
        q[x] <- rowSum / matrixSum 
    }
    print("point 3")
    R_d <- matrix(nrow = sampleSize, ncol = ncol(matB))
    C_d <- matrix(nrow = nrow(matA), ncol = sampleSize)
    rowsSampled <- vector(length = sampleSize)
    for (i in 1:sampleSize) {
        j <- sample(1:nrow(matB), size = 1, replace = TRUE, prob = q)
        rowsSampled[i] <- j
        R_d[i,] <- matB[j,] / sqrt(sampleSize * q[j])
        C_d[,i] <- matA[,j] / sqrt(sampleSize * q[j])
    }
    print("point 4")
    uniqueColsC <- t(unique(t(C_d)))
    uniqueRowsR <- unique(R_d)
    R_s <- matrix(nrow = sampleSize, ncol = ncol(matB))
    C_s <- matrix(nrow = nrow(matA), ncol = sampleSize)
    for (i in 1:nrow(uniqueRowsR)) {
        u <- 0 # number of instances of this row in the full set of Rs
        for (j in 1:nrow(R_d)) { # count the instances of this column in the initial subspace
            if (identical(R_d[j,], uniqueRowsR[i,])) {
                u <- u + 1
            }
        }
        R_s[i,] <- u * uniqueRowsR[i,]
        C_s[,i] <- uniqueColsC[,i]
    }
    return(list("C" = C_d, "R" = R_s, "rowsSampled" = rowsSampled)) 
}

# master function, derived from figure 6
cmd_decomposition <- function(matA, c, r) {
    print("Constructing Subspace")
    cSubspace <- cmd_subspace_construction(matA, c)
    
    # extract the init subspace for later use
    initSubspace <- cSubspace$initSubspace
    cSubspace <- cSubspace$cmdSubspace
    
    print("Calculating SVD")
    cSubspace.svd <- svd(t(initSubspace) %*% initSubspace)
    cTranspose <- t(initSubspace)
    print("ApprMult")
    apprMult <- appr_multiplication(cTranspose, matA, r)
    c_s <- apprMult$C
    r_s <- apprMult$R
    print("Calculating U")
    u <- cSubspace.svd$v %*% qr.solve(diag(cSubspace.svd$d) %*% diag(cSubspace.svd$d)) %*% t(cSubspace.svd$v) %*% c_s
    return(list("C" = initSubspace, "U" = u, "R" = apprMult$R, "rows" = apprMult$rowsSampled))
}

# generate random testing matrix
make_test_data <- function (row, col) {
    testMatrix <- matrix(nrow  = row, ncol = col)
    for (i in 1:ncol(testMatrix)) {
        for (j in 1:nrow(testMatrix)) {
            testMatrix[j,i] <- sample(1:20, 1)
        }
        print(paste("Col: ", i))
    }
    return(testMatrix)
}

# this is silently erroring, getting a big matrix with NAs in it
calculate_error <- function(matA, matC, matU, matR) {
    mult <- matC %*% matU %*% matR
    error <- matA - mult
    return(error)
}

run_cmd_test <- function() {
    c <- 50
    r <- 100
    testMatrix <- make_test_data(1000,1000)
    matA <- testMatrix
    decomp <- cmd_decomposition(testMatrix, c, r)
    return(decomp)
}

decomp <- run_cmd_test()

c <- decomp$C
u <- decomp$U
r <- decomp$R

nrow(c)
ncol(c)

nrow(u)
ncol(u)

nrow(r)
ncol(r)
