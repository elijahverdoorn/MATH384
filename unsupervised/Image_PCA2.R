#######################################################
## Using SVD for Image Compression
#######################################################
library(jpeg)
library(ggplot2)
img.dir <- "~/Dropbox/F16/DataSets/Images"

##imageOrig <- readJPEG(file.path(img.dir,"ElvisNixon.jpg"))
imageOrig <- readJPEG(file.path(img.dir,"kitty.jpg"))

plot(1:2,type='n')
rasterImage(imageOrig, 1,1.2,2,1.2+dd[1]/dd[2])


dd <- dim(imageOrig)
tot=dd[1]*dd[2]
tot

#######################################################
## SVD and get best rank k approximation
s <- svd(imageOrig)

U <- s$u
V <- s$v
D <- s$d
dim(U)
dim(V)


#######################################################
## Best Rank k approximation
#######################################################
dim(imageOrig)

k<-100
U1 <- U[,1:k]
V1 <- V[,1:k]
D1 <- D[1:k]

totK <- nrow(U1)*ncol(U1)+length(D1)+nrow(V1)*ncol(V1)
totK/tot

############################################
## Assemble a
imageComp <- U1 %*% diag(D1,k) %*% t(V1)
dim(imageComp)

##this might move values outside of (0,1). Scale back
xmax <- max(imageComp)
xmin <- min(imageComp)
imageComp <- (imageComp-xmin)/(xmax-xmin)
##
plot(1:2,type='n')
rasterImage(imageComp, 1,1.2,2,1.2+dd[1]/dd[2])


plot(1:2,type='n')
rasterImage(imageOrig, 1,1.2,2,1.2+dd[1]/dd[2])



