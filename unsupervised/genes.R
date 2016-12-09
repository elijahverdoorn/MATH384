# Kinda a monster of a file, but it gives the idea of clustering, especially for heatmaps
# gets some data, then makes some models. The heatmap at the end is especially helpful.
# note that the top few rows are highly clustered
# the data has 20 healthy people and 20 sick people. Goal: classify/cluster the two to find genes corrolated with being sick

library(ggdendro)
library(tidyverse)
library(tree)
library(randomForest)
library(e1071)
library(rvest)
library(dplyr)

genes.df <- read.csv("../datasets/Ch10Ex11.csv", header = F)

model.pca <- prcomp(genes.df)
biplot(model.pca)

PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
    # PC being a prcomp object
    data <- data.frame(obsnames=row.names(PC$x), PC$x)
    plot <- ggplot(data, aes_string(x=x, y=y)) +
        geom_text(alpha=.6, size=3,
                  aes(label=obsnames),
                  color=colors[1])
    ##add axes
    plot <- plot +
        geom_hline(aes(yintercept=0), size=.2) +
        geom_vline(aes(xintercept=0), size=.2,
                   color=colors[2])
    ##rotation data
    datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    ##Scale the data
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
    )
    datapc <- transform(datapc,
                        v1 = .7 * mult * (get(x)),
                        v2 = .7 * mult * (get(y))
    )
    ##Add PC
    plot <- plot +
        coord_equal() +
        geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
    plot <- plot +
        geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),
                     arrow=arrow(length=unit(0.2,"cm")),
                     alpha=0.75, color=colors[4])
    plot
}

# PCbiplot(model.pca) # this doesn't work, no idea why. TODO: fix this

############################################
## Use correlation as the distance metric
############################################
dd <- 1-as.dist(cor(t(genes.df)))
genes.hc.cor <- hclust(dd,method="complete")

genes.dendr.cor <- dendro_data(genes.hc.cor)
genes.dendro.gg2 <- ggplot() +
    geom_segment(data=segment(genes.dendr.cor),
                 aes(x=x, y=y, xend=xend, yend=yend),size=.1) +
    geom_text(data=label(genes.dendr.cor),
              aes(x=x, y=y, label=label, hjust=0), size=3,color="blue") +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0))+
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank())+
    ggtitle("Hierarchical Clustering using PCA")
genes.dendro.gg2

# for the heatmap clusters
my_palette <- colorRampPalette(c("red", "white", "blue"))(n = 20)
my_palette

genes.data <- data.matrix(genes.df)

heatmap(genes.data, col = my_palette, Rowv = NA, Colv = NA)
heatmap(genes.data, col = my_palette)


dist.pear <- function(x) as.dist(1-cor(t(x)))
hclust.ave <- function(x) hclust(x, method="average")
hclust.comp <- function(x) hclust(x, method="complete")

heatmap(genes.data,
        col = my_palette,
        distfun = dist.pear)

heatmap(genes.data,
        col = my_palette)
