library(rvest)
library(dplyr)

## get the data

url <- "http://www.basketball-reference.com/leagues/NBA_2016_advanced.html"
sel <- ".left , .right, .center, #advanced_stats_clone a"

dat <- url %>%
    read_html() %>%
    html_nodes(sel)%>%
    html_text()
dat

#######################################################
## there are 27 fields
head(dat,27)
##and 463 players
length(dat)/27

#######################################################
## Put into correct shaep
nba.dat <- matrix(dat,byrow=T,ncol=27)


############################################
## Take out first five colums
flds <- nba.dat[1,]
nba.df0 <- data.frame(nba.dat[-1,1:5])
names(nba.df0) <- flds[1:5]

#######################################################
## Here is all the data...convert to numeric
nba.df1 <- nba.dat[-1,6:27]

nba.df1 <-apply(nba.df1,2,
                function(x)as.numeric(as.character(x)))%>%
    scale()%>%
    data.frame()

names(nba.df1) <- flds[6:27]

dim(nba.df0)
dim(nba.df1)
############################################
## Put back together
str(nba.df0)
str(nba.df1)
nba.df <-bind_cols(nba.df0,nba.df1)

nba.df <- mutate(nba.df,Age=as.numeric(as.character(Age)),
                 Rk=as.numeric(as.character(Rk)))
str(nba.df)
nrow(nba.df)

#######################################################
## Use Advanced Stats
## Set Basketball-reference.com: NBA/Season/Advanced
## Run Rvest_NBA.R
## This starts the analysis.
names(nba.df)
head(nba.df)
str(nba.df)
#######################################################
cc <- complete.cases(nba.df)
sum(!cc)
nba.df <- nba.df[cc,]
nrow(nba.df)

## take care of players with split seasons
split.players <- droplevels(with(nba.df,Player[Tm=="TOT"]))
split.players

nba.df <- nba.df%>%
    mutate(isSplit=Player %in% split.players)

nba.df <- nba.df%>%
    filter(!isSplit | (isSplit & Tm=="TOT"))%>%
    select(-isSplit)

##nba.df <- nba.df[,-ncol(nba.df)]
nrow(nba.df)


### this seemed like a good idea at the time
players.df <- nba.df%>%
    group_by(Player)%>%
    summarize(ID=min(Rk),
              Age=min(Age))


#######################################################
## Group Positions
nba.df <- droplevels(nba.df)
with(nba.df,table(Pos))


############################################
## Subset the data
## Maybe only look at shooting guards? (SG)
############################################
###take top % or so of players my minutes played
minsq <- with(nba.df,quantile(MP,0.85))
minsq

nba.df0 <- subset(nba.df,MP>minsq)
nrow(nba.df0)

##extract player names
(players <- nba.df0$Player)

#######################################################
## Principal Components
nba.df3 <-  nba.df0[,8:27]
names(nba.df3)

keepFlds <- c(1:6,8:14)
nba.df3 <- nba.df3[,keepFlds]
row.names(nba.df3) <- players

nba.pca <- prcomp(nba.df3)
biplot(nba.pca)

#######################################################
## A better bioplot (taken from stackexchange)
############################################
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

PCbiplot(nba.pca)

#######################################################
rots <- nba.pca$rotation
data.matrix(nba.df3)%*% rots[,1]
pca13 <- data.matrix(nba.df3) %*% rots[,1:3]
pca1 <- pca13[,1]
pca2 <- pca13[,2]
pca3 <- pca13[,3]



#######################################################
## K Means clustering
#######################################################
K <- 10
##run 30 times, allow up to 30  iterations for convergence
nba.km <- kmeans(nba.df3,K,iter.max=20,nstart=30)

##the clusters
nba.df0$cluster.km<- nba.km$cluster
nba.km$cluster
filter(nba.df0,cluster.km==1)

with(nba.df0,table(cluster.km))
head(nba.df0)


nbaCluster.df <- data.frame(pca1,pca2,cluster.km=factor(nba.df0$cluster.km),name=players)

##plot the cluster.pcas on top of the PCA first two components
ggplot(nbaCluster.df,aes(pca1,pca2,color=cluster.km))+
    geom_point(size=1)+
    geom_text(aes(label=name),size=3)


#######################################################
## Add cluster colors
PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
    # PC being a prcomp object
    data <- data.frame(obsnames=row.names(PC$x), PC$x)
    plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.7,
                                                           size=4,
                                                           aes(label=obsnames),
                                                           color=nba.km$cluster)
    plot <- plot +
        geom_hline(aes(yintercept=0), size=.2) +
        geom_vline(aes(xintercept=0), size=.2, color=colors[2])
    datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
    )
    datapc <- transform(datapc,
                        v1 = .7 * mult * (get(x)),
                        v2 = .7 * mult * (get(y))
    )
    plot <- plot + coord_equal() +
        geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
    plot <- plot +
        geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),
                     #                     arrow=arrow(length=unit(0.2,"cm")),
                     alpha=0.75, color=colors[4])
    plot
}

PCbiplot(nba.pca)
#######################################################

#######################################################
## Some hierarchical clustering
row.names(nba.df3) <- players

nba.hc.c <- hclust(dist(nba.df3),method="complete")
nba.hc.a <- hclust(dist(nba.df3),method="average")
nba.hc.s <- hclust(dist(nba.df3),method="single")


##require(graphics)
plot(nba.hc.c)
plot(nba.hc.a)
plot(nba.hc.s)

ggdendrogram(nba.hc.c,rotate=T)
ggdendrogram(nba.hc.a,rotate=T)
ggdendrogram(nba.hc.s,rot=T)

############################################
## We can do better visuals with ggplot
## ggdendro library
############################################
#install.packages("ggdendro")
library(ggdendro)

##Extract the gg data
nba.dendr <- dendro_data(nba.hc.c)
#3nba.dendr <- dendro_data(nba.hc.c, type="triangle")

#######################################################
## Build the plot
nba.dendro.gg <-
    ggplot() +
    geom_segment(data=segment(nba.dendr),
                 aes(x=x, y=y, xend=xend, yend=yend),size=.1) +
    geom_text(data=label(nba.dendr),
              aes(x=x, y=y, label=label, hjust=0),
              size=3,color="blue") +
    coord_flip() +
    scale_y_reverse(expand=c(0.2, 0))+
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank())+
    ggtitle("Hierarchical Clustering")
nba.dendro.gg


##enhance visuals...identify clusters
#################################
###  cluster cuts
###################################
Cut.lev <- 6
cut.labs <- cutree(nba.hc.c,Cut.lev)

##create data frame to match to player names
cut.df <- data.frame(Player=names(cut.labs),
                     cluster=cut.labs)
#######################################################
## add in cluster information
labels.df <- label(nba.dendr)%>%
    inner_join(cut.df,by=c("label"="Player"))

##Replot with clusters labeled
nba.dendro.gg1 <- nba.dendro.gg +
    geom_text(data=labels.df,
              aes(x=x, y=y, label=label,
                  hjust=0,
                  color=factor(cluster)),
              size=3)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Hierarchical Clustering")
nba.dendro.gg1


############################################
## Use correlation as the distance metric
############################################
dd <- 1-as.dist(cor(t(nba.df3)))
nba.hc.cor <- hclust(dd,method="complete")

nba.dendr.cor <- dendro_data(nba.hc.cor)
##nba.dendr <- dendro_data(nba.hc.c, type="triangle")

#######################################################
nba.dendro.gg2 <- ggplot() +
    geom_segment(data=segment(nba.dendr.cor),
                 aes(x=x, y=y, xend=xend, yend=yend),size=.1) +
    geom_text(data=label(nba.dendr.cor),
              aes(x=x, y=y, label=label, hjust=0), size=3,color="blue") +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0))+
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank())+
    ggtitle("Hierarchical Clustering using PCA")
nba.dendro.gg2

#################################
###cluster cuts
#######################################################
Cut.lev <- 6
cut.labs <- cutree(nba.hc.cor,Cut.lev)
cut.df <- data.frame(Player=names(cut.labs),
                     cluster=cut.labs)

## add in cluster information
labels.df <- label(nba.dendr.cor)%>%
    inner_join(cut.df,by=c("label"="Player"))
head(labels.df)

##Replot with clusters labeled
nba.dendro.gg2 <- nba.dendro.gg2 +
    geom_text(data=labels.df,
              aes(x=x, y=y, label=label,
                  hjust=0,
                  color=factor(cluster)),
              size=3)+
    scale_color_brewer(palette="Set1")+
    ggtitle("Hierarchical Clustering (Correlation)")
nba.dendro.gg2

nba.dendro.gg1


##################################################################
## Heatmaps
## This allows us to see clustering in two dimensions
##http://www.r-bloggers.com/drawing-heatmaps-in-r/
#######################################################
#######################################################
## Heatmap without clustering/
## each grid value is the player x stat value (scaled)
############################################
my_palette <- colorRampPalette(c("red", "white", "blue"))(n = 20)
my_palette


nba.data <- data.matrix(nba.df3)


heatmap(nba.data,col=my_palette,Rowv=NA,Colv=NA)

#######################################################
## data clustering
#######################################################

heatmap(nba.data,col=my_palette)


dist.pear <- function(x) as.dist(1-cor(t(x)))
hclust.ave <- function(x) hclust(x, method="average")
hclust.comp <- function(x) hclust(x, method="complete")


heatmap(nba.data,
        col=my_palette,
        distfun=dist.pear)

heatmap(nba.data,
        col=my_palette)