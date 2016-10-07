library(MASS) ##for lda
library(dplyr)
library(ggplot2)

olive.df<-read.csv("OliveOilData.csv")
head(olive.df)
dim(olive.df)
##don't use the 
summary(olive.df)


##take out the Area
with(olive.df,table(Region,Area))
olive.df<-olive.df %>% 
  select(-Area)

with(olive,table(Region))


mod.lda<-lda(Region~.,data=olive.df)
mod.lda
summary(mod.lda)
lda.pred<-predict(mod.lda,olive.df)

preds<-lda.pred$class
olive.df<- olive.df %>% 
  mutate(pred.lda=preds)

##Wow!!
with(olive.df,table(Region,pred.lda))


Wvec<-mod.lda$scaling
head(olive.df)
X<-data.matrix(olive.df[,2:9])
dim(X)
dim(Wvec)

X.trans<-X %*% Wvec
dim(X.trans)

oliveTrans.df<-data.frame(Region=factor(olive.df$Region),
                          LDA1=X.trans[,1],
                          LDA2=X.trans[,2])

#################################################
ggplot(oliveTrans.df,aes(LDA1,LDA2,color=Region))+
  geom_point()+
  scale_color_manual(values=c("red","blue","orange"))
####################################################


####do lda again....
mod.lda2<-lda(Region~LDA1+LDA2,data=oliveTrans.df)
lda.pred2<-predict(mod.lda2,data=oliveTrans.df)
probs<-lda.pred2$posterior


## make a plotting grid in x and y
GS <- 60
x.vals0<-with(oliveTrans.df,seq(min(LDA1),max(LDA1),length=GS))
y.vals0<-with(oliveTrans.df,seq(min(LDA2),max(LDA2),length=GS))
grid.df <- expand.grid(x.vals0,y.vals0)
names(grid.df) <- c("LDA1","LDA2")

###make predictions for grid xy vals
grid.lda <- predict(mod.lda2,grid.df)
grid.df <- within(grid.df,cat.lda <- grid.lda$class)

##plot the grid values versus the original values
grid.gg <- ggplot()+
  geom_point(data=oliveTrans.df,aes(LDA1,LDA2,color=Region),size=2)+
  geom_point(data=grid.df,aes(LDA1,LDA2,color=cat.lda),size=1,shape=9,alpha=0.6)+
  ##  stat_contour(data=grid.df,aes(x,y,z=probs),breaks=c(0.5))+
  scale_color_brewer(palette="Set1")+
  ggtitle("LDA")
grid.gg
