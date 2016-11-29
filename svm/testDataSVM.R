dat.df <- read.csv("arrhythmia.data")
dim(dat.df)
names(dat.df)[280] <- "class"
with(dat.df,table(class))

#######################################################
dat.df <- dat.df%>%
    mutate(class=factor(ifelse(class==1,"A","B")))

#######################################################
dat.df <- dat.df%>%
    mutate(class=factor(ifelse(class==1,"A",
                        ifelse(class %in% c(9,10),"B","C"))))

#######################################################
dat.df <- dat.df%>%
    mutate(class=factor(class))


with(dat.df,table(class))


flds <- paste0("c",1:279)
names(dat.df) <- c(flds,"class")


#######################################################
## eliminate zero variance fields
dd <- scale(data.matrix(dat.df[,-280]))
vars <- apply(dd,2,var)
var0 <- is.na(vars)
dat.df <- dat.df[,!var0]

#######################################################
## test and train
nn <- nrow(dat.df)
train <- sample(nn,nn/2)
train.df <- dat.df[train,]
test.df <- dat.df[-train,]

nrow(train.df)
head(train.df)

#######################################################
## Try to fit the best possible modell
with(train.df,table(class))
svm.fit <- svm(class~.,
               data=train.df,
               kernel="polynomial",
               d=1,
               cost=.10,


pred.svm <- predict(svm.fit,newdata=train.df)
with(train.df,table(class,pred.svm))
with(train.df,mean(class!=pred.svm))

pred.svm <- predict(svm.fit,newdata=test.df)
with(test.df,table(class,pred.svm))
with(test.df,mean(class!=pred.svm))

tune.out <- tune(svm,class~.,data=train.df,kernel="polynomial",scale=F,
                 range=list(d=seq(1,5),cost=10^seq(-2,0,length=6)),
                 tunecontrol=tune.control(cross=3))

summary(tune.out)

#######################################################
svm.fit <- svm(class~.,
               data=train.df,
               kernel="radial",
               gamma=.15,
               cost=.545,
               scale=F)



pred.svm <- predict(svm.fit,newdata=train.df)
with(train.df,table(class,pred.svm))
with(train.df,mean(class!=pred.svm))

pred.svm <- predict(svm.fit,newdata=test.df)
with(test.df,table(class,pred.svm))
with(test.df,mean(class!=pred.svm))


tune.out <- tune(svm,class~.,data=train.df,kernel="radial",scale=F,
                 range=list(gamma=10^seq(-2,2),cost=10^seq(-2,3)),
                 tunecontrol=tune.control(cross=3))


summary(tune.out)



