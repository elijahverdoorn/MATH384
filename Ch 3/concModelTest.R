library(ggplot2)
library(dplyr)
library(tidyr)
library(FNN)
library(ISLR)

concrete.df<-read.csv("concrete.csv")
names(concrete.df)
concrete.df <- concrete.df %>% 
  select(-SLUMP,-FLOW,-No)
pairs(concrete.df)
with(concrete.df,plot(Cement,Strength))
with(concrete.df,plot(Slag,Strength))

with(concrete.df,hist(Slag,breaks=25))
concrete.df<-mutate(concrete.df,LowSlag=Slag==0)

ggplot(concrete.df,aes(Cement,Strength,color=LowSlag))+
  geom_point()

mod1<-lm(Strength~Cement+LowSlag,data=concrete.df)

summary(mod1)

pred<-predict(mod1)
concrete.df$pred<-pred

ggplot(concrete.df,aes(Cement,Strength,color=LowSlag))+
  geom_point()+
  geom_line(aes(Cement,pred))+
  ggtitle("Concrete: Linear Model")

mod2<-lm(Strength~Cement+LowSlag+Cement:LowSlag,data=concrete.df)
summary(mod2)

concrete.df$pred<-predict(mod2)
ggplot(concrete.df,aes(Cement,Strength,color=LowSlag))+
  geom_point()+
  geom_line(aes(Cement,pred))

ggplot(concrete.df,aes(Cement,Strength,color=LowSlag))+
  geom_point()+
  geom_smooth(method=lm,se=F)

mod0<-lm(Strength~Cement,data=concrete.df)
concrete.df$pred<-predict(mod0)
summary(mod0)

# Feature expansion

mod2<-lm(Strength~Cement+I(Cement^2),data=concrete.df)
concrete.df$pred<-predict(mod2)

concrete.df<-concrete.df %>% 
  mutate(Cement2=Cement^2)
mod2x<-lm(Strength~Cement+Cement2,data=concrete.df)

mod3<-lm(Strength~Cement+I(Cement^2)+I(Cement^3),data=concrete.df)
summary(mod3)
concrete.df$pred<-predict(mod3)

mod4<-lm(Strength~Cement+I(Cement^2)+I(Cement^3)+I(Cement^4),
         data=concrete.df)
summary(mod4)

concrete.df$pred<-predict(mod4)
ggplot(concrete.df,aes(Cement,Strength))+
  geom_point()+
  geom_line(aes(Cement,pred))  

maxDeg<-4

formStr<-"Strength~Cement" #starter string
for(d in 2:maxDeg){
  formStr<-sprintf("%s+I(Cement^%s)",formStr,d)
}
formStr

modMax<-lm(formula(formStr),
           data=concrete.df)
summary(modMax)

concrete.df$pred.lm<-predict(modMax)
ggplot(concrete.df,aes(Cement,Strength))+
  geom_point()+
  geom_line(aes(Cement,pred.lm))  

cement<-with(concrete.df,Cement)
resp<-with(concrete.df,Strength)

dim(cement)<-c(nrow(concrete.df),1)
dim(resp)<-c(nrow(concrete.df),1)

kval<-10
mod.knn<-knn.reg(cement,NULL,resp,k=kval)

concrete.df$pred.knn<-mod.knn$pred

nn<-75
rr<-with(concrete.df,range(Cement))
test<-seq(rr[1],rr[2],len=nn)
dim(test)<-c(nn,1)

mod.knn<-knn.reg(cement,test,resp,k=kval)

iterations <- 10
nn <- nrow(cement)
test <- sample(1:nn, nn/2, rep = F)
train <- setdiff(1:nn, test)

train.df <- concrete.df[train,]
test.df <- concrete.df[test,]

mse <- matrix(nrow = iterations, ncol = 2) # need 2 cols, one for testing the LM, one for the KNN
# we can only do it nn / 2 times since that is the number of values in the set and k can't be higher than that
for (i in 1:iterations) {
  # make lm
  formStr <- "Strength~Cement" #starter string
  for(d in 1:i){
    formStr <- sprintf("%s+I(Cement^%s)", formStr, d) # build the formula of degree i
  }
  
  linearModel <- lm(formula(formStr), data = train.df) # train the lm
  ggplot()
  
  # test the lm
  testResults <- predict(mod1, newdata = train.df) # test the lm
  head(testResults)
  mse[1, i] <- with(test, mean((test["strength"] - testResults)^2)) # put the MSE in the appropriate row, first column
}
