f<-function(x) sin(x)
n<-20
sig<-0.5
x0<-0.5

M<-100
mseVals<-matrix(nrow=M,ncol=1)
biasVals<-matrix(nrow=M,ncol=1)
varVals<-matrix(nrow=M,ncol=1)
epsVals<-matrix(nrow=M,ncol=1)
for(m in 1:M){
  ##training set
  x<-runif(n,0,1)
  y<-f(x)+rnorm(n,0,sig)
  mod<-lm(y~poly(x,1))
  ##make prediction each time
  fhat_x0<-predict(mod,newdata=data.frame(x=x0))
  eps<-rnorm(1,0,sig)
  y0<-f(x0)+eps
  ##compute some values of interest
  mseVals[m,1]<-(y0-fhat_x0)^2
  biasVals[m,1]<-  f(x0)-fhat_x0
  varVals[m,1]<-fhat_x0
  epsVals[m,1]<-eps
}

mean(mseVals)
var(varVals)
mean(biasVals)^2
var(epsVals)

mean(mseVals)
var(varVals)+mean(biasVals)^2+var(epsVals)