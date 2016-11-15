library(ggplot2)
library(tidyverse)

#######################################################
## Boosting by hand
#######################################################

###############
## Create a crazy function as our target
n<-50
x<-seq(-4,4,len=n)
x
y<-1+3*x
#y<-sin(x)
y<-1+3*x+4*sin(2*x)+rnorm(n)




######################
data.df<-data.frame(x,y)
##Here is our initial approximation of y...zero
data.df$approx<-0
## Our initial residuals...same as our target
data.df$residual<-y

## We will keep looking at this picture. It has the current approximation,
## the target function, and the residuals
## Initiall thje 

data.df %>% 
  gather(type,val,y:residual) %>% 
  ggplot(aes(x,val,color=type))+
  scale_color_manual(values=c("red","blue","black"))+
  geom_line()


############################################
## Weak learning...use a trivial "stump" to estimate y
## Step 1: find the cut point in x that minimizes mse
############################################
#########################
## this is the boosting parameter.. the amount of boosting is key
lambda<-.05


####################
## The algorithm...we will repeat this many times

### Step 1: Find the optimal split in x and build a stump
mse<-c()
i<-1
for(x0 in x){
  mse[i]<-data.df %>% 
    mutate(class=x<x0) %>% 
    group_by(class) %>% 
    mutate(residual.hat = mean(residual)) %>% 
    mutate( r=mean((residual-residual.hat)^2)) %>% 
    with(sum(r))
  i<-i+1
}
##get index of smallest mse
id<-which.min(mse)
## This is our optimal slplit
x0 <- x[id]

##################
## Use this split to get the mean residual in each class (x<x0 x>=x0)
## and take a sliver of this off the current residual.
data.df<-data.df %>% 
  mutate(class=x<x0) %>% 
  group_by(class) %>% 
  ##the mean
  mutate(mu=mean(residual)) %>% 
  ##take off the residual (add onto the approximation)
  mutate(residual=residual-lambda*mu,
         approx=approx+lambda*mu) %>%
  ##clean up
  ungroup() %>% 
  select(-mu,-class) 


########################
## Plot...not necessary for the algorithm but nice to look at.
data.df %>% 
  gather(type,val,y:residual) %>% 
  ggplot(aes(x,val,color=type))+
  scale_color_manual(values=c("red","blue","black"))+
  geom_line()



##############################
## Encapsulate this all in a simple function.
##
doit<-function(data.df,SHOW=F){
    mse<-c()
    i<-1
    for(x0 in x){
      mse[i]<-data.df %>% 
        mutate(class=x<x0) %>% 
        group_by(class) %>% 
        mutate(residual.hat = mean(residual)) %>% 
        mutate( r=mean((residual-residual.hat)^2)) %>% 
        with(sum(r))
      i<-i+1
    }
    ##get index of smallest mse
    id<-which.min(mse)
    ##use this cut points. recompute
    x0 <- x[id]
    print(x0)
    data.df<-data.df %>% 
      mutate(class=x<x0) %>% 
      group_by(class) %>% 
      mutate(mu=mean(residual)) %>% 
      mutate(residual=residual-lambda*mu,
             approx=approx+lambda*mu) %>% 
      ungroup() %>% 
      select(-mu,-class) 
    
    if(SHOW){
      gg<-data.df %>% 
      gather(type,val,y:residual) %>% 
      ggplot(aes(x,val,color=type))+
      scale_color_manual(values=c("red","blue","black"))+
      geom_line()
   
      show(gg)
    }
    data.df
}

##########################
## We can run this over and over again to see the step-by-step effect of boosting
##
data.df<-doit(data.df,SHOW=T)


#######################
## Or run it a bunch of times.
#################
for(i in 1:1000){
  print(i)
  data.df<-doit(data.df)
}
## and now look at the plot
data.df<-doit(data.df,T)
