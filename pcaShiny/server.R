library(dplyr)
library(ggplot2)


#######################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(ISLR)
ad.df <-
    read.csv("Advertising.csv")
ad.df1 <- data.frame(scale(ad.df))
ad.df1 <- ad.df1[c("Radio","Newspaper")]
ad.dat <- data.matrix(ad.df1)
thetas <- seq(0,pi,length=100)
#################################
varDir <- function(theta){
    w.dir <- c(cos(theta),sin(theta))
    ##Projections onto  w.dir
    alpha <- ad.dat %*% w.dir
    w.proj <- cbind(alpha*w.dir[1],alpha*w.dir[2])
    ad.df2 <- ad.df1%>%
        bind_cols(data.frame(w.proj,alpha))
    names(ad.df2)[3:4] <- c("p1","p2")
    with(ad.df2,var(alpha))
}

vals <-sapply(thetas,varDir)
idx <- which.max(vals)
(theta.max <- thetas[idx])
var.max <- varDir(theta.max)
#######################################################


shinyServer(function(input, output) {
    computeValues <- reactive({
          theta <<- input$theta
        w.dir <- c(cos(theta),sin(theta))
        ##Projections onto  w.dir
        alpha <- ad.dat %*% w.dir
        w.proj <- cbind(alpha*w.dir[1],alpha*w.dir[2])
        ad.df2 <<- ad.df1%>%
            bind_cols(data.frame(w.proj,alpha))
          names(ad.df2)[3:4] <<- c("p1","p2")
    })


    output$pcrPlot1 <- renderPlot({
  ##       theta <- input$theta
  ##       w.dir <- c(cos(theta),sin(theta))
  ##       ##Projections onto  w.dir
  ##       alpha <- ad.dat %*% w.dir
  ##       w.proj <- cbind(alpha*w.dir[1],alpha*w.dir[2])
  ##       ad.df2 <- ad.df1%>%
  ##           bind_cols(data.frame(w.proj,alpha))
  ##       names(ad.df2)[3:4] <- c("p1","p2")
  ##       with(ad.df2,var(alpha))
  ##
        computeValues()
        ggplot(ad.df2,aes(Radio,Newspaper))+
            geom_point(size=3) +
            geom_segment(aes(x=Radio,xend=p1,y=Newspaper,yend=p2),color="red",size=.15)+
            geom_abline(slope=tan(theta),color="blue",size=2)+
            scale_x_continuous(limits=c(-2,3))+
            scale_y_continuous(limits=c(-2,4))+
            coord_equal()

    })

    output$pcrPlot2 <- renderPlot({
         computeValues()
        var.curr<- varDir(input$theta)
        data.frame(theta=thetas,Var=vals)%>%
            ggplot(aes(theta,Var))+geom_point(size=1)+
            geom_segment(aes(x=input$theta,xend=input$theta,y=0,yend=var.curr),color="blue",size=2)

    })


    output$pcrPlot3 <- renderPlot({
        computeValues()
        ggplot(ad.df2,aes(alpha))+
            geom_histogram(fill="blue",color="white",binwidth=.1)+
            scale_x_continuous(limits=c(-3,3))

    })



})
