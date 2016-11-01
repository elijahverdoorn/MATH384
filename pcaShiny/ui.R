#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
pi0 <- 3.14
shinyUI(fluidPage(

  # Application title
  titlePanel("Principal Components App"),
  fluidRow(
      column(6,
             wellPanel(
                 sliderInput("theta", "Theta", min=0, max=pi0,value=0,
                             step=pi0/50,sep="")
                 )
             )
     ),
  fluidRow(
      column(4,
             plotOutput("pcrPlot1")
             ),
      column(4,
             plotOutput("pcrPlot2")
             ),
      column(4,
             plotOutput("pcrPlot3")
             )
  )))




# Define UI for application that draws a histogram
##shinyUI(fluidPage(
##
##  # Application title
##  titlePanel("Principal Components App"),
##
##  # Sidebar with a slider input for number of bins
##  sidebarLayout(
##    sidebarPanel(
##        sliderInput("theta", "Theta", min=0, max=pi,value=0,
##                    step=pi/100)
##    ),
##
##    # Show a plot of the generated distribution
##    mainPanel(
##        plotOutput("pcrPlot1"),
##        plotOutput("pcrPlot2")
##
##    )
##  )
##))
