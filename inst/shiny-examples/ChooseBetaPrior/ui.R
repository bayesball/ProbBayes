#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Constructing a Beta Prior"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("Prior_median",
                   "Choose Prior Median:",
                   min = 0,
                   max = 1,
                   value = 0.5),
       sliderInput("Prior_90",
                   "Choose Prior 90th Percentile:",
                   min = 0,
                   max = 1,
                   value = 0.8)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       textOutput("text1"),
       textOutput("text2")
    )
  )
))
