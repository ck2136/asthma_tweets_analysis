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
shinyUI(
  
  fluidPage(
    # Application title
    titlePanel("Asthma Tweet Analysis"),
    

    
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        
        dateRangeInput(inputId = "dateRange",
                       label = "Date range",
                       start = "2017-09-24",
                       end = Sys.Date(),
                       max = Sys.Date(),
                       min = "2017-09-24"
        ),
        
        sliderInput(inputId = "nTweet",
                    label = "Number of Tweets",
                    min = 100,
                    max = 7000,
                    value = 500,
                    step = 1),
        
        sliderInput(inputId = "nWords",
                    label = "Number of Words for Word Cloud",
                    min = 20,
                    max = 500,
                    value = 200,
                    step = 1)
        
      ),
      
      # Show Graphics
      mainPanel(
        tabsetPanel(
            tabPanel("Text Summary", textOutput("textdisplay")),
            tabPanel("Term Frequency", plotOutput("tfGraph")),
            tabPanel("Simple Word Cloud", plotOutput("swcGraph", width = "100%")),
            tabPanel("Simple Sentiment", plotOutput("sswcGraph")),
            tabPanel("Complex Sentiment", plotOutput("cswcGraph")),
            tabPanel("Smoothed Polarity of Asthma Tweets", plotOutput("smoothGraph"))
        )
        )
      )
    )
)