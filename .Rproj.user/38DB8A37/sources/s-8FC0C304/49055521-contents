#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

shinyUI(navbarPage(
    shiny::titlePanel("COVID-19"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "countries", 
                           "Countries To Look For", 
                           selected = "Global",
                           multiple = T,
                           choices = datall$Country.Region),
            sliderInput(inputId = "dates", 
                        label = "Time Range", 
                        min = min(datall$Last.Update), 
                        max = max(datall$Last.Update), 
                        value = c(min(datall$Last.Update), max(datall$Last.Update)))
            
        ),
        mainPanel(
            tabsetPanel(
                tabsetPanel(
                    tabPanel("Overall Counts", plotOutput("plot")), 
                    tabPanel("Time Trends", verbatimTextOutput("summary"))
                )
        )
    )
    )
    
))