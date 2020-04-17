#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

shinyUI(navbarPage("COVID-19",
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "countries", 
                           "Countries To Look For", 
                           selected = "Global",
                           multiple = T,
                           choices = c("Global", sort(dat$Country.Region))),
            dateRangeInput('dateRange',
                           label = 'Date range input: yyyy-mm-dd',
                           start = min(dat$Last.Update), end = max(dat$Last.Update)
            )
            
        ),
        mainPanel(
            tabsetPanel(
                tabsetPanel(
                    tabPanel("Overall Counts", 
                             plotlyOutput("confirmeda"),
                             plotlyOutput("deathsa"),
                             plotlyOutput("recovereda"),
                             plotlyOutput("activea"),
                             DT::DTOutput("taba")
                             ), 
                    tabPanel("Time Trends", 
                             plotlyOutput("confirmedt"),
                             plotlyOutput("deathst"),
                             plotlyOutput("recovetredt"),
                             plotlyOutput("activet"),
                             DT::DTOutput("tabt")
                             )
                )
        )
    )
    )
    
))

