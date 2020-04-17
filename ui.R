source('data.R', local = F)
library(plotly)
shinyUI(navbarPage("COVID-19",
    sidebarLayout(
        sidebarPanel(
            h3("Select all the countries you want and set the time range you want to search for"),
            h4("Delete `Global` if you want to see singular countries."),
            selectizeInput(inputId = "countries", 
                           "Countries To Look For", 
                           selected = "Global",
                           multiple = T,
                           choices = c("Global", sort(unique(dat$Country.Region)))),
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

