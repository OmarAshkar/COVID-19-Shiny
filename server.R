library(shiny)
library(plotly)
library(tidyverse)


# Download data 
## Get zip file
#download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip" 
#, destfile = "data/covid_repo.zip") 

## Unzip file 
#unzip("data/covid_repo.zip", exdir = "data")

## Load all csv files in daily_reports

datanames <- list.files("data/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", 
                        pattern = "\\.csv$",
                        full.names = T)


# pat <- "(?<=/)(\\d\\d)(.*)(?<=\\.csv)"

for(i in 1:60){
    # fname <-  str_extract(string = datanames, 
    #                       pattern = pat)[i]
    assign("fname",
           read.csv(datanames[i])
    )
    if(i == 1){
        x = fname
    }
    else {
        x <- bind_rows(x, fname)
    }
}


for(i in 61:83){
    # fnamey <-  str_extract(string = datanames, 
    #                       pattern = pat)[i]
    assign("fname",
           read.csv(datanames[i])
    )
    if(i == 61){
        y = fname
    }
    else {
        y <- bind_rows(y, fname)
    }
    
}
y <- y %>% rename(Last.Update = Last_Update, Province.State = Province_State,
                  Latitude = Lat, Longitude = Long_, Country.Region = Country_Region)

dat <- bind_rows(x, y) %>% select(2:6, 11)

dat$Last.Update <- as.Date(lubridate::parse_date_time(dat$Last.Update, c("%m/%d/%Y %H:%M", "%m/%d/%y %H:%M", "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M","%Y-%m-%d %H:%M:%S")))




# # Cleaning date. Till 560 --> %d/%m/%Y %h:% // rest --> automatic as_datetime
# dat1 <- dat[1:560,]
# dat2 <- dat[561:7617,]
# dat3 <- dat[7618:11034,]
# dat4 <- dat[11035:17102,]
# dat5 <- dat[17103:28136,]
# dat6 <- dat[28137:38439,]
# dat7 <- dat[38440:43358,]
# dat8 <- dat[43359:45927,]
# dat9 <- dat[45928:77354,]
# 
# dat1$Last.Update <- as.Date(dat1$Last.Update, format = "%m/%d/%Y %H:%M")
# dat2$Last.Update <- as.Date(dat2$Last.Update)
# dat3$Last.Update <- as.Date(dat3$Last.Update, format = "%m/%d/%y %H:%M")
# dat4$Last.Update <- as.Date(dat4$Last.Update)
# dat5$Last.Update <- as.Date(dat5$Last.Update)
# dat6$Last.Update <- as.Date(dat6$Last.Update, format = "%m/%d/%y %H:%M")
# dat7$Last.Update <- as.Date(dat7$Last.Update, format = "%Y-%m-%d %H:%M")
# dat8$Last.Update <- as.Date(dat8$Last.Update, format = "%m/%d/%Y %H:%M")
# dat9$Last.Update <- as.Date(dat9$Last.Update, format = "%Y-%m-%d %H:%M:%S")
# 
# # which.min(is.na(datall$Last.Update))
# # which.max(is.na(datall$Last.Update))
# # table(is.na(datall$Last.Update))
# 
# datall <- bind_rows(dat1, dat2, dat3, dat4, dat5, dat6, dat7)
# datall$Last.Update <- as.Date(format(datall$Last.Update, "%y-%m-%d"))
# 
# datall$Last.Update <- strptime(as.character(datall$Last.Update), "%y-%m-%d") %>% as.POSIXct()


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  datmain <- reactive({
        dat %>% filter(Country.Region %in% input$countries & between(Last.Update, min(input$dateRange),
                                                                 max(input$dateRange))) %>% 
      group_by(Country.Region, Last.Update) %>% 
      summarize(Confirmed = sum(Confirmed, na.rm = T),
                Deaths = sum(Deaths, na.rm = T),
                Active = sum(Active, na.rm = T),
                Recovered = sum(Recovered, na.rm = T))
  })
  
  dat2 <- reactive({
     datmain() %>% ungroup() %>% 
      group_by(Country.Region) %>% 
      summarize(Confirmed = sum(Confirmed, na.rm = T),
                Deaths = sum(Deaths, na.rm = T),
                Active = sum(Active, na.rm = T),
                Recovered = sum(Recovered, na.rm = T))
  })

  g <- reactive({
      ggplot(dat2()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })


  output$confirmeda <- renderPlotly({
      ggplotly(g() + geom_bar(aes(y = Confirmed, x = Country.Region), stat = 'identity'))
  })


  output$deathsa <- renderPlotly({
  ggplotly(g() + geom_bar(aes(y = Deaths, x = Country.Region), stat = 'identity'))
  })

  output$recovereda <- renderPlotly({
    ggplotly(g() + geom_bar(aes(y = Recovered, x = Country.Region), stat = 'identity'))
  })

  output$activea <- renderPlotly({
    ggplotly(g() + geom_bar(aes(y = Active, x = Country.Region), stat = 'identity'))
  })

  output$taba <- DT::renderDT({
      DT::datatable(datmain())
  })


  
  output$confirmedt <- renderPlotly({
    ggplotly(
      ggplot(data = datmain()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%d %b") + 
        geom_line(aes(y = Confirmed, x = Last.Update, color = Country.Region), stat = 'identity'))
  })
  output$activet <- renderPlotly({
    ggplotly(
      ggplot(data = datmain()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%d %b") + 
        geom_line(aes(y = Active, x = Last.Update, color = Country.Region), stat = 'identity'))
  })
  output$recovetredt <- renderPlotly({
    ggplotly(
      ggplot(data = datmain()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%d %b") + 
        geom_line(aes(y = Recovered, x = Last.Update, color = Country.Region), stat = 'identity'))
  })
  output$deathst <- renderPlotly({
    ggplotly(
      ggplot(data = datmain()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%d %b") + 
        geom_line(aes(y = Deaths, x = Last.Update, color = Country.Region), stat = 'identity'))
  })
    
  output$tabt <- DT::renderDT({
      datmain()
    })
# g2() + geom_line(aes(y = Deaths, x = Last.Update))
# g2() + geom_line(aes(y = Active, x = Last.Update))
# g2() + geom_line(aes(y = Recovered, x = Last.Update))
    
}) # End of shiny function

# https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
