library(shiny)
library(plotly)
library(tidyverse)


shinyServer(function(input, output) {
  # Download data 
  ## Get zip file
  #download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip" 
  #, destfile = "data/covid_repo.zip") 
  
  ## Unzip file 
  #unzip("data/covid_repo.zip", exdir = "data")
  
  datanames <- list.files("data/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", 
                          pattern = "\\.csv$",
                          full.names = T)
  
  
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

  datmain <- reactive({
    if(input$countries == "Global"){
      dat0 <- dat %>% filter(between(Last.Update, min(input$dateRange),
                                     max(input$dateRange)))
    }
    else{
      dat0 <- dat %>% filter(Country.Region %in% input$countries & between(Last.Update, min(input$dateRange),
                                                           max(input$dateRange)))
    }

      dat0 %>% group_by(Country.Region, Last.Update) %>% 
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
      ggplotly(g() + geom_bar(aes(y = Confirmed, x = Country.Region, fill = Country.Region), stat = 'identity'))
  })

  output$deathsa <- renderPlotly({
    ggplotly(g() + geom_bar(aes(y = Deaths, x = Country.Region, fill = Country.Region), stat = 'identity'))
  })

  output$recovereda <- renderPlotly({
    ggplotly(g() + geom_bar(aes(y = Recovered, x = Country.Region, fill = Country.Region), stat = 'identity'))
  })

  output$activea <- renderPlotly({
    ggplotly(g() + geom_bar(aes(y = Active, x = Country.Region, fill = Country.Region), stat = 'identity'))
  })

  output$taba <- DT::renderDT({
      DT::datatable(dat2())
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
  
})


