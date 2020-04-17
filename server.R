library(shiny)
library(plotly)
library(tidyverse)

source('data.R', local = F)
shinyServer(function(input, output, session) {

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


