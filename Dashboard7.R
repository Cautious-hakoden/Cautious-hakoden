library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)
library(HighFreq)
library(quantmod)
library(data.table)
library(shinyjs)
library(etrader)



ui <-
  dashboardPage(skin = "green",
    dashboardHeader(title = "Rashad's Website"),
    dashboardSidebar(      
      selectInput("action","Select order type",choices=c("BUY","SELL","BUY_TO_COVER","SELL_SHORT"),multiple = FALSE,selected="SELL"),
      selectInput('stock','Stock',choices=pick,multiple = FALSE,selected=pick[1]),
      textInput('Price','Buy/Sell Price',value=""),
      textInput('quan','Buy/Sell Quantity',value=""),
      sliderInput("slider", "Range:", 0, 10000, value=350,step =300),
      actionButton('submit', 'Submit Order')
                    ),
    dashboardBody(
      useShinyjs(),
      fluidRow(box(textOutput("balance"), title="Daily Gain/Loss")),
      fluidRow(
        tabBox(title = tagList(shiny::icon("gear"),    
        tags$style(
          ".tab-content div {
        font-size: 20px;
        font-weight: bold;
        
      }"), "tabBox status"),
            tabPanel("Moving Average","Plot Type",plotlyOutput("plot1", height = 250), background = "black"),
            tabPanel("DSS","Plot Type",plotlyOutput("plot1b", height = 250), background = "black")),
        tabBox(title = tagList(shiny::icon("gear"), "tabBox status"),
            tabPanel("Moving Average","Plot Type",plotlyOutput("plot2", height = 250), background = "black"),
            tabPanel("DSS","Plot Type",plotlyOutput("plot2b", height = 250), background = "black")),
        tabBox(title = tagList(shiny::icon("gear"), "tabBox status"),
            tabPanel("Moving Average","Plot Type",plotlyOutput("plot3", height = 250), background = "black"),
            tabPanel("DSS","Plot Type",plotlyOutput("plot3b", height = 250), background = "black")),
        tabBox(title = tagList(shiny::icon("gear"), "tabBox status"),
            tabPanel("Moving Average","Plot Type",plotlyOutput("plot4", height = 250), background = "black"),
            tabPanel("DSS","Plot Type",plotlyOutput("plot4b", height = 250), background = "black"))
                     ),
      fluidRow(
        box(DT::dataTableOutput("table1", width="100%",height = 250)),
        box(DT::dataTableOutput("table2", width="100%",height = 250)),
        box(DT::dataTableOutput("table3", width="100%",height = 250)),
        box(DT::dataTableOutput("table4", width="100%",height = 250))
      )
))

server <- function(input, output,session) {
  
  #########################################################################################################################
  #Submit orders
    observeEvent(input$submit,{
      data<-out()
      etrd_place_eq_order(
        account = account,symbol = input$stock,
        quantity=input$quan
         ,
        orderAction=input$action #"BUY"#, BUY_TO_COVER,    SELL_SHORT  BUY,
        ,priceType="LIMIT"
        ,stopPrice = "",
        limitPrice = input$Price,
        stopLimitPrice = "",
        quantityType = "quantity",
        orderTerm = "good_for_day",
        marketSession = "regular",
        allOrNone = "true",
        previewOrder = "none",
        access_tokens = access_tokens,
        etrade_cred = etrade_cred
        # ,sandbox = TRUE
      )
      #
    })
    

  out <- reactiveFileReader(8000, session,"C:/Users/Rash/Documents/technical.csv",read.csv)
  out2 <- reactiveFileReader(8000, session,"C:/Users/Rash/Documents/technical2.csv",read.csv)
  out3 <- reactiveFileReader(8000, session,"C:/Users/Rash/Documents/technical3.csv",read.csv)
  out4 <- reactiveFileReader(8000, session,"C:/Users/Rash/Documents/technical4.csv",read.csv)

  
  #########################################################################################################################
  #Data tables
    output$table1 <- DT::renderDataTable({
      x<-out()
      DT::datatable(out() %>% select(Count,Date,Symbol,Volume,Price,avg,PP1,Decide,diff_smooth,m,BC,SC) %>% arrange(desc(Count)))
    })
    
    output$table2 <- DT::renderDataTable({
      x<-out2()
      DT::datatable(out2() %>% select(Count,Date,Symbol,Volume,Price,avg,PP1,Decide,diff_smooth,m,BC,SC) %>% arrange(desc(Count)))
    })
    
    
    output$table3 <- DT::renderDataTable({
      x<-out3()
      DT::datatable(out3() %>% select(Count,Date,Symbol,Volume,Price,avg,PP1,Decide,diff_smooth,m,BC,SC) %>% arrange(desc(Count)))
    })
    
    
    output$table4 <- DT::renderDataTable({
      x<-out4()
      DT::datatable(out4() %>% select(Count,Date,Symbol,Volume,Price,avg,PP1,Decide,diff_smooth,m,BC,SC) %>% arrange(desc(Count)))
    })
    
    #########################################################################################################################
    #Generate Plots
    output$plot1b <- renderPlotly({
      ##load data
      x<-out()
      x$Close<-x$Price
      
      ##generate DSS
      xy<-data.frame(stoch(x[,c("Close")], n = 168,nFastD = 36,nSlowD = 36))
      x<-cbind(x,xy)
      #MA Plot with DSS
      x %>% filter(Count>input$slider) %>%
        plot_ly(x=~Date,y=~Price,  type = 'scatter', name = "Price")  %>%
        # add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines", name = "13min_MA") %>%
        add_trace(x=~Date, y = ~fastD,type = 'scatter',mode="lines", yaxis = "y2", name = "fastD") %>%
        add_trace(x=~Date, y = ~slowD,type = 'scatter',mode="lines", yaxis = "y2", name = "slowD") %>%
        layout(xaxis= list(showticklabels = FALSE),
          yaxis2 = list(overlaying = "y", side = "right"))
      
    })
  
    output$plot1 <- renderPlotly({
      ##load data
      x<-out() %>% filter(Count>input$slider) 

      

        #MA plot
      x %>% plot_ly() %>%
        add_trace(x=~Date,y=~Price,type = 'scatter',mode="markers")  %>%
        add_trace(x=~Date, y = ~avg,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_8,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines") %>%
        layout(
          xaxis = list(
            type = 'date'
          ),yaxis2 = list(overlaying = "y", side = "right"))

    })
    
    output$plot2b <- renderPlotly({
      ##load data
      x<-out2()
      x$Close<-x$Price
      
      ##generate DSS
      xy<-data.frame(stoch(x[,c("Close")], n = 168,nFastD = 36,nSlowD = 36))
      x<-cbind(x,xy)
      #MA Plot with DSS
      x %>% filter(Count>input$slider) %>%
        plot_ly(x=~Date,y=~Price,  type = 'scatter', name = "Price")  %>%
        # add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines", name = "13min_MA") %>%
        add_trace(x=~Date, y = ~fastD,type = 'scatter',mode="lines", yaxis = "y2", name = "fastD") %>%
        add_trace(x=~Date, y = ~slowD,type = 'scatter',mode="lines", yaxis = "y2", name = "slowD") %>%
        layout(xaxis= list(showticklabels = FALSE),
               yaxis2 = list(overlaying = "y", side = "right"))
      
    })
    
    output$plot2 <- renderPlotly({
      ##load data
      x<-out2() %>% filter(Count>input$slider) 
      
      
      
      #MA plot
      x %>% plot_ly() %>%
        add_trace(x=~Date,y=~Price,type = 'scatter',mode="markers")  %>%
        add_trace(x=~Date, y = ~avg,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_8,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines") %>%
        layout(
          xaxis = list(
            type = 'date'
          ),yaxis2 = list(overlaying = "y", side = "right"))
      
    })
    
    
    output$plot3b <- renderPlotly({
      ##load data
      x<-out3()
      x$Close<-x$Price
      
      ##generate DSS
      xy<-data.frame(stoch(x[,c("Close")], n = 168,nFastD = 36,nSlowD = 36))
      x<-cbind(x,xy)
      #MA Plot with DSS
      x %>% filter(Count>input$slider) %>%
        plot_ly(x=~Date,y=~Price,  type = 'scatter', name = "Price")  %>%
        # add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines", name = "13min_MA") %>%
        add_trace(x=~Date, y = ~fastD,type = 'scatter',mode="lines", yaxis = "y2", name = "fastD") %>%
        add_trace(x=~Date, y = ~slowD,type = 'scatter',mode="lines", yaxis = "y2", name = "slowD") %>%
        layout(xaxis= list(showticklabels = FALSE),
               yaxis2 = list(overlaying = "y", side = "right"))
      
    })
    
    output$plot3 <- renderPlotly({
      ##load data
      x<-out3() %>% filter(Count>input$slider) 
      
      
      
      #MA plot
      x %>% plot_ly() %>%
        add_trace(x=~Date,y=~Price,type = 'scatter',mode="markers")  %>%
        add_trace(x=~Date, y = ~avg,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_8,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines") %>%
        layout(
          xaxis = list(
            type = 'date'
          ),yaxis2 = list(overlaying = "y", side = "right"))
      
    })
    
    output$plot4b <- renderPlotly({
      ##load data
      x<-out4()
      x$Close<-x$Price
      
      ##generate DSS
      xy<-data.frame(stoch(x[,c("Close")], n = 168,nFastD = 36,nSlowD = 36))
      x<-cbind(x,xy)
      #MA Plot with DSS
      x %>% filter(Count>input$slider) %>%
        plot_ly(x=~Date,y=~Price,  type = 'scatter', name = "Price")  %>%
        # add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines", name = "13min_MA") %>%
        add_trace(x=~Date, y = ~fastD,type = 'scatter',mode="lines", yaxis = "y2", name = "fastD") %>%
        add_trace(x=~Date, y = ~slowD,type = 'scatter',mode="lines", yaxis = "y2", name = "slowD") %>%
        layout(xaxis= list(showticklabels = FALSE),
               yaxis2 = list(overlaying = "y", side = "right"))
      
    })
    
    output$plot4 <- renderPlotly({
      ##load data
      x<-out4() %>% filter(Count>input$slider) 
      
      
      
      #MA plot
      x %>% plot_ly() %>%
        add_trace(x=~Date,y=~Price,type = 'scatter',mode="markers")  %>%
        add_trace(x=~Date, y = ~avg,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_8,type = 'scatter',mode="lines") %>%
        add_trace(x=~Date, y = ~avg_13,type = 'scatter',mode="lines") %>%
        layout(
          xaxis = list(
            type = 'date'
          ),yaxis2 = list(overlaying = "y", side = "right"))
      
    })
    
    output$balance<-renderText({
      # curr<-curr()
      curr<-etrd_account_balance(account,realTimeNAV = "true",output = c("df"),
                                 access_tokens = access_tokens,etrade_cred = etrade_cred,sandbox = sandbox) %>% group_by(Computed.cashBuyingPower) %>% summarise(Computed.cashBuyingPower=sum(as.numeric(Computed.cashBuyingPower)))
      bal<-curr$Computed.cashBuyingPower-start$Computed.cashBuyingPower
      print(bal)
      bal
    })
    #########################################################################################################################
}

shinyApp(ui, server)