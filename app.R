#Packages-----------------------------------------------------------------------
library(shiny)
library(glue) 
library(DBI)
library(RSQLite)
library(shinyjs)
library(plotly)

#Options------------------------------------------------------------------------
options(shiny.maxRequestSize=4000*1024^2)
#increases allowed upload size...I do not think I need this since you don't upload anything.

#Connection---------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "test_2.db") 
#need to add 'disconnect when close app' option

#Functions Bank-----------------------------------------------------------------

#Extracts data from a specific channel in database
get_channel_data <- function(num, chan, downsamp = FALSE){   
  chan <- paste0(num, chan)
  
  sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
  
  query_data <- DBI::dbSendQuery(con, sql_data)
  
  data <- DBI::dbFetch(query_data)
  DBI::dbClearResult(query_data)

  return(list(chan, data))
}

#Makes basic plot of channel data by selected time range
basic_plot <- function(chan_dat, time_range, meta_data, col){
  chan <- chan_dat[[1]]
  data <- chan_dat[[2]]
  
  min <- which(data$Time == time_range[1])
  max <- which(data$Time == time_range[2])
  
  meta1 <- meta_data[which(meta_data$ChannelID == chan),]
  name <- meta1$ChannelName
  units <- meta1$Units
  
  main <- paste0(name, " (", units, ")")
  
  fig <- plot_ly(data[min:max,], x = ~Time, y=~Value, type = 'scatter', mode = 'lines',
                 line = list(color = col), name = '') %>% layout(font = list(size = 9))
  
  fig <- fig %>% add_annotations(text = main,  x = 0, y = 1,
                                 yref = "paper", xref = "paper",
                                 xanchor = "left", yanchor = "top",
                                 yshift = 20, xshift = -50, showarrow = FALSE,
                                 font = list(size = 12))
  
  fig
  
}

#UI-----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("VUMC Sleep Study Database"),
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      #Select Options 
      uiOutput("selectvar"),
      br(),
      #Time Options
      h5(strong("Select Time Range (sec):")),
      #Below appears after app loads
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      textOutput("time_range"),
      uiOutput("select_time1"),
      uiOutput("select_time2"),
      actionButton("run", "Plot Raw Signals")
    ),
    mainPanel(
      #Tabs 
      uiOutput("tb")
    )
    
  )
)


#Server-------------------------------------------------------------------------

server <- function(input,output, session) {
  
  #Selecting subject to graph-----------------------------------------------------
  output$selectvar <- renderUI({
    opts <- dbGetQuery(con, 'SELECT SubjectID FROM pt_char')[[1]]
    list(selectInput("Select2", "Select Subject:", choices = opts)
    )
  })
  
  #Enter Time Range --------------------------------------------------------------
  
  #Range of possible times
  poss_time <- reactive({
    if(is.null(input$Select2)) {return()}
    select_sub <- dbQuoteLiteral(con, paste0('_',input$Select2))
    query_sub = paste0("SELECT TIME FROM channeldata WHERE ChannelID LIKE ", select_sub)
    time <- dbGetQuery(con, query_sub)
    c(min(time), max(time))
  })
  
  #Enter Start Time
  output$select_time1 <- renderUI({
    if(is.null(input$Select2)) {return()}
    list(numericInput("Time1", "Start:", value = poss_time()[1], 
                      min = poss_time()[1], max = poss_time()[2])
    )
  })
  
  #Enter End Time
  output$select_time2 <- renderUI({
    if(is.null(input$Select2)) {return()}
    list(numericInput("Time2", "End:", value = poss_time()[2], 
                      min = poss_time()[1], max = poss_time()[2])
    )
  })
  
  #Setting Time as Reactive Values
  time <- reactiveValues(time1 = NULL, time2 = NULL) 
  observeEvent(input$run,{time$time1 <- input$Time1})
  observeEvent(input$run,{time$time2 <- input$Time2})
  
  #Printed Instructions for Entering Time Range
  output$time_range <- renderText({
    if(is.null(input$Select2)) {return()}
    paste0("The time range for the selected subject is ",
           poss_time()[1], " to ", poss_time()[2], " seconds.")})
  
  #Set Time Range When Button is Pressed (also renders error messages for invalid values)
  range <- eventReactive(input$run, {
    validate(need(time$time1 >= poss_time()[1], "Start time is out of range.")) 
    #start time must be greater than or equal to smallest possible time
    validate(need(time$time2 <= poss_time()[2], "End time is out of range."))
    #end time must be less than or equal to greatest possible time
    c(as.numeric(time$time1), as.numeric(time$time2))
  })
  
  #Hide Button Until Time is Loaded
  observe({
    shinyjs::hide("run")
    
    if(!is.null(input$Time1))
      shinyjs::show("run")
  })
  
  #Raw Signals Tab Message--------------------------------------------------------
  output$text = renderText({"Enter time range and press 'Plot Raw Signals' to view graphs."})
  
  observe({
    shinyjs::show("text")
    
    if(!is.null(input$run))
      shinyjs::hide("text")
  })
  
  #Meta Data----------------------------------------------------------------------
  
  meta <- reactive({
    meta <- dbGetQuery(con, 'SELECT * FROM channelmeta')
    meta$ChannelName <- sub("_", " ", meta$ChannelName)
    meta
  })
  
  #Data---------------------------------------------------------------------------
  chan1 <- reactive(get_channel_data(1, input$Select2))
  chan2 <- reactive(get_channel_data(2, input$Select2))
  chan3 <- reactive(get_channel_data(3, input$Select2))
  chan4 <- reactive(get_channel_data(4, input$Select2))
  chan5 <- reactive(get_channel_data(5, input$Select2))
  chan6 <- reactive(get_channel_data(6, input$Select2))
  chan7 <- reactive(get_channel_data(7, input$Select2))
  chan8 <- reactive(get_channel_data(8, input$Select2))
  
  
  #Figure-------------------------------------------------------------------------
  output$fig <- renderPlotly({
    plot1 <- basic_plot(chan1(), range(), meta(), "red")
    plot2 <- basic_plot(chan2(), range(), meta(), "blue")
    plot3 <- basic_plot(chan3(), range(), meta(), "yellowgreen")
    plot4 <- basic_plot(chan4(), range(), meta(), "violet")
    plot5 <- basic_plot(chan5(), range(), meta(), "purple")
    plot6 <- basic_plot(chan6(), range(), meta(), "mediumspringgreen")
    plot7 <- basic_plot(chan7(), range(), meta(), "peru")
    plot8 <- basic_plot(chan8(), range(), meta(), "mediumpurple")
    
    p <- subplot(list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8), 
                 nrows = 8, shareX = TRUE, margin = 0.02) %>% layout(xaxis = list(title = 'Time (sec)'))
    p <- hide_legend(p)
  })
  
  
  #Main Panel Tabs----------------------------------------------------------------
  #Plots not shown until button is pressed.
  output$tb <- renderUI({
    if(is.null(con)) {return()}
    else
      tabsetPanel(
        tabPanel("Raw Signals", 
                 br(), textOutput('text'),
                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div("Loading...",id="loadmessage")),
                 plotlyOutput('fig', height = 800)))
  })
}

#Run-App------------------------------------------------------------------------
shinyApp(ui,server)