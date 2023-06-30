#Packages-----------------------------------------------------------------------
library(shiny)
library(glue)
library(DBI)
library(RSQLite)

#Options------------------------------------------------------------------------
options(shiny.maxRequestSize=4000*1024^2)

con <- DBI::dbConnect(RSQLite::SQLite(), "test_2.db") 

#UI-----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("VUMC Sleep Study Database"),
  sidebarLayout(
    sidebarPanel(
      #Select Options (appear once file is uploaded)
      uiOutput("selectvar"),
      br(),
      h4(strong("Select Time Range (sec):")),
      uiOutput("select_time1"),
      uiOutput("select_time2")
    ),
    mainPanel(
      #Tabs (appear once file is uploaded)
      uiOutput("tb")
    )
    
  )
)


#Server-------------------------------------------------------------------------

server <- function(input,output, session) {
  
  #Select options in side bar that depend on output
  #Selecting Person to graph
  output$selectvar <- renderUI({
    opts <- dbGetQuery(con, 'SELECT SubjectID FROM pt_char')
    list(hr(), 
         selectInput("Select2", "Select Subject:", choices = opts)
    )
    
  })
  
  output$select_time1 <- renderUI({
    if(is.null(input$Select2)) {return()}
    select_sub <- dbQuoteLiteral(con, paste0('_',input$Select2))
    query_sub = paste0("SELECT TIME FROM channeldata WHERE ChannelID LIKE ", select_sub)
    time <- dbGetQuery(con, query_sub)
    
    t_min = min(time)
    t_max = max(time)
    
    list(hr(),
         numericInput("Time1", "Start:", value = 1990, min = t_min, max = t_max)
    )
    
  })
  
  output$select_time2 <- renderUI({
    if(is.null(input$Select2)) {return()}
    select_sub <- dbQuoteLiteral(con, paste0('_',input$Select2))
    query_sub = paste0("SELECT TIME FROM channeldata WHERE ChannelID LIKE ", select_sub)
    time <- dbGetQuery(con, query_sub)
    
    t_min = min(time)
    t_max = max(time)
    
    list(hr(),
         numericInput("Time2", "End:", value = 2000, min = t_min, max = t_max)
    )
    
  })
  
  
#Plot 1-------------------------------------------------------------------------
  output$plot1 <- renderPlot({
    chan <- paste0('1', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "Stim Monitor (V)", xlab = "Time (sec)", 
         ylab = "Stim Monitor (V)")
    
  })
  
#Plot 2-------------------------------------------------------------------------
  output$plot2 <- renderPlot({
    chan <- paste0('2', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "RIP Thorax (mV)", xlab = "Time (sec)", 
         ylab = "RIP Thorax (mV)")
    
  })
  
#Plot 3-------------------------------------------------------------------------
  output$plot3 <- renderPlot({
    chan <- paste0('3', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "RIP Abd (mV)", xlab = "Time (sec)", 
         ylab = "RIP Abd (mV)")
    
  })

#Plot 4-------------------------------------------------------------------------
  output$plot4 <- renderPlot({
    chan <- paste0('4', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "Pv (mmHg)", xlab = "Time (sec)", 
         ylab = "Pv (mmHg)")
    
  })

#Plot 5-------------------------------------------------------------------------
  output$plot5 <- renderPlot({
    chan <- paste0('5', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "Pepi (mmHg)", xlab = "Time (sec)", 
         ylab = "Pepi (mmHg)")
    
  })
  
#Plot 6-------------------------------------------------------------------------
  output$plot6 <- renderPlot({
    chan <- paste0('6', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "Flow (L/min)", xlab = "Time (sec)", 
         ylab = "Flow (L/min)")
    
  })

#Plot 7-------------------------------------------------------------------------
  output$plot7 <- renderPlot({
    chan <- paste0('7', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "Volume (L)", xlab = "Time (sec)", 
         ylab = "Volume (L)")
    
  })
  
#Plot 8-------------------------------------------------------------------------
  output$plot8 <- renderPlot({
    chan <- paste0('8', input$Select2)
    
    sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan}
  ", .con = con)
    
    query_data <- DBI::dbSendQuery(con, sql_data)
    
    data <- DBI::dbFetch(query_data)
    DBI::dbClearResult(query_data)
    
    min <- which(data$Time == as.numeric(input$Time1))
    max <- which(data$Time == as.numeric(input$Time2))
    
    plot(data$Time[min:max], data$Value[min:max], type = "l",
         main = "CPAP Pressure (cm H2O)", xlab = "Time (sec)", 
         ylab = "CPAP Pressure (cm H2O)")
    
  })
  
  #Displaying head 'pt_char'
  output$table1 <- renderTable({ 
    dbGetQuery(con, 'SELECT * FROM pt_char LIMIT 5')
  })
  
  #Displaying head 'channelmeta'
  output$table2 <- renderTable({ 
    dbGetQuery(con, 'SELECT * FROM channelmeta LIMIT 5')
  })
  
  #Displaying head 'channeldata'
  output$table3 <- renderTable({ 
    dbGetQuery(con, 'SELECT * FROM channeldata LIMIT 5')
  })
  
  #Displaying head 'annots'
  output$table4 <- renderTable({ 
    dbGetQuery(con, 'SELECT * FROM annots LIMIT 5')
  })
  
  
  
  #Main Panel Tabs
  output$tb <- renderUI({
    if(is.null(con)) {return(c('None'))}
    else
      tabsetPanel(
        tabPanel("Example Output of Each Table", h4(strong('Patient Characteristics')), tableOutput("table1"), br(),  
                 h4(strong('Channel Meta Data')),tableOutput("table2"), br(), 
                 h4(strong('Channel Data')), tableOutput("table3"), br(),  
                 h4(strong('Annotations')),tableOutput("table4")),
        tabPanel("Plots", plotOutput('plot1'), plotOutput('plot2'), plotOutput('plot3'), plotOutput('plot4'),
                 plotOutput('plot5'), plotOutput('plot6'), plotOutput('plot7'), plotOutput('plot8'))
      )
  })
}

#Run-App------------------------------------------------------------------------
shinyApp(ui,server)
