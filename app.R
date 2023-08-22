#Packages-----------------------------------------------------------------------
library(shiny)
library(glue) 
library(DBI)
library(RSQLite)
library(shinyjs)
library(plotly)
library(reticulate)

use_virtualenv('my-python', required = TRUE)
source_python("C:/Users/megan/Desktop/Summer-2023-Project/Sleep-Study-Shiny-App/reference.py")

np <- import("numpy", convert = TRUE)


#Options------------------------------------------------------------------------
options(shiny.maxRequestSize=4000*1024^2)
#increases allowed upload size...I do not think I need this since you don't upload anything.

#Connection---------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/megan/Desktop/Summer-2023-Project/test_2.db") 
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
  
  if (downsamp == TRUE) {
    sql_meta <- glue_sql("
    SELECT *
    FROM channelmeta
    ", .con = con)
    
    query_meta <- DBI::dbSendQuery(con, sql_meta)
    
    meta <- DBI::dbFetch(query_meta)
    DBI::dbClearResult(query_meta)
    
    rate_stim <- meta[meta$ChannelID == chan,'SampleRate'][1]
    rate <- unique(meta[meta$SampleRate != rate_stim, 'SampleRate']) 
    
    data$Value <- low_pass_filter(data$Value, rate = rate_stim, cutoff = 1)
    
    data2 <- down_sample(sig_stim_lfilt = data$Value, sample_rate = rate) #broken
    
    list <- which(is.na(match(data$Value, data2)) == FALSE)
    
    data <- data[list,]
  }
  
  return(list (channel = chan, data = data))
}

#Makes basic plot of channel data by selected time range
basic_plot <- function(chan_dat, time_range, meta_data, annots_data, sub, col){
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
  
  annots <- annots_data
  annot_line <- vector(mode = 'list')
  
  for (n in 1:nrow(annots)){
    if(annots$ChannelID[n] == chan & annots$Time[n] > time_range[1] & annots$Time[n] < time_range[2]){
      fig <- fig %>% add_trace(x = annots$Time[n], type = 'scatter', mode = 'lines',
                               line = list(color = "darkgrey"), text = annots$Annotation[n])
    }
  }
  
  chan0 <- paste0('0', sub)
  channeg <- paste0('-1', sub)
  
  for (n in 1:nrow(annots)){
    if(annots$ChannelID[n] == chan0 & annots$Time[n] > time_range[1] & annots$Time[n] < time_range[2]){
      fig <- fig %>% add_trace(x = annots$Time[n], type = 'scatter', mode = 'lines',
                               line = list(color = "darkgrey"), text = annots$Annotation[n])
    }
  }
  
  for (n in 1:nrow(annots)){
    if(annots$ChannelID[n] == channeg & annots$Time[n] > time_range[1] & annots$Time[n] < time_range[2]){
      fig <- fig %>% add_trace(x = annots$Time[n], type = 'scatter', mode = 'lines',
                               line = list(color = "darkgrey"), text = annots$Annotation[n])
    }
  }
  
  fig
  
}

#Extracts data from a specific channel in database
get_annots <- function(chan){   
  chan <- paste0('%', chan, '%')
  
  sql_annots <- glue_sql("
    SELECT *
    FROM annots
    WHERE ChannelID LIKE {chan}
  ", .con = con)
  
  query_annots <- DBI::dbSendQuery(con, sql_annots)
  annots <- DBI::dbFetch(query_annots)
  DBI::dbClearResult(query_annots)
  return(annots)
}


#Get_Baseline______
#Rewriting functions from Yikes code because I cannot get them to run correctly 
# when calling from Python

get_baseline <- function(vector, data){
  start <- as.numeric(data['Exp start'])
  end <- as.numeric(data['Exp end'])
  mean(vector[start:end])
}

#Limit Time Range of Data
limit_range <- function(chans_dat, time_range){
  chans <- chans_dat
  
  chans_new <- vector(mode = 'list', length = length(chans))
  
  for(n in 1:length(chans)){
    c <- chans[[n]][[2]]
    
    min <- which(c$Time == time_range[1])
    max <- which(c$Time == time_range[2])
    
    chans_new[[n]] <- c[min:max,]
    
  }
  return(chans_new)
  
}

stim_loop1 <- function(df1, stim_mode, stim = stim_lbl, airway = airway_lbl){
  
  df_set <- df1[df1[stim] == stim_mode & df1[airway] == 'NFL',]
  
  min_nfl_p <- suppressWarnings({ifelse(is.infinite(min(df_set$CPAP.ceil)), NA, min(df_set$CPAP.ceil))})
  
  y_nfl <- suppressWarnings({ifelse(is.na(min_nfl_p), NA, mean(df_set$Flow.max.adj[df_set$CPAP.ceil == min_nfl_p]))})
  return(y_nfl)
}

stim_loop2 <- function(df2, stim_mode, stim = stim_lbl, airway = airway_lbl) {
  
  df_set <- df2[df2[stim] == stim_mode,]
  
  x_fl <- df_set[df_set[airway] == 'FL', 'CPAP.ceil']
  y_fl <- df_set[df_set[airway] == 'FL', 'Flow.max.adj'] * 1000/60
  
  max_apnea_p <- suppressWarnings({ifelse(is.infinite(max(df_set[df_set[airway] == 'Apnea', 'CPAP.ceil'])), NA, max(df_set[df_set[airway] == 'Apnea', 'CPAP.ceil']))})
  min_nfl_p <- suppressWarnings({ifelse(is.infinite(min(df_set[df_set[airway] == 'NFL', 'CPAP.ceil'])), NA, min(df_set[df_set[airway] == 'NFL', 'CPAP.ceil']))})
  
  x_apnea <- df_set[df_set[airway] == 'Apnea' & df_set$CPAP.ceil == max_apnea_p, 'CPAP.ceil']
  y_apnea <- df_set[df_set[airway] == 'Apnea' & df_set$CPAP.ceil == max_apnea_p, 'Flow.max.adj'] * 1000/60
  
  x_nfl <- df_set[df_set[airway] == 'NFL' & df_set$CPAP.ceil == min_nfl_p, 'CPAP.ceil']
  y_nfl <- df_set[df_set[airway] == 'NFL' & df_set$CPAP.ceil == min_nfl_p, 'Flow.max.adj'] * 1000/60
  
  x_arr <- c(x_fl, x_apnea, x_nfl)
  y_arr <- c(y_fl, y_apnea, y_nfl)
  airway <- c(rep('FL', length(x_fl)), rep('Apnea', length(x_apnea)), rep('NFL', length(x_nfl)))
  
  data <- data.frame(x_arr, y_arr, airway)
  
  return(data)
}

figure <- function(data, stim_mode, lbl_size = 15, stim = stim_lbl, airway = airway_lbl) {
  
  dat <- as.data.frame(data[,colnames(data) == stim_mode])
  
  #marker color and type
  if (stim_mode == 'Combined'){
    marker_color = 'green'
    marker_type = 'circle'
  } else if (stim_mode == 'HGNS') {
    marker_color = 'darkorange'
    marker_type = 'triangle-up'
  } else if (stim_mode == 'ACS') {
    marker_color = 'magenta'
    marker_type = 'diamond'
  } else {
    marker_color = 'navy'
    marker_type = 'square'
  }
  
  fl_list <- list(x = c(dat[dat$airway == 'FL',]$x_arr), y = c(dat[dat$airway == 'FL',]$y_arr), 
                  name = paste(stim_mode, 'FL'), mode = 'markers',
                  marker = list(size = lbl_size, color = marker_color, opacity = 0.3, line = list(color = marker_color, width = 2)), 
                  symbols = marker_type)
  
  apnea_list <- list(x = c(dat[dat$airway == 'Apnea',]$x_arr), y = c(dat[dat$airway == 'Apnea',]$y_arr), 
                     name = paste(stim_mode, 'Apnea'), mode = 'markers',
                     marker = list(size = lbl_size, color = marker_color, opacity = 0.3, line = list(color = marker_color, width = 2)), 
                     symbols = marker_type)
  
  nfl_list <- list(x = c(dat[data$airway == 'NFL',]$x_arr), y = c(dat[data$airway == 'NFL',]$y_arr), 
                   name = paste(stim_mode, 'NFL'), mode = 'markers',
                   marker = list(size = lbl_size, color = 'white', opacity = 0.7, line = list(color = marker_color, width = 2)), 
                   symbols = marker_type)
  
  return(list(fl = fl_list, apnea = apnea_list, nfl = nfl_list))
}

show_curve <- function(data, y.open) {
  
  plt <- vector(mode='list')
  legend <- vector(mode='list')
  
  for (i in seq(ncol(data))){
    name <- colnames(data)[i]
    df_temp <- as.data.frame(data[,i])
    x <- df_temp$x_arr
    y <- df_temp$y_arr
    z <- np$polyfit(x, y, 1)
    p <- np$poly1d(z)
    x_zero <- -z[2]/z[1]
    
    x_nfl_min <- suppressWarnings({ifelse(is.infinite(min(df_temp[df_temp$airway == 'NFL',]$x_arr)), NA, min(df_temp[df_temp$airway == 'NFL',]$x_arr))})
    
    x_open <- suppressWarnings({ifelse(is.infinite(max(c((y.open - z[2])/z[1], x_nfl_min), na.rm = TRUE)), NA, 
                                       max(c((y.open - z[2])/z[1], x_nfl_min), na.rm = TRUE))})
    
    #x_plt <- c(x_zero, x_open)
    x_plt <- seq(-5, 20, by = 0.1)
    
    popen <- (y.open - z[2])/z[1]
    
    line1 <- paste0(name, ": y = ", round(z[1], 2), "x + ", round(z[2], 2))
    line2 <- paste0("Pcrit: ", round(x_zero, 2), "; Popen: ", round(popen, 2))
    
    legend[[name]] <- data.frame(line1, line2)
    
    plt[[name]] <- data.frame(x = x_plt, y = p(x_plt))
  }
  
  return(list(plt, legend))
}

show_outside <- function(df4, stim_mode, stim = stim_lbl, airway = airway_lbl) {
  
  df_set <- df4[df4[stim] == stim_mode,]
  
  max_apnea_p <- suppressWarnings({max(df_set[df_set[airway] == 'Apnea', 'CPAP.ceil'])})
  min_nfl_p <- suppressWarnings({min(df_set[df_set[airway] == 'NFL', 'CPAP.ceil'])})
  
  x_nfl_discard <- df_set[df_set[airway] == 'NFL' & df_set['CPAP.ceil'] > min_nfl_p, 'CPAP.ceil']
  y_nfl_discard <- df_set[df_set[airway] == 'NFL' & df_set['CPAP.ceil'] > min_nfl_p, 'Flow.max.adj'] * 1000/60 
  
  x_apnea_discard <- df_set[c(df_set[airway] == 'Apnea' & df_set['CPAP.ceil'] < max_apnea_p) | df_set[airway] == 'Dogleg', 'CPAP.ceil']
  y_apnea_discard <- df_set[c(df_set[airway] == 'Apnea' & df_set['CPAP.ceil'] < max_apnea_p) | df_set[airway] == 'Dogleg', 'Flow.max.adj'] * 1000/60
  
  x_arr <- c(x_nfl_discard, x_apnea_discard)
  y_arr <- c(y_nfl_discard, y_apnea_discard)
  airway <- c(rep('NFL', length(x_nfl_discard)), rep('Apnea', length(x_apnea_discard)))
  
  data <- data.frame(x_arr, y_arr, airway)
  
  return(data)
  
}

figure_outside <- function(data, stim_mode, lbl_size = 15, stim = stim_lbl, airway = airway_lbl) {
  
  data <- as.data.frame(data[, stim_mode])
  
  #marker color and type
  if (stim_mode == 'Combined'){
    marker_color = 'green'
    marker_type = 'circle'
  } else if (stim_mode == 'HGNS') {
    marker_color = 'darkorange'
    marker_type = 'triangle-up'
  } else if (stim_mode == 'ACS') {
    marker_color = 'magenta'
    marker_type = 'diamond'
  } else {
    marker_color = 'navy'
    marker_type = 'square-open'
  }
  
  apnea_list <- list(x = c(data[data$airway == 'Apnea',]$x_arr), y = c(data[data$airway == 'Apnea',]$y_arr), 
                     name = paste(stim_mode, 'Apnea Discard'), mode = 'markers',
                     marker = list(size = lbl_size, color = marker_color, opacity = 0.9, line = list(color = marker_color, width = 2)), 
                     symbols = marker_type) #wrong?
  
  nfl_list <- list(x = c(data[data$airway == 'NFL',]$x_arr), y = c(data[data$airway == 'NFL',]$y_arr), 
                   name = paste(stim_mode, 'NFL Discard'), mode = 'markers',
                   marker = list(size = lbl_size, color = marker_color, opacity = 0.9), 
                   symbols = marker_type) 
  
  return(list(apnea = apnea_list, nfl = nfl_list))
}

h_line <- function(y, col, type) {
  list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y,
       line = list(color = col, dash = type))
}

get_plot_data <- function(df, reviewed = FALSE) {
  airway_lbl <- 'Airway.status.DL'
  stim_lbl <- 'Stim.mode'
  if (reviewed == TRUE) {
    airway_lbl <- 'Airway.status.final'
    stim_lbl <- 'Stim.mode.final'
  }
  
  stim_mode_list <- unique(df[,stim_lbl])
  
  min_nfl_list <- sapply(stim_mode_list, stim_loop1, df1 = df, stim = stim_lbl, airway = airway_lbl)
  min_nfl_list <- min_nfl_list[!is.na(min_nfl_list)]
  y.open <- mean(unlist(min_nfl_list)) * 1000/60
  dat <- sapply(stim_mode_list, stim_loop2, df2 = df, stim = stim_lbl, airway = airway_lbl)
  return(list(y.open, dat))
}
  

plot_pressure_flow_curve <- function(df, reviewed = FALSE, outside = FALSE, curve = TRUE) {
  fig <- plot_ly(type = 'scatter', mode = 'marker') 
  
  dat <- get_plot_data(df)[[2]]
  y.open <- get_plot_data(df)[[1]]
  
  airway_lbl <- 'Airway.status.DL'
  stim_lbl <- 'Stim.mode'
  if (reviewed == TRUE) {
    airway_lbl <- 'Airway.status.final'
    stim_lbl <- 'Stim.mode.final'
  }
  stim_mode_list <- unique(df[,stim_lbl])
  
  fig_atts <- sapply(stim_mode_list, figure, data = dat, stim = stim_lbl, airway = airway_lbl)
  for (i in seq_along(fig_atts)){
    fig <- add_trace(fig, x=fig_atts[[i]]$x ,y=fig_atts[[i]]$y, 
                     name=fig_atts[[i]]$name, mode=fig_atts[[i]]$mode,
                     marker = list(size = fig_atts[[i]]$marker$size, color = fig_atts[[i]]$marker$color,
                                   opacity = fig_atts[[i]]$marker$opacity, 
                                   line = list(color = fig_atts[[i]]$marker$line$color, width = fig_atts[[i]]$marker$line$width),
                                   symbol = fig_atts[[i]]$symbols),
                     showlegend = FALSE)
  }
  
  if(outside == TRUE) {
    outside <- sapply(stim_mode_list, show_outside, df4 = df, stim = stim_lbl, airway = airway_lbl)
    outside_atts <- sapply(stim_mode_list, figure_outside, data = outside, stim = stim_lbl, airway = airway_lbl)
    
    for (i in seq_along(outside_atts)){
      fig <- add_trace(fig, x=outside_atts[[i]]$x ,y=outside_atts[[i]]$y, 
                       name=outside_atts[[i]]$name, mode=outside_atts[[i]]$mode,
                       marker = list(size = outside_atts[[i]]$marker$size, color = outside_atts[[i]]$marker$color,
                                     opacity = outside_atts[[i]]$marker$opacity, 
                                     line = list(color = outside_atts[[i]]$marker$line$color, width = outside_atts[[i]]$marker$line$width),
                                     symbol = outside_atts[[i]]$symbols),
                       showlegend = FALSE)
    }}
  
  if(curve == TRUE){
    x <- show_curve(dat, y.open)
    
    for (i in seq_along(x[[2]])) {
      
      stim_mode = names(x[[2]][i])
      
      if (stim_mode == 'Combined'){
        marker_color = 'green'
        marker_type = 'circle'
      } else if (stim_mode == 'HGNS') {
        marker_color = 'darkorange'
        marker_type = 'triangle-up'
      } else if (stim_mode == 'ACS') {
        marker_color = 'magenta'
        marker_type = 'diamond'
      } else {
        marker_color = 'navy'
        marker_type = 'square'
      }
      
      fig <- add_trace(fig, x = x[[1]][[i]]$x, y = x[[1]][[i]]$y, mode = 'lines', name = x[[2]][[i]][1],
                       line = list(color = marker_color, dash = 'dash'))
      fig <- add_trace(fig, x = c(0, 1), y = c(0, 1), mode = 'lines', name = x[[2]][[i]][2],
                       line = list(color = 'white', dash = 'dash')) 
    }
  }
  
  fig <- fig %>% layout(shapes = list(h_line(y = 0, col = 'lightcoral', type = 'dashdot'),
                                      h_line(y = y.open, col = 'orchid', type = 'dash')),
                        title = list(text = 'Pressure Flow Curves', xanchor = 'center', yanchor = 'top'),
                        xaxis = list(title = 'CPAP Pressure (cm H20)', range = list(-1, 20)),
                        yaxis = list(title = 'Peak Inspiratory Flow (mL/sec)'),
                        legend = list(x = 0.75, y = 0.95, font = list(size = 14)))
  
  return(fig)
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
      actionButton("run", "Plot Raw Signals"),
      actionButton("run2", "Plot Pressure Flow Curve"),
      checkboxInput("curve", "Show curve?", FALSE),
      checkboxInput("outside", "Show outside?", FALSE)
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
  
  #Setting Time for Flow Curve as Reactive Variable
  time_flow <- reactiveValues(time1 = NULL, time2 = NULL) 
  observeEvent(input$run,{time_flow$time1 <- input$Time1})
  observeEvent(input$run,{time_flow$time2 <- input$Time2})
  
  #Set Time Range When Button is Pressed (also renders error messages for invalid values)
  range_flow <- eventReactive(input$run2, {
    validate(need(time_flow$time1 >= poss_time()[1], "Start time is out of range.")) 
    #start time must be greater than or equal to smallest possible time
    validate(need(time_flow$time2 <= poss_time()[2], "End time is out of range."))
    #end time must be less than or equal to greatest possible time
    c(as.numeric(time_flow$time1), as.numeric(time_flow$time2))
  })
  
  #Hide Button Until Time is Loaded
  observe({
    shinyjs::hide("run2")
    
    if(!is.null(input$Time1))
      shinyjs::show("run2")
  })
  
#Raw Signals Tab Message--------------------------------------------------------
  output$text = renderText({"Enter time range and press 'Plot Raw Signals' to view graphs."})
  
  observe({
    shinyjs::show("text")
    
    if(!is.null(input$run))
      shinyjs::hide("text")
  })
  
#Plot Pressure Flow Tab Message-------------------------------------------------
  output$text2 = renderText({"Enter time range and press 'Plot Pressure Flow Curve' to view graphs."})
  
  observe({
    shinyjs::show("text2")
    
    if(!is.null(input$run))
      shinyjs::hide("text2")
  })
  
#Meta Data----------------------------------------------------------------------
  
  meta <- reactive({
    meta <- dbGetQuery(con, 'SELECT * FROM channelmeta')
    meta$ChannelName <- sub("_", " ", meta$ChannelName)
    meta
  })
  
#Data---------------------------------------------------------------------------
  chan1 <- reactive(get_channel_data(1, input$Select2, downsamp = TRUE))
  chan2 <- reactive(get_channel_data(2, input$Select2))
  chan3 <- reactive(get_channel_data(3, input$Select2))
  chan4 <- reactive(get_channel_data(4, input$Select2))
  chan5 <- reactive(get_channel_data(5, input$Select2))
  chan6 <- reactive(get_channel_data(6, input$Select2))
  chan7 <- reactive(get_channel_data(7, input$Select2))
  chan8 <- reactive(get_channel_data(8, input$Select2))
  
  chans <- reactive(list(chan1(), chan2(), chan3(), chan4(), chan5(), chan6(), chan7(), chan8()))
  
  
#Figure-------------------------------------------------------------------------
  output$fig <- renderPlotly({
    plot1 <- basic_plot(chan1(), range(), meta(), annots(), input$Select2, "red")
    plot2 <- basic_plot(chan2(), range(), meta(), annots(), input$Select2, "blue")
    plot3 <- basic_plot(chan3(), range(), meta(), annots(), input$Select2, "yellowgreen")
    plot4 <- basic_plot(chan4(), range(), meta(), annots(), input$Select2, "violet")
    plot5 <- basic_plot(chan5(), range(), meta(), annots(), input$Select2, "purple")
    plot6 <- basic_plot(chan6(), range(), meta(), annots(), input$Select2, "mediumspringgreen")
    plot7 <- basic_plot(chan7(), range(), meta(), annots(), input$Select2, "peru")
    plot8 <- basic_plot(chan8(), range(), meta(), annots(), input$Select2, "mediumpurple")
    
    p <- subplot(list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8), 
                 nrows = 8, shareX = TRUE, margin = 0.02) %>% layout(xaxis = list(title = 'Time (sec)'))
    p <- hide_legend(p)
  })
  
#Annotations--------------------------------------------------------------------
  
  annots <- reactive(get_annots(input$Select2))
  output$annots <- renderTable(annots())
  
#Pressure Flow Curve------------------------------------------------------------
  
  df_breaths <- reactive({
    
    t_start <- range_flow()[1]
    
    minimun_breath_dur = 2.2
    meta <- meta()
    chans_new <- limit_range(chans(), range_flow())
    chan <- paste0(1, input$Select2)
    
    rate_stim <- meta[meta$ChannelID == chan,'SampleRate']
    sample_rate <- unique(meta[meta$SampleRate != rate_stim, 'SampleRate'])
    
    meta2 <- subset(meta, grepl('MH$', meta$ChannelID))
    
    chan_rip_abd <- which(meta2$ChannelName == 'RIP Abd')
    sig_rip_abd <- chans_new[[chan_rip_abd]]$Value
    
    chan_rip_chst <- which(meta2$ChannelName == 'RIP Thorax')
    sig_rip_chst <- chans_new[[chan_rip_chst]]$Value
    
    chan_epi <- which(meta2$ChannelName == 'Pepi')
    sig_epi <- chans_new[[chan_rip_abd]]$Value
    
    chan_flow <- which(meta2$ChannelName == 'Flow')
    sig_flow <- chans_new[[chan_flow]]$Value
    
    chan_stim <- which(meta2$ChannelName == 'Stim Monitor')
    sig_stim_lfilt <- chans_new[[chan_stim]]$Value
    
    chan_cpap <- which(meta2$ChannelName == 'CPAP Pressure')
    sig_cpap <- chans_new[[chan_cpap]]$Value
    
    model <- build_model_inception(input_shape = c(299,299,3), num_classes=3)
    
    df <- get_cycles(sig_rip_abd, sig_rip_chst, sig_epi, sig_flow, minimun_breath_dur, sample_rate, 
                     signal_type = 'both', poly_order = 6, min_cycle = 8, rise_rate = 0.04)
    
    df2 <- recheck_cycles(df, sample_rate, minimun_rate = 0.5)
    
    df3 <- get_baseline_portions(df2, sig_flow)
    
    df3['Exp.flow.mean'] <- apply(df3, 1, get_baseline, vector = sig_flow)
    df3['Exp.chst.mean'] <- apply(df3, 1, get_baseline, vector = sig_rip_chst)
    df3['Exp.abd.mean'] <- apply(df3, 1, get_baseline, vector = sig_rip_abd)
    
    
    df4 <- get_measurement(df3, sig_flow, sig_rip_chst, sig_rip_abd, sig_stim_lfilt, sig_epi, sample_rate, t_start, sig_cpap)
    df5 <- data_analysis(df4)
    df6 <- detect_airway_status_dl(sig_flow, df5, model)
    df_breaths <- data_analysis(df6)
    df_breaths
  })
  
  output$table_curve1 <- renderTable({
    dat <- get_plot_data(df = df_breaths())
    as.data.frame(dat[,1])
    })
  output$table_curve2 <- renderTable({
    dat <- get_plot_data(df = df_breaths())
    as.data.frame(dat[,2])
  })
  output$table_curve3 <- renderTable({
    dat <- get_plot_data(df = df_breaths())
    as.data.frame(dat[,3])
  })
  
  output$flow_curve <- renderPlotly({
    plot_pressure_flow_curve(df = df_breaths(), outside = input$outside, curve = input$curve)
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
                 plotlyOutput('fig', height = 800)),
        tabPanel("Pressure Flow Curve", 
                 br(), textOutput('text2'),
                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div("Loading...",id="loadmessage")),
                 plotlyOutput('flow_curve'),
                 tableOutput('table_curve1'), tableOutput('table_curve2'), tableOutput('table_curve3')),
        tabPanel("Annotations", 
                 br(), tableOutput('annots'))
      )
  })
}

#Run-App------------------------------------------------------------------------
shinyApp(ui,server)