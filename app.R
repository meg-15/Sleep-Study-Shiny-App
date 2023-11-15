#Packages-----------------------------------------------------------------------
# Install and load required packages
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("stringi", quietly = TRUE)) install.packages("stringi")

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(glue) 
library(DBI)
library(RSQLite)
library(shinyjs)
library(plotly)
library(reticulate)
library(dplyr)
library(stringr)
library(stringi)
library(fresh)


#Python Environment-------------------------------------------------------------

use_virtualenv('my-python', required = TRUE)
source_python("C:/Users/megan/Desktop/Summer-2023-Project/Sleep-Study-Shiny-App/reference.py")

np <- import("numpy", convert = TRUE)

#Options------------------------------------------------------------------------
#options(shiny.maxRequestSize=4000*1024^2)

#Connection---------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/megan/Desktop/Summer-2023-Project/Sleep-Study-Shiny-App/data_new3.db") 
#need to add 'disconnect when close app' option

#Setting Up Annotations---------------------------------------------------------
#Create Responses Table
annots_new <- data.frame(AnnotationID = numeric(),
                         SubjectID = numeric(),
                         ChannelID = numeric(),
                         Time = numeric(), 
                         Annotation = character(),
                         stringsAsFactors = FALSE)

#Add Responses to 'annots'
dbWriteTable(con, "annots", annots_new, overwrite = FALSE, append = TRUE)

#Label Mandatory Fields---------------------------------------------------------
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

#Functions Bank-----------------------------------------------------------------

  #Generally, the following abbreviations apply to the rest of the app:
    #sub = SubjectID
    #chan = ChannelID
  
#Extracts data from a specific channel in database
get_channel_data <- function(chan, sub, met, downsamp = FALSE){   

  sql_data <- glue_sql("
    SELECT *
    FROM channeldata
    WHERE ChannelID = {chan} AND SubjectID = {sub}
  ", .con = con)
  
  data <- dbGetQuery(con, sql_data)
  
  if (downsamp == TRUE) {
    meta <- met
    
    rate_stim <- meta[meta$ChannelID == chan,'SampleRate'][1]
    rate <- unique(meta[meta$SampleRate != rate_stim, 'SampleRate']) 
    
    data$Value <- low_pass_filter(data$Value, rate = rate_stim, cutoff = 1)
    
    data2 <- down_sample(sig_stim_lfilt = data$Value, sample_rate = rate) 
    
    list <- which(!is.na(match(data$Value, data2)))
    
    data <- data[list,]
  }
  
  return(list(channel = chan, data = data)) 
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
  channels <- c(chan, paste0('0', sub), paste0('-1', sub))
  annots <- cbind(annots, ChannelID = sub('_[^_]*$', '', annots$AnnotationID))
  
  for (channel in channels) {         
    for (n in 1:nrow(annots)) {
      if (annots$ChannelID[n] == channel &&
          annots$Time[n] > time_range[1] &&
          annots$Time[n] < time_range[2]) {
        fig <- fig %>% add_trace(x = annots$Time[n], type = 'scatter', mode = 'lines',
                                 line = list(color = "darkgrey"), text = annots$Annotation[n])
      }
    }
  }
  
  return(fig)
  
}

#Extracts data from a specific channel in database
get_annots <- function(sub){   

  sql_annots <- glue_sql("
    SELECT *
    FROM annots
    WHERE SubjectID = {sub}
  ", .con = con)
  
  annots <- dbGetQuery(con, sql_annots)
  #annots$Time <- round(annots$Time, 2)
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
  chans_new <- vector(mode = 'list', length = length(chans_dat))
  for(n in seq_along(chans_dat)){
    c <- chans_dat[[n]][[2]]
    min <- which(c$Time == time_range[1])
    max <- which(c$Time == time_range[2])
    chans_new[[n]] <- c[min:max,]
  }
  return(chans_new)
}

stim_loop1 <- function(df1, stim_mode, stim = stim_lbl, airway = airway_lbl){
  df_set <- df1[df1[stim] == stim_mode & df1[airway] == 'NFL',]
  min_nfl_p <- suppressWarnings(ifelse(is.infinite(min(df_set$CPAP.ceil)), NA, min(df_set$CPAP.ceil)))
  y_nfl <- suppressWarnings(ifelse(is.na(min_nfl_p), NA, mean(df_set$Flow.max.adj[df_set$CPAP.ceil == min_nfl_p])))
  return(y_nfl)
}

stim_loop2 <- function(df2, stim_mode, stim = stim_lbl, airway = airway_lbl) {
  
  df_set <- df2[df2[stim] == stim_mode,]
  num.c <- ncol(df_set)
  
  fl_rows <- which(df_set[airway] == 'FL')
  fl <- df_set[fl_rows, c('Ins start', 'Ins end', 'CPAP.ceil', 'Flow.max.adj', airway)]
  fl['Flow.max.adj'] <- fl['Flow.max.adj'] * 1000/60
  fl <- cbind(fl, Annotations = df_set[fl_rows, 7:num.c])
  
  max_apnea_p <- suppressWarnings({ifelse(is.infinite(max(df_set[df_set[airway] == 'Apnea', 'CPAP.ceil'])), NA, max(df_set[df_set[airway] == 'Apnea', 'CPAP.ceil']))})
  min_nfl_p <- suppressWarnings({ifelse(is.infinite(min(df_set[df_set[airway] == 'NFL', 'CPAP.ceil'])), NA, min(df_set[df_set[airway] == 'NFL', 'CPAP.ceil']))})
  
  ap_rows <- which(df_set[airway] == 'Apnea' & df_set$CPAP.ceil == max_apnea_p)
  ap <- df_set[ap_rows, c('Ins start', 'Ins end', 'CPAP.ceil', 'Flow.max.adj', airway)]
  ap['Flow.max.adj'] <- ap['Flow.max.adj'] * 1000/60
  ap <- cbind(ap, Annotations = df_set[ap_rows, 7:num.c])
  
  nfl_rows <- which(df_set[airway] == 'NFL' & df_set$CPAP.ceil == min_nfl_p)
  nfl <- df_set[nfl_rows, c('Ins start', 'Ins end', 'CPAP.ceil', 'Flow.max.adj', airway)]
  nfl['Flow.max.adj'] <- nfl['Flow.max.adj'] * 1000/60
  nfl <- cbind(nfl, Annotations = df_set[nfl_rows, 7:num.c])
  
  data <- rbind(fl, ap, nfl)
  colnames(data) <- c('Ins start', 'Ins end', 'x_arr', 'y_arr', 'airway', 'Annotations')
  
  return(data)
}

figure <- function(data, stim_mode, lbl_size = 15, stim = stim_lbl, airway = airway_lbl) {
  
  dat <- as.data.frame(data[,colnames(data) == stim_mode])
  
  dat[,'Annotations'] <- tolower(dat[,'Annotations'])
  
  dat[,'Status'] <- ifelse(is.na(str_extract(dat[,'Annotations'], "test")), 'Inside', 'Remove')
  tab <- dat
  dat <- dat[dat$Status == 'Inside',]
  
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
  
  nfl_list <- list(x = c(dat[dat$airway == 'NFL',]$x_arr), y = c(dat[dat$airway == 'NFL',]$y_arr), 
                   name = paste(stim_mode, 'NFL'), mode = 'markers',
                   marker = list(size = lbl_size, color = 'white', opacity = 0.7, line = list(color = marker_color, width = 2)), 
                   symbols = marker_type)
  
  return(list(list(fl = fl_list, apnea = apnea_list, nfl = nfl_list), tab))
}

show_curve <- function(data, y.open) {
  
  plt <- vector(mode='list')
  legend <- vector(mode='list')
  
  for (i in seq(ncol(data))){
    name <- colnames(data)[i]
    df_temp <- as.data.frame(data[,i])
    x <- df_temp$x_arr
    y <- df_temp$y_arr
    
    if(length(x) >= 2){
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
    } else {
      line1 <- paste0("Too few points to calculate line.")
      line2 <- paste0("Pcrit: NA; Popen: NA")
      legend[[name]] <- data.frame(line1, line2)
      plt[[name]] <- data.frame(x = c(0,1), y = c(0,1))
    }
    
    

  }
  
  return(list(plt, legend))
}

show_outside <- function(df4, stim_mode, stim = stim_lbl, airway = airway_lbl) {
  
  df_set <- df4[df4[stim] == stim_mode,]
  num.c <- ncol(df_set)
  
  max_apnea_p <- suppressWarnings({max(df_set[df_set[airway] == 'Apnea', 'CPAP.ceil'])})
  min_nfl_p <- suppressWarnings({min(df_set[df_set[airway] == 'NFL', 'CPAP.ceil'])})
  
  ap_rows <- which(df_set[airway] == 'Apnea' & df_set$CPAP.ceil < max_apnea_p | df_set[airway] == 'Dogleg')
  ap <- df_set[ap_rows, c('Ins start', 'Ins end', 'CPAP.ceil', 'Flow.max.adj', airway)]
  ap['Flow.max.adj'] <- ap['Flow.max.adj'] * 1000/60
  ap <- cbind(ap, Annotaions = df_set[ap_rows, 7:num.c])
  
  nfl_rows <- which(df_set[airway] == 'NFL' & df_set$CPAP.ceil > min_nfl_p)
  nfl <- df_set[nfl_rows, c('Ins start', 'Ins end', 'CPAP.ceil', 'Flow.max.adj', airway)]
  nfl['Flow.max.adj'] <- nfl['Flow.max.adj'] * 1000/60
  nfl <- cbind(nfl, Annotaions = df_set[nfl_rows, 7:num.c])
  
  
  data <- rbind(ap, nfl)
  colnames(data) <- c('Ins start', 'Ins end', 'x_arr', 'y_arr', 'airway', 'Annotations')
  
  return(data)
  
}

figure_outside <- function(data, stim_mode, lbl_size = 15, stim = stim_lbl, airway = airway_lbl) {
  
  dat <- as.data.frame(data[, stim_mode])
  
  dat[,'Annotations'] <- tolower(dat[,'Annotations'])
  
  dat[,'Status'] <- ifelse(is.na(str_extract(dat[,'Annotations'], "test")), 'Outside', 'Remove')
  tab <- dat
  dat <- dat[dat$Status == 'Outside',]
  
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
  
  apnea_list <- list(x = c(dat[dat$airway == 'Apnea',]$x_arr), y = c(dat[dat$airway == 'Apnea',]$y_arr), 
                     name = paste(stim_mode, 'Apnea Discard'), mode = 'markers',
                     marker = list(size = lbl_size, color = marker_color, opacity = 0.9, line = list(color = marker_color, width = 2)), 
                     symbols = marker_type) #wrong?
  
  nfl_list <- list(x = c(dat[dat$airway == 'NFL',]$x_arr), y = c(dat[dat$airway == 'NFL',]$y_arr), 
                   name = paste(stim_mode, 'NFL Discard'), mode = 'markers',
                   marker = list(size = lbl_size, color = marker_color, opacity = 0.9), 
                   symbols = marker_type) 
  
  return(list(list(apnea = apnea_list, nfl = nfl_list), tab))
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


pressure_flow_curve_data <- function(df, reviewed = FALSE) {

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

  outside <- sapply(stim_mode_list, show_outside, df4 = df, stim = stim_lbl, airway = airway_lbl)
  outside_atts <- sapply(stim_mode_list, figure_outside, data = outside, stim = stim_lbl, airway = airway_lbl)
  
  x <- show_curve(dat, y.open)
  
  return(list(fig_atts, outside_atts, x, y.open))
}


plot_pressure_flow_curve <- function(df, outside = FALSE, curve = FALSE) {
  fig <- plot_ly(type = 'scatter', mode = 'marker') 
  
  fig_atts <- df[[1]][1,]
  outside_atts <- df[[2]][1,]
  x <- df[[3]]
  y.open <- df[[4]]
  
  #stim_mode_list <- unique(df[,stim_lbl])
  
  for (i in seq_along(fig_atts)){
    for(c in seq_along(fig_atts[[i]])){
      fig <- add_trace(fig, x=fig_atts[[i]][[c]]$x, y=fig_atts[[i]][[c]]$y, 
                      name=fig_atts[[i]][[c]]$name, mode=fig_atts[[i]][[c]]$mode,
                      marker = list(size = fig_atts[[i]][[c]]$marker$size, color = fig_atts[[i]][[c]]$marker$color,
                                    opacity = fig_atts[[i]][[c]]$marker$opacity, 
                                    line = list(color = fig_atts[[i]][[c]]$marker$line$color, width = fig_atts[[i]][[c]]$marker$line$width),
                                    symbol = fig_atts[[i]][[c]]$symbols),
                      showlegend = FALSE)
    }
  }
  
  if(outside == TRUE) {
    
    for (i in seq_along(outside_atts)){
      for(c in seq_along(outside_atts[[i]])){
        fig <- add_trace(fig, x=outside_atts[[i]][[c]]$x, y=outside_atts[[i]][[c]]$y, 
                         name=outside_atts[[i]][[c]]$name, mode=outside_atts[[i]][[c]]$mode,
                         marker = list(size = outside_atts[[i]][[c]]$marker$size, color = outside_atts[[i]][[c]]$marker$color,
                                       opacity = outside_atts[[i]][[c]]$marker$opacity, 
                                       line = list(color = outside_atts[[i]][[c]]$marker$line$color, width = outside_atts[[i]][[c]]$marker$line$width),
                                       symbol = outside_atts[[i]][[c]]$symbols),
                         showlegend = FALSE)
      }
    }
  }
  
  if(curve == TRUE){
    
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
                        xaxis = list(title = list(text = 'CPAP Pressure (cm H20)', standoff = 10), range = list(-1, 20)),
                        yaxis = list(title = 'Peak Inspiratory Flow (mL/sec)'),
                        legend = list(orientation = "h",
                                      xanchor = 'center', x =0.5,
                                      font = list(size = 14)))
  
  return(fig)
}

#UI-----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "VUMC Sleep Study Database"),
  dashboardSidebar(
    useShinyjs(),
    #Select Options 
    uiOutput("selectvar"),
    br(),
    #Time Options
    h5(style="margin-left: 18px", strong("Select Time Range (sec):")),
    #Below appears after app loads
    #conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #                 tags$div("Loading...",id="loadmessage")),
    div(
      style = "text-indent: 18px;",
      textOutput("time_range", container = tags$style),
      id = "indented_time_range"
    ),
    uiOutput("select_time1"),
    uiOutput("select_time2"),
    hr(), 
    h5(style="margin-left: 18px", strong("Raw Signals")),
    actionButton("run", "Plot Raw Signals"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    hr(),
    #conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #                 tags$div("Loading...",id="loadmessage")),
    h5(strong(style="margin-left: 18px", "Pressure Flow Curve")),
    actionButton("run2", "Plot Pressure Flow Curve"),
    checkboxInput("curve", "Show curve?", FALSE),
    checkboxInput("outside", "Show outside?", FALSE)
  ),
  dashboardBody(
    if(is.null(con)) {return()}
    else
    fluidRow(
      box(
        title = "Raw Signals",
        textOutput('text'),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        plotlyOutput("fig", height = 800),
        width = 6
      ),
      box(
        title = "Annotations",
        fluidRow(
          column(3,
                 div(
                   id = "form",
                   
                   textInput("ChannelNumber", labelMandatory("ChannelNumber"), placeholder = "", width = "150px"),
                   numericInput("AnnotationNumber", labelMandatory("AnnotationNumber"), value = 0, width = "150px"),
                   numericInput("Time", labelMandatory("Time"), value = 0, width = "150px"),
                   textAreaInput("Annotation", labelMandatory("Annotation"), placeholder = "", height = 100, width = "150px"),
                   helpText(labelMandatory(""), paste("Mandatory field.")),
                   uiOutput("submit_edit"),
                   actionButton("submit", "Submit", class = "btn-primary"),
                   br(),
                   br(),
                   actionButton("edit_button", "Edit", class = "btn-primary"),
                   actionButton("delete_button", "Delete", class = "btn-primary"),
                   shinyjs::hidden(
                     span(id = "submit_msg", "Submitting..."),
                     div(id = "error",
                         div(br(), tags$b("Error: "), span(id = "error_msg"))
                     )
                   ))
          ),
          column(9,
                 dataTableOutput("responses_table")
          )),
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "Pressure Flow Curve",
        textOutput('text2'),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        plotlyOutput("flow_curve", height = 500),
        width = 6
      ),
      box(
        title = "Breaths",
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        tableOutput("cycles"),
        width = 6
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
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
    query_sub = paste0("SELECT TIME FROM channeldata WHERE ChannelID = 2 and SubjectID =", input$Select2)
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
    paste0("     The time range for the selected      subject is ",
           poss_time()[1], " to ", poss_time()[2], " seconds.")
    })
  
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
  observeEvent(input$run2,{time_flow$time1 <- input$Time1}) #changed run to run2
  observeEvent(input$run2,{time_flow$time2 <- input$Time2})
  
  #Set Time Range When Button is Pressed (also renders error messages for invalid values)
  range_flow <- eventReactive(input$run2, {
    validate(need(time_flow$time1 >= poss_time()[1], "Start time is out of range.")) 
    #start time must be greater than or equal to smallest possible time
    validate(need(time_flow$time2 <= poss_time()[2], "End time is out of range."))
    #end time must be less than or equal to greatest possible time
    c(as.numeric(time_flow$time1), as.numeric(time_flow$time2))
  })
  
  #Hide Flow Curve Button Until Time is Loaded #HELP
  observe({ 
    shinyjs::hide("run2")
    if(!is.null(input$Time1))
      shinyjs::show("run2")
  })
  
  observe({ 
    shinyjs::hide("curve")
    if(!is.null(input$Time1))
      shinyjs::show("curve")
  })
  
  observe({ 
    shinyjs::hide("outside")
    if(!is.null(input$Time1))
      shinyjs::show("outside")
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
    
    if(!is.null(input$run2))
      shinyjs::hide("text2")
  })
  
#Meta Data----------------------------------------------------------------------
  
  meta <- reactive({
    meta <- dbGetQuery(con, 'SELECT * FROM channelmeta')
    meta$ChannelName <- sub("_", " ", meta$ChannelName)
    meta
  })
  
#Data---------------------------------------------------------------------------
  chan1 <- reactive(get_channel_data(1, input$Select2, meta(), downsamp = TRUE))
  chan2 <- reactive(get_channel_data(2, input$Select2, meta()))
  chan3 <- reactive(get_channel_data(3, input$Select2, meta()))
  chan4 <- reactive(get_channel_data(4, input$Select2, meta()))
  chan5 <- reactive(get_channel_data(5, input$Select2, meta()))
  chan6 <- reactive(get_channel_data(6, input$Select2, meta()))
  chan7 <- reactive(get_channel_data(7, input$Select2, meta()))
  chan8 <- reactive(get_channel_data(8, input$Select2, meta()))
  
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
    p
  })
  
#Pressure Flow Curve------------------------------------------------------------
  
  model <- reactive({build_model_inception(input_shape = c(299,299,3), num_classes=3)}) 
    #needs to be outside the df_breaths reactive element so that the program does not rebuild the model everytime
  
  df_breaths <- reactive({ 
    
    model <- model()
    range <- range_flow()
    
    minimun_breath_dur = 2.2
    meta <- meta()
    chans_new <- limit_range(chans(), range)
    #chan <- paste0(1, subj())
    
    annots <- annots()
    annots <- annots[which(annots$Time>range[1] & annots$Time<range[2]),]
    
    rate_stim <- meta[meta$ChannelID == 1,'SampleRate']
    sample_rate <- unique(meta[meta$SampleRate != rate_stim, 'SampleRate'])
    
    chan_rip_abd <- which(meta$ChannelName == 'RIP Abd')
    sig_rip_abd <- chans_new[[chan_rip_abd]]$Value
    
    chan_rip_chst <- which(meta$ChannelName == 'RIP Thorax')
    sig_rip_chst <- chans_new[[chan_rip_chst]]$Value
    
    chan_epi <- which(meta$ChannelName == 'Pepi')
    sig_epi <- chans_new[[chan_rip_abd]]$Value
    
    chan_flow <- which(meta$ChannelName == 'Flow')
    sig_flow <- chans_new[[chan_flow]]$Value
    
    chan_stim <- which(meta$ChannelName == 'Stim Monitor')
    sig_stim_lfilt <- chans_new[[chan_stim]]$Value
    
    chan_cpap <- which(meta$ChannelName == 'CPAP Pressure')
    sig_cpap <- chans_new[[chan_cpap]]$Value
    
    df <- get_cycles(sig_rip_abd, sig_rip_chst, sig_epi, sig_flow, minimun_breath_dur, sample_rate, 
                     signal_type = 'both', poly_order = 6, min_cycle = 8, rise_rate = 0.04)
    
    df2 <- recheck_cycles(df, sample_rate, minimun_rate = 0.5)
    
    cycles <- df2
    cycles[,1:2] <- (cycles[,1:2]/sample_rate) + range[1]
    c_annot <- lapply(1:nrow(cycles), function(c) {
      annots$Annotation[annots$Time >= cycles[c, 1] & annots$Time <= cycles[c, 2]]
    })
    max_len <- max(max(sapply(c_annot, length)), 1)
    c_annot2 <- matrix(NA, nrow = nrow(cycles), ncol = max_len)
    
    # Fill in matrix with annotations
    for (c in 1:nrow(cycles)) {
      if(length(c_annot[[c]]) == 0) {c_annot[[c]] <- NA}
      c_annot2[c, 1:length(c_annot[[c]])] <- c_annot[[c]]
    } 
    
    cycles <- cbind(cycles, c_annot2)
    
    df3 <- get_baseline_portions(df2, sig_flow)
    
    df3['Exp.flow.mean'] <- apply(df3, 1, get_baseline, vector = sig_flow)
    df3['Exp.chst.mean'] <- apply(df3, 1, get_baseline, vector = sig_rip_chst)
    df3['Exp.abd.mean'] <- apply(df3, 1, get_baseline, vector = sig_rip_abd)
    
    
    df4 <- get_measurement(df3, sig_flow, sig_rip_chst, sig_rip_abd, sig_stim_lfilt, sig_epi, sample_rate, range[1], sig_cpap)
    df5 <- data_analysis(df4)
    df6 <- detect_airway_status_dl(sig_flow, df5, model)
    df_breaths <- data_analysis(df6)
    
    reviewed = FALSE
    airway_lbl <- 'Airway.status.DL'
    stim_lbl <- 'Stim.mode'
    if (reviewed == TRUE) {
      airway_lbl <- 'Airway.status.final'
      stim_lbl <- 'Stim.mode.final'
    }
    num.c <- ncol(cycles)
    df_breaths <- cbind(cycles[,1:2], df_breaths[,c(airway_lbl, stim_lbl, 'Flow.max.adj', 'CPAP.ceil')], Annotations = cycles[,4:num.c])
    p = pressure_flow_curve_data(df = df_breaths)
    return(p)
  })
  
  
  output$flow_curve <- renderPlotly({
    plot_pressure_flow_curve(df_breaths(), outside = input$outside, curve = input$curve)
  })
  
  #Cycles Table-------------------------------------------------------------------
  
  #reviewed <- reactiveVal(FALSE)
  
  output$cycles <- renderTable({
    df <- df_breaths()
    df2 <- rbind(df[[1]][[2]], df[[2]][[2]])
    colnames(df2) <- c('Ins. Start', 'Ins. End', 'CPAP Pressure', 'PIF', 'Airway Status', 'Annot.', 'Plot Status')
    df2[order(df2[,'Ins. Start']),]
  })
  
#Annotations--------------------------------------------------------------------  
  #load annots and make reactive to inputs  
  annots <- reactive({
    input$submit
    input$submit_edit
    input$delete_button
    get_annots(input$Select2)
  })  
  
  #List of mandatory fields for submission
  fieldsMandatory <- c("ChannelNumber", "AnnotationNumber", "Time", "Annotation")
  
  #define which input fields are mandatory 
  observe({
    
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  fieldsAll <- c("ChannelNumber", "AnnotationNumber", "Time", "Annotation")
  
  #save form data into data_frame format
  formData <- reactive({
    
    formData <- data.frame(AnnotationID = input$AnnotationNumber,
                           SubjectID = input$Select2,
                           ChannelID = input$ChannelNumber,
                           Time = input$Time, 
                           Annotation = input$Annotation,
                           stringsAsFactors = FALSE)
    return(formData)
    
  })
  
  #Add Data Functions-------------------------------------------------------------
  appendData <- function(data){
    quary <- sqlAppendTable(con, "annots", data, row.names = FALSE)
    dbExecute(con, quary)
  }
  
  observeEvent(input$submit, priority = 20,{
    appendData(formData())
    updateTextInput(session, "ChannelNumber", value = " ")
    updateNumericInput(session, "AnnotationNumber", value = 0)
    updateNumericInput(session, "Time", value = 0)
    updateTextAreaInput(session, "Annotation", value = " ")
  })
  
  #Delete Data Functions ---------------------------------------------------------#HELP
  deleteData <- reactive({
    SQL_df <- dbReadTable(con, "annots")
    aID_select <- dbQuoteLiteral(con, SQL_df[input$responses_table_rows_selected, "AnnotationID"])
    sub_select <- dbQuoteLiteral(con, SQL_df[input$responses_table_rows_selected, "SubjectID"])
    
    query <- paste0("DELETE FROM annots WHERE `AnnotationID` = ", aID_select, " AND `SubjectID` = ", sub_select )
    #query
    dbExecute(con, query)
    
    #dbReadTable(con, 'annots')
    
  })
  
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      deleteData()
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  
#Edit Data Functions------------------------------------------------------------
  observeEvent(input$edit_button, priority = 20,{
    
    hide("submit")
    show("submit_edit")
    
    output$submit_edit <- renderUI({
      actionButton("submit_edit", label = "Submit Edit", , class = "btn-primary")
    })
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select only one row." ),easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Please select a row." ),easyClose = TRUE)
      })
    
    if(length(input$responses_table_rows_selected) == 1 ) {
      SQL_df <- get_annots(input$Select2)
      row_idx <- input$responses_table_rows_selected
      
      updateTextInput(session, "ChannelNumber", value = SQL_df[row_idx, "ChannelID"]) 
      updateNumericInput(session, "AnnotationNumber", value = SQL_df[row_idx, "AnnotationID"])
      updateNumericInput(session, "Time", value = SQL_df[row_idx, "Time"])
      updateTextAreaInput(session, "Annotation", value = SQL_df[row_idx, "Annotation"])
    }
  }) 
  
  observeEvent(input$submit_edit, priority = 20, {
    
    SQL_df <- annots()
    aID_select <- SQL_df[input$responses_table_row_last_clicked, "AnnotationID"] 
    sub_select <- SQL_df[input$responses_table_row_last_clicked, "SubjectID"] 
    
    query <- paste0("UPDATE annots SET AnnotationID = ?, SubjectID = ?, ChannelID = ?, Time = ?, Annotation = ?
                    WHERE `AnnotationID` = ", aID_select, " AND `SubjectID` = ", sub_select )
    
    dbExecute(con, query, 
              param = list(input$AnnotationNumber,
                           input$Select2,
                           input$ChannelNumber,
                           input$Time,
                           input$Annotation))
    
    updateTextInput(session, "ChannelNumber", value = " ")
    updateNumericInput(session, "AnnotationNumber", value = 0)
    updateNumericInput(session, "Time", value = 0)
    updateTextAreaInput(session, "Annotation", value = " ")
    hide("submit_edit")
    show("submit")
    
  })
  
  
  #Annotations Table--------------------------------------------------------------
  output$responses_table <- DT::renderDataTable({
    
    table <- annots() 
    names(table) <- c("AnnotationID", "subjectID", "ChannelID", "Time", "Annotation")
    #table <- table %>% mutate(Time = round(Time, 2))
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(searching = TRUE, lengthChange = TRUE,
                                      pageLength = 10, scrollX = TRUE,
                                      lengthMenu = list(c(10, 25, -1), c('10', '25', 'ALL'))
                       ))
    table
  })
}

# Run the app
shinyApp(ui, server)
