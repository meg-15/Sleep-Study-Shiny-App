#Packages-----------------------------------------------------------------------
library(plotly)
library(pracma)

#Uploading analysis file created from Yike's code as csv------------------------
  breaths <- read.csv('MH-14-06.csv')
  
#Function Bank------------------------------------------------------------------
  
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
      marker_type = 'square'
    }
    
    fl_list <- list(x = c(data[data$airway == 'FL',]$x_arr), y = c(data[data$airway == 'FL',]$y_arr), 
                    name = paste(stim_mode, 'FL'), mode = 'markers',
                    marker = list(size = lbl_size, color = marker_color, opacity = 0.3, line = list(color = marker_color, width = 2)), 
                    symbols = marker_type)
    
    apnea_list <- list(x = c(data[data$airway == 'Apnea',]$x_arr), y = c(data[data$airway == 'Apnea',]$y_arr), 
                       name = paste(stim_mode, 'Apnea'), mode = 'markers',
                       marker = list(size = lbl_size, color = marker_color, opacity = 0.3, line = list(color = marker_color, width = 2)), 
                       symbols = marker_type)
    
    nfl_list <- list(x = c(data[data$airway == 'NFL',]$x_arr), y = c(data[data$airway == 'NFL',]$y_arr), 
                     name = paste(stim_mode, 'NFL'), mode = 'markers',
                     marker = list(size = lbl_size, color = 'white', opacity = 0.7, line = list(color = marker_color, width = 2)), 
                     symbols = marker_type)
    
    return(list(fl = fl_list, apnea = apnea_list, nfl = nfl_list))
  }
  
  
  vndrMat <- function(x, n) matrix(rep(as.vector(x)[[1]], each = n) ^ rev((seq_len(n) - 1)), ncol = n, byrow = TRUE)
  
  polyfit_mine <- function(x, y, deg) {
    order = deg + 1
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    
    #Check Arguments
    if (deg < 0){noquote("Error: Expected deg >= 0.")}
    if (ncol(x) != 1){noquote("Error: Expected 1D data frame for x.")}
    if (ncol(y) < 1 | ncol(y) > 2){noquote("Error: Expected 1D or 2D data frame for y.")}
    if (nrow(y) != nrow(x)){noquote("Error: Expected x and y to have same length.")}
    
    #Set up least squares equation for powers of x
    lhs <- vndrMat(x, order)
    rhs <- y
    
    #Scale lhs to improve condition number and solve
    scale <- sqrt(colSums(lhs*lhs))
    for (i in 1:ncol(lhs)) {
      lhs[,i] <- lhs[,i]/scale[i]
    }  
    
    c <- lm(as.matrix(rhs) ~ lhs - 1) ##
    
    c <- as.vector(na.omit(c$coefficients))
    
    if (length(c) == 1) {c <- c(c/2, c/2)}
    
    return(c/scale)
    
  }
  
  
  show_curve <- function(data) {
    
    plt <- vector(mode='list')
    legend <- vector(mode='list')
    
    for (i in seq(len)){
      name <- colnames(dat)[i]
      df_temp <- as.data.frame(data[,i])
      x <- df_temp$x_arr
      y <- df_temp$y_arr
      z <- polyfit_mine(x, y, 1)
      x_zero <- -z[2]/z[1]
      
      x_nfl_min <- ifelse(is.infinite(min(df_temp[df_temp$airway == 'NFL',]$x_arr)), NA, min(df_temp[df_temp$airway == 'NFL',]$x_arr))
      
      x_open <- max(c((y.open - z[2])/z[1], x_nfl_min), na.rm = TRUE)
      
      x_plt <- c(x_zero, x_open)
      
      popen <- (y.open - z[2])/z[1]
      
      line1 <- paste0(name, ": y = ", round(z[1], 2), "x + ", round(z[2], 2))
      line2 <- paste0("Pcrit: ", round(x_zero, 2), "; Popen: ", round(popen, 2))

      
      legend[[name]] <- data.frame(line1, line2)
      
      plt[[name]] <- data.frame(x = x_plt, y = c(0, y.open))
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

    
  

#Plot_Pressure_Flow_Curve-------------------------------------------------------
  

  plot_pressure_flow_curve <- function(df, reviewed = FALSE, outside = FALSE, curve = TRUE) {
    fig <- plot_ly(type = 'scatter', mode = 'marker') 
    stim_mode_list <- unique(df$Stim.mode.final)
    airway_lbl <- 'Airway.status.DL'
    stim_lbl <- 'Stim.mode'
    if (reviewed == TRUE) {
      airway_lbl <- 'Airway.status.final'
      stim_lbl <- 'Stim.mode.final'
    }
    min_nfl_list <- sapply(stim_mode_list, stim_loop1, df1 = df, stim = stim_lbl, airway = airway_lbl)
    min_nfl_list <- min_nfl_list[!is.na(min_nfl_list)]
    y.open <- mean(unlist(min_nfl_list)) * 1000/60
    dat <- sapply(stim_mode_list, stim_loop2, df2 = df, stim = stim_lbl, airway = airway_lbl)
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
      x <- show_curve(dat)
      
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
                          xaxis = list(title = 'CPAP Pressure (cm H20)'),
                          yaxis = list(title = 'Peak Inspiratory Flow (mL/sec)'),
                          legend = list(x = 0.75, y = 0.95, font = list(size = 14)))
    
    return(fig)
  }
  
plot_pressure_flow_curve(df = breaths, reviewed = TRUE, outside = TRUE, curve = TRUE)
  