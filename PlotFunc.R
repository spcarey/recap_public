

##### Plot Functions


##### DataFitForecastPlot
DataFitForecastPlot <- function(df = month.usage.data,
                                fm = fit.choice,
                                ID = "Forecast Plot",
                                subt = "Periodic Data with Fit and Forecast (80% & 95% CIs)",
                                cap = "Caption",
                                ylab = "Y Label",
                                llab = "Use Label",
                                t.period = "days") {
  
  #require(ggplot2)
  #require(grid)
  #require(stats)
  
  main.color <- "green3"
  
  #Prepare Data for plotting
  date <- df$Date
  elem.name <- colnames(df[2])
  temp.series <- data.frame("Date" = df$Date, "elem" = df[, 2])
  model.fit <- as.vector(fm[["fitted"]])
  
  ##Build plot data with forecast included
  #arrange new plot data
  forecast.mean <- c(rep(NA, length = length(fm$x)), fm$mean)
  forecast.lower80 <- c(rep(NA, length = length(fm$x)), fm$lower[, 1])
  forecast.upper80 <- c(rep(NA, length = length(fm$x)), fm$upper[, 1])
  forecast.lower95 <- c(rep(NA, length = length(fm$x)), fm$lower[, 2])
  forecast.upper95 <- c(rep(NA, length = length(fm$x)), fm$upper[, 2])
  #Use the last ts value a start point for forecast - eliminates 'gap' in chart
  forecast.mean[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.lower80[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.upper80[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.lower95[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.upper95[length(fm$x)] <- model.fit[length(fm$x)]
  
  ##Watch this line - may be data dependent
  date.Add <- seq(from = (seq(from = max(df$Date), by = t.period, length = 2)[2]),
                  by = t.period, length = length(fm$mean))
  Time.new <- c(date, date.Add)
  freq.new <- c(temp.series$elem, rep(NA, length(fm$mean)))
  model.fit.new <- c(model.fit, rep(NA, length(fm$mean)))
  
  #Trim the vectors to ensure no unfeasible solutions
  freq.new <- TrimVector(freq.new, minimum = 0)
  model.fit.new <- TrimVector(model.fit.new, minimum = 0)
  forecast.mean <- TrimVector(forecast.mean, minimum = 0)
  forecast.upper80 <- TrimVector(forecast.upper80, minimum = 0)
  forecast.lower80 <- TrimVector(forecast.lower80, minimum = 0)
  forecast.upper95 <- TrimVector(forecast.upper95, minimum = 0)
  forecast.lower95 <- TrimVector(forecast.lower95, minimum = 0)
  
  #Pull together the plot data
  plotdata <- data.frame(Time.new, freq.new, model.fit.new, forecast.mean,
                         forecast.lower80, forecast.upper80,
                         forecast.lower95, forecast.upper95)
  
  #Fix/adjust for timeperiod
  if (t.period == "years") {
    maj.breaks <- "1 year"
    min.breaks <- "1 year"
    date.lbls <- "%Y"
  } else {
    maj.breaks <- "3 months"
    min.breaks <- "1 month"
    date.lbls <- "%m-%Y"
  }
  
  ##Build the plot
  f2 <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = Time.new),
                        na.rm(TRUE)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast.lower95,
                                      ymax = forecast.upper95,
                                      fill = "gray80"),
                         color = "transparent", linetype = "blank", size = 1) +
    # ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast.lower80,
    #                                   ymax = forecast.upper80,
    #                                   fill = "gray70"),
    #                      color = "transparent", linetype = "blank", size = 1) +
    ggplot2::geom_col(ggplot2::aes(y = freq.new, fill = main.color)) +
    ggplot2::geom_line(ggplot2::aes(y = forecast.mean, color = "Forecast"),
                       linetype = "solid", size = 0.5) +
    # ggplot2::geom_point(ggplot2::aes(y = model.fit),
    #                     color = "black", shape = 20,
    #                     size = 3) + #add points to fit line
    ggplot2::geom_line(ggplot2::aes(y = model.fit.new,
                                    color = "Fit Line"),
                       linetype = "solid", size = 0.5) +
    # ggplot2::geom_point(ggplot2::aes(y = seasonal.model.fit.new),
    #                     color = "black", shape = 20,
    #                     size = 3) + #add points to trend line
    ggplot2::labs(title = ID, subtitle = subt, caption = cap, fill = NULL) +
    ggplot2::scale_x_date(name = "Date", #Axis title
                          date_breaks = maj.breaks, #Label interval/break
                          labels = "Dates", #Axis label
                          date_labels = date.lbls, #Date format
                          date_minor_breaks = min.breaks, #minor interval break (no label)
                          # limits = c(min(dates), max(dates)), #Axis limits
                          expand = c(0.01, 0.1), #How far to exen beyond range limits (for display)
                          position = "bottom") + #Sets label position to the bottom of plot
    ggplot2::scale_y_continuous(name = ylab, #Axis title
                                expand = c(0, 0), #No expansion beyond limits set for axis
                                position = "left") + #Left side of plot
    ggplot2::scale_colour_manual(name = "",
                                 values = c("Forecast" = "blue",
                                            "Fit Line" = "black")) +
    ggplot2::scale_fill_identity(name = "", guide = "legend",
                                 labels = c(#"80% Fore. Int.",
                                   "95% Fore. Int.",
                                   llab)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) + #Aligns titles center
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1.0)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(size = 0.5)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "gray95",
                                                            colour = "black")) +
    ggplot2::theme(legend.position = "bottom",
                   legend.key = ggplot2::element_blank(),
                   legend.key.size = grid::unit(0.5, "cm"))
  
  return(f2)
  
} ##/DataFitForecastPlot


##### DataFitForecastPlotwEOL
DataFitForecastPlotwEOL <- function(df,
                                    ID = "Forecast Plot",
                                    subt = "Periodic Data with Fit and Forecast (80% & 95% CIs)",
                                    cap = "Caption",
                                    ylab = "Y Label",
                                    llab = "Use Label",
                                    t.period = "days",
                                    end.use) {
  
  #require(ggplot2)
  #require(stats)
  
  main.color <- "green3"
  
  #Fix/adjust for timeperiod
  if (t.period == "years") {
    maj.breaks <- "1 year"
    min.breaks <- "1 year"
    date.lbls <- "%Y"
  } else {
    maj.breaks <- "3 months"
    min.breaks <- "1 month"
    date.lbls <- "%m-%Y"
  }
  
  ##Build the plot
  f3 <- ggplot2::ggplot(data = df, ggplot2::aes(x = Time.new),
                        na.rm(TRUE)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast.lower95,
                                      ymax = forecast.upper95,
                                      fill = "gray80"),
                         color = "transparent", linetype = "blank", size = 1) +
    # ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast.lower80,
    #                                   ymax = forecast.upper80,
    #                                   fill = "gray70"),
    #                      color = "transparent", linetype = "blank", size = 1) +
    ggplot2::geom_col(ggplot2::aes(y = freq.new, fill = main.color)) +
    ggplot2::geom_line(ggplot2::aes(y = forecast.mean, color = "Forecast"),
                       linetype = "solid", size = 0.5) +
    # ggplot2::geom_point(ggplot2::aes(y = model.fit),
    #                     color = "black", shape = 20,
    #                     size = 3) + #add points to fit line
    ggplot2::geom_line(ggplot2::aes(y = model.fit.new,
                                    color = "Fit Line"),
                       linetype = "solid", size = 0.5) +
    # ggplot2::geom_point(ggplot2::aes(y = seasonal.model.fit.new),
    #                     color = "black", shape = 20,
    #                     size = 3) + #add points to trend line
    ggplot2::geom_vline(ggplot2::aes(xintercept = max(Time.new)),
                        color = "tomato3", linetype = "solid",
                        size = 0.75) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = end.use,
                                     color = "EOL (Time & Use)"),
                        linetype = "solid",
                        size = 0.75) +
    ggplot2::labs(title = ID, subtitle = subt, caption = cap, fill = NULL) +
    ggplot2::scale_x_date(name = "Date", #Axis title
                          date_breaks = maj.breaks, #Label interval/break
                          labels = "Dates", #Axis label
                          date_labels = date.lbls, #Date format
                          date_minor_breaks = min.breaks, #minor interval break (no label)
                          # limits = c(min(dates), max(dates)), #Axis limits
                          # expand = ggplot2::expand_scale(c(0.01, 0.025)),
                          expand = c(0.01, 0.025),
                          position = "bottom") + #Sets label position to the bottom of plot
    ggplot2::scale_y_continuous(name = ylab, #Axis title
                                # expand = ggplot2::expand_scale(c(0.01, 0.05)),
                                expand = c(0.01, 0.05),
                                position = "left") + #Left side of plot
    ggplot2::scale_color_manual(name = "",
                                values = c("Forecast" = "blue",
                                           "Fit Line" = "black",
                                           "EOL (Time & Use)" = "tomato3",
                                           "temp" = "purple")) +
    ggplot2::scale_fill_identity(name = "", guide = "legend",
                                 labels = c(#"80% Fore. Int.",
                                   "95% Fore. Int.",
                                   llab)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) + #Aligns titles center
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1.0)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(size = 0.5)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "gray95",
                                                            colour = "black")) +
    ggplot2::theme(legend.position = "bottom",
                   legend.key = ggplot2::element_blank(),
                   legend.key.size = grid::unit(0.5, "cm"))
  
  
  return(f3)
  
} ##/DataFitForecastPlotwEOL


##### DataFitForecastPlotwEOL_V2
DataFitForecastPlotwEOL_V2 <- function(df,
                                       ID = "Forecast Plot",
                                       subt = "Periodic Data with Fit and Forecast (with 95% FI)",
                                       cap = "Caption",
                                       ylab = "Y Label",
                                       llab = "Use Label",
                                       llab2 = "Use Label #2",
                                       t.period = "months",
                                       end.use) {
  
  #require(ggplot2)
  #require(stats)
  #require(grid)
  
  main.color <- "green3"
  opn.color <- "khaki2"
  
  #return(plotdata)
  #Fix/adjust for timeperiod
  if (t.period == "years") {
    maj.breaks <- "1 year"
    min.breaks <- "1 year"
    date.lbls <- "%Y"
  } else {
    maj.breaks <- "3 months"
    min.breaks <- "1 month"
    date.lbls <- "%m-%Y"
  }
  
  ##Build the plot
  f4 <- ggplot2::ggplot(data = df, ggplot2::aes(x = Time.new),
                        na.rm = TRUE) +
    ggplot2::geom_col(ggplot2::aes(y = opn.freq.new, fill = opn.color),
                      na.rm = TRUE) +
    ggplot2::geom_col(ggplot2::aes(y = freq.new, fill = main.color),
                      na.rm = TRUE) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = opn.fore.lower95,
                                      ymax = opn.fore.upper95,
                                      fill = "gray80"),
                         color = "transparent", linetype = "blank",
                         size = 1, na.rm = TRUE) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast.lower95,
                                      ymax = forecast.upper95,
                                      fill = "gray80"),
                         color = "transparent", linetype = "blank",
                         size = 1, na.rm = TRUE) +
    # ggplot2::geom_ribbon(ggplot2::aes(ymin = opn.fore.lower80,
    #                                   ymax = opn.fore.upper80,
    #                                   fill = "gray70"),
    #                      color = "transparent", linetype = "blank",
    #                      size = 1, na.rm = TRUE) +
    # ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast.lower80,
    #                                   ymax = forecast.upper80,
    #                                   fill = "gray70"),
    #                      color = "transparent", linetype = "blank",
    #                      size = 1, na.rm = TRUE) +
    
  ggplot2::geom_line(ggplot2::aes(y = opn.fore.mean, color = "Forecast"),
                     linetype = "solid", size = 0.5, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = forecast.mean, color = "Forecast"),
                       linetype = "solid", size = 0.5, na.rm = TRUE) +
    # ggplot2::geom_point(ggplot2::aes(y = model.fit),
    #                     color = "black", shape = 20,
    #                     size = 3) + #add points to fit line
    ggplot2::geom_line(ggplot2::aes(y = opn.model.fit.new,
                                    color = "Fit Line"),
                       linetype = "solid", size = 0.5, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = model.fit.new,
                                    color = "Fit Line"),
                       linetype = "solid", size = 0.5, na.rm = TRUE) +
    # ggplot2::geom_point(ggplot2::aes(y = seasonal.model.fit.new),
    #                     color = "black", shape = 20,
    #                     size = 3) + #add points to trend line
    ggplot2::geom_vline(ggplot2::aes(xintercept = max(Time.new)),
                        color = "tomato3", linetype = "solid",
                        size = 0.75, na.rm = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = end.use,
                                     color = "EOL (Time & Use)"),
                        linetype = "solid",
                        size = 0.75, na.rm = TRUE) +
    ggplot2::labs(title = ID, subtitle = subt, caption = cap, fill = NULL) +
    ggplot2::scale_x_date(name = "Date", #Axis title
                          date_breaks = maj.breaks, #Label interval/break
                          labels = "Dates", #Axis label
                          date_labels = date.lbls, #Date format
                          date_minor_breaks = min.breaks, #minor interval break (no label)
                          # limits = c(min(dates), max(dates)), #Axis limits
                          # expand = ggplot2::expand_scale(c(0.01, 0.025)),
                          expand = c(0.01, 0.025),
                          position = "bottom") + #Sets label position to the bottom of plot
    ggplot2::scale_y_continuous(name = ylab, #Axis title
                                # expand = ggplot2::expand_scale(c(0.01, 0.05)),
                                expand = c(0.01, 0.05),
                                position = "left") + #Left side of plot
    ggplot2::scale_color_manual(name = "",
                                values = c("Forecast" = "blue",
                                           "Fit Line" = "black",
                                           "EOL (Time & Use)" = "tomato3")) +
    ggplot2::scale_fill_identity(name = "", guide = "legend",
                                 labels = c(#"80% Fore. Int.",
                                   "95% Fore. Int.",
                                   llab, llab2)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) + #Aligns titles center
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1.0)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(size = 0.5)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "gray95",
                                                            colour = "black")) +
    ggplot2::theme(legend.position = "bottom",
                   legend.key = ggplot2::element_blank(),
                   legend.key.size = grid::unit(0.5, "cm"))
  
  
  return(f4)
  
} ##/DataFitForecastPlotwEOL_V2


##### DataFitPlot
DataFitPlot <- function(df, fm, ID = "Main Title",
                        subt = "Subtitle", cap = "Caption",
                        ylab = "Y Label",
                        llab = "Metric Label",
                        t.period = "days") {
  #require(ggplot2)
  #require(grid)
  
  main.color <- "green3"
  shade.color <- "lightgreen"
  
  date <- df$Date
  elem <- df[, 2]
  
  model.fit <- as.vector(fm[["fitted"]])
  
  model.fit <- TrimVector(model.fit, minimum = 0)
  
  #Prepare plot data, trend line, and fit line
  plotdata <- data.frame(date, elem, model.fit)
  
  #Fix/adjust for timeperiod
  if (t.period == "years") {
    maj.breaks <- "1 year"
    min.breaks <- "1 year"
    date.lbls <- "%Y"
  } else {
    maj.breaks <- "3 months"
    min.breaks <- "1 month"
    date.lbls <- "%m-%Y"
  }
  
  #Build Plot
  f1 <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = date)) +
    ggplot2::geom_col(ggplot2::aes(y = elem, fill = main.color)) +
    
    ggplot2::geom_line(ggplot2::aes(y = model.fit, color = "Fit Line"),
                       linetype = "solid", size = 0.5) +
    # ggplot2::geom_point(ggplot2::aes(y = seasonal.model.fit),
    #                     color = "black",
    #                     shape = 20,
    #                     size = 3) + #Adds point to fit line
    ggplot2::labs(title = ID, subtitle = subt, caption = cap, fill = NULL) +
    ggplot2::scale_x_date(
      name = "Date", #Axis title
      date_breaks = maj.breaks, #Axis interval / axis break
      labels = "Dates", #Axis Label
      date_labels = date.lbls, #Date Format
      date_minor_breaks = min.breaks, #Minor interavl - no label
      #limits = c(min(date), max(date)), #Axis limits
      expand = c(0.01, 0.1), #How far to expand beyond range limits for display
      position = "bottom") + #Sets label position to bottom of plot
    ggplot2::scale_y_continuous(
      name = ylab, #Axis title
      position = "left") + #Axis title left of plot
    ggplot2::scale_colour_manual(name = "",
                                 values = c("Trend Line" = "gray55",
                                            "Fit Line" = "black")) +
    ggplot2::scale_fill_identity(name = "",
                                 guide = "legend",
                                 labels = c(llab)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(
                     hjust = 0.5)) + #Aligns titles center
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1.0)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(size = 0.5)) +
    #Plot background & color
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "gray90",
                                                            colour = "black")) +
    ggplot2::theme(legend.position = "bottom",
                   legend.key = ggplot2::element_blank(),
                   legend.key.size = grid::unit(0.5, "cm"))
  
  return(f1)
} ##/DataFitPlot


##### GenerateRiskPlot_V1
GenerateRiskPlot_V1 <- function(df, key.dates, ID = NULL,
                                subt = NULL, cap = NULL) {
  # require(ggplot2)
  
  if (length(key.dates) == 1) {
    key.dates.colors <- c("red")
  } else if (length(key.dates) == 2) {
    key.dates.colors <- c("yellow", "red")
  } else if (length(key.dates) == 3) {
    key.dates.colors <- c("green", "yellow", "red")
  } else if (length(key.dates) == 4) {
    key.dates.colors <- c("green", "yellow", "red", "red")
  } else if (length(key.dates) == 5) {
    key.dates.colors <- c("green", "green", "yellow", "red", "red")
  } else if (length(key.dates) == 6) {
    key.dates.colors <- c("green", "green", "yellow", "red", "black", "black")
  }
  
  risk.plot <- GenerateRiskPlot(df = df, key.dates = key.dates,
                                key.date.colors = key.dates.colors,
                                ID = ID, subt = subt, cap = cap)
  
  return(risk.plot)
  
} ##/GenerateRiskPlot_V1


##### GenerateRiskPlot_V2
GenerateRiskPlot_V2 <- function(df, key.dates, ID = NULL,
                                subt = NULL, cap = NULL) {
  # require(ggplot2)
  
  if (length(key.dates) == 1) {
    key.dates.colors <- c("red")
  } else if (length(key.dates) == 2) {
    key.dates.colors <- c("yellow", "red")
  } else if (length(key.dates) == 3) {
    key.dates.colors <- c("yellow", "yellow", "red")
  } else if (length(key.dates) == 4) {
    key.dates.colors <- c("green", "yellow", "yellow", "red")
  } else if (length(key.dates) == 5) {
    key.dates.colors <- c("green", "yellow", "yellow", "red", "red")
  } else if (length(key.dates) == 6) {
    key.dates.colors <- c("green", "green", "yellow", "yellow", "red", "red")
  }
  
  risk.plot <- GenerateRiskPlot(df = df, key.dates = key.dates,
                                key.date.colors = key.dates.colors,
                                ID = ID, subt = subt, cap = cap)
  
  return(risk.plot)
  
} ##/GenerateRiskPlot_V2


##### GenerateRiskPlot
GenerateRiskPlot <- function(df, key.dates, key.date.colors,
                             ID = NULL, subt = NULL, cap = NULL) {
  
  if (length(unique(key.dates)) == 1) {
    key.dates.linecolors <- c("black")
    key.dates.textcolors <- c("black")
  } else {
    key.dates.linecolors <- c("gray5")
    key.dates.textcolors <- c("gray5")
  }
  
  f6 <- ggplot2::ggplot(df, ggplot2::aes(group = shade.group.fill,
                                         fill = shade.fill)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x.min, xmax = x.max,
                                    ymin = y.min, ymax = y.max)) +
    ggplot2::geom_vline(xintercept = as.numeric(key.dates),
                        linetype = "dashed",
                        colour = key.dates.linecolors,
                        size = 0.75)
  
  for (i in 1:length(key.dates)) {
    # if (i == length(key.dates)) {
    if (i > 1) {
      f6 <- f6 + ggplot2::geom_text(x = as.Date(key.dates[i]),
                                    y = 0.5,
                                    label = format(key.dates[i], format = "%b-%Y"),
                                    colour = key.dates.textcolors,
                                    angle = 90,
                                    vjust = -0.5,
                                    hjust = 0.5,
                                    size = 6.5)
    } else {
      f6 <- f6 + ggplot2::geom_text(x = as.Date(key.dates[i]),
                                    y = 0.5,
                                    label = format(key.dates[i], format = "%b-%Y"),
                                    colour = key.dates.textcolors,
                                    angle = 90,
                                    vjust = 1.4,
                                    hjust = 0.5,
                                    size = 6.5)
    }
  } ##/ for i
  
  f6 <- f6 + ggplot2::labs(title = ID,
                           subtitle = subt,
                           caption = cap) +
    ggplot2::scale_fill_gradientn(colours = key.date.colors) +
    # ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(c(0, 0))) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    # ggplot2::scale_x_date(expand = ggplot2::expand_scale(c(0, 0))) +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) + #Aligns titles center
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       vjust = 1.0,
                                                       hjust = 1.0)) +
    ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none")
  
  
  return(f6)
  
} ##/GenerateRiskPlot


#####SimpleDataPlot
SimpleDataPlot <- function(df,  main.title = NULL, x.lab = NULL, y.lab = NULL) {
  #require(ggplot)
  #require(scales)
  
  dp <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = df[,1],
                                    y = df[,2])) +
    ggplot2::scale_x_date(date_breaks = "year",
                          date_labels = "%Y") +
    ggplot2::labs(x = x.lab,
                  y = y.lab,
                  title = main.title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(dp)
  
} ##/SimpleDataPlot


##### StripGGPlot
StripGGPlot <- function(plot, ID = NULL) {
  plot2 <- plot +
    ggplot2::labs(title = ID,
                  subtitle = NULL,
                  caption = NULL) +
    # ggplot2::scale_x_date(expand = ggplot2::expand_scale(c(0, 0))) +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    # ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(c(0, 0))) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + #Aligns titles center
    ggplot2::theme(#axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      #axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      #panel.grid.major = ggplot2::element_blank(),
      #panel.grid.minor = ggplot2::element_blank(),
      #panel.border = ggplot2::element_blank(),
      #panel.background = ggplot2::element_blank(),
      legend.position = "none")
  
  return(plot2)
}

