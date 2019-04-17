

##### Custom/Special Functions


##### Finding the Average Ranking
AvgRank <- function(df) {
  new.df <- as.matrix(df[ c(-1, -2)], mode = "numeric")
  Num_Fits <- dim(df)[1]
  Fits <- df[ ,1]
  FitName <- df[, 2]
  MeanRanks <- rowMeans(new.df, na.rm = TRUE)
  Rankings <- data.frame(Fits, FitName, MeanRanks, stringsAsFactors = FALSE)
  Rankings <- Rankings[order(rank(Rankings[, 3], 
                                  na.last = TRUE,
                                  ties.method = "random")
  ), ]  #Ties handled through random draw
  Rankings <- data.frame(Rankings, seq_along(Rankings[, 1]),
                         stringsAsFactors = FALSE)
  names(Rankings) <- c("Fit", "Name", "Average Normalized Value", "Position")
  
  attributes(Rankings)$row.names <- c(1:length(Rankings$Name))
  
  return(Rankings)
} ##/AvgRank


##### ConsolidateDailyUseData
ConsolidateDailyUseData <- function(df, start.date) {
  # require(dplyr)
  # require(lubridate)
  
  "%>%" <- dplyr::"%>%"
  ### Preserve column Names
  cn <- colnames(df)
  colnames(df) <- c("Date", "elem") #Need generic colnames for ease in dplyr function
  ### Remove all NA's and zeros from the data set, sum unique days, and ensure ascending order
  df <- df[!is.na(df[, 2]), ] #remove all rows with NAs
  df <- df[(df[, 2] != 0), ] #remove all rows with zeros
  df <- df %>%  #Make each day unique
    dplyr::group_by(Date) %>% #group by date
    dplyr::summarise(elem = sum(elem)) #sum elements that occur on the same date
  df <- data.frame(df)
  df <- df[order(df[1], decreasing = FALSE), ] #In ascending date order
  ##Organize the data set to perform time series:
  #(Ascending order, fill in missing days/months/years as appropriate for continuous data set)
  # Fill in the missing Days
  date.range <- seq(start.date, max(df[, 1]), 1)
  use.elem <- vector(mode = "numeric", length = length(date.range))
  for (i in 1:length(date.range)) {
    if(date.range[i] %in% df$Date) {
      use.elem[i] <- df$elem[df$Date == date.range[i]]
    }
    else{use.elem[i] <- 0}
  }
  #Put it all back together
  df <- data.frame("Date" = date.range, "elem" = use.elem,
                   stringsAsFactors = FALSE)
  colnames(df) <- cn
  df <- data.frame(df, stringsAsFactors = FALSE)
  
  return(df)
} ##/ConsolidateDailyUseData


##### ConsolidateMonthlyUseData
ConsolidateMonthlyUseData <- function(df) {
  # require(dplyr)
  # require(lubridate)
  
  "%>%" <- dplyr::"%>%"
  
  cn <- colnames(df)
  colnames(df) <- c("Date", "elem")
  #Add a "Month" column to summarize by
  df <- df %>%
    dplyr::mutate(month = as.Date(paste0(lubridate::month(Date),
                                         "/", lubridate::days_in_month(Date),
                                         "/", lubridate::year(Date)),
                                  format = "%m/%d/%Y"))
  #Summarize by month
  df <- df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(sum_elem = sum(elem))
  #re-set column names
  colnames(df) <- cn
  df <- data.frame(df, stringsAsFactors = FALSE)
  
  return(df)
} ##/ConsolidateMonthlyUseData


##### Function to adjust date in data frame, for data frames with
# $Month and $Year columns; combines the two and returns combination
# in a single dat formatted column ($Date)
FixMonthlyDateDataFrame <- function(df) {
  #require(dplyr)
  "%>%" <- dplyr::"%>%"
  
  #Check for different date formats and take appropriate actions
  if (any(names(df) %in% c("Date"))) {
    #Fix Date format
    new.df <- df
    new.df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
    
  } else if (any(names(df) %in% c("Month", "Year"))) {
    new.df <- df %>%
      dplyr::mutate(Date = paste0("01/", df$Month, "/", df$Year))
    #Fix Date format
    new.df$Date <- as.Date(df$Date, format = "%d/%b/%Y")
    #Remove previous $Month and $Year columns
    new.df <- new.df[, !names(new.df) %in% c("Month", "Year")]
    
  } else {
    new.df <- df
    
  }
  
  return(new.df)
} ##/ FixMonthlyDateDataFrame


###Generates a cummulative total aka runnig total
GenerateCumData <- function(df, init) {
  len.df <- length(df[, 2])
  df[1, 2] <- df[1, 2] + init
  for(i in 2:len.df) {
    df[i, 2] <- df[(i-1), 2] + df[i, 2]
  } ##/ for i
  return(df)
} ##/GenerateCommulativeData


#####Generate a summary data frame of the time series or forecast accuracy of
#the methods used in GenerateMultipleFits
GenerateFitSummary <- function(fit.x, ts.x, fit.x.methods, multiFit = TRUE,
                               indexNum = NULL) {
  #require(dplyr)
  #require(forecast)
  
  temp1 <- data.frame(colnames(c("Fit", "Forecast_Method",
                                 "ME", "RMSE", "MAE", "MPE",
                                 "MAPE", "MASE", "ACF1", "Thiel's U")),
                      stringsAsFactors = FALSE)
  if (!multiFit) {
    temp2 <- tryCatch({forecast::accuracy(f = fit.x,
                                          x = ts.x)[2,]},
                      error = function(e) {e = c(rep(NA, 8))})
    
    temp3 <- data.frame(matrix(ncol = 10, nrow = 1))
    colnames(temp3) <- c("Fit", "Forecast_Method",
                         "ME", "RMSE", "MAE", "MPE",
                         "MAPE", "MASE", "ACF1", "Thiel's U")
    temp3$Fit <- paste0("Choice")
    # temp3$Forecast_Method <- fit.x$method
    temp3$Forecast_Method <- fit.x.methods[indexNum]
    if (length(temp2) == 6) {
      temp3[1, 3:8] <- temp2[1:6]
    } else if (length(temp2 == 7)){
      temp3[1, 3:9] <- temp2[1:7]
    } else {
      temp3[1, 3:10] <- temp2[1:8]
    }
    
    temp1 <- dplyr::bind_rows(temp1, temp3)
  } else {
    for (i in 1:length(fit.x)) {
      #cat(paste0("run", i, " ")) #for troubleshooting
      temp2 <- tryCatch({forecast::accuracy(f = fit.x[[i]],
                                            x = ts.x)[2,]},
                        error = function(e) {e = c(rep(NA, 8))})
      
      temp3 <- data.frame(matrix(ncol = 10, nrow = 1))
      colnames(temp3) <- c("Fit", "Forecast_Method",
                           "ME", "RMSE", "MAE", "MPE",
                           "MAPE", "MASE", "ACF1", "Thiel's U")
      temp3$Fit <- paste0("Fit", i)
      temp3$Forecast_Method <- fit.x.methods[i]
      if (length(temp2) == 6) {
        temp3[1, 3:8] <- temp2[1:6]
      } else if (length(temp2 == 7)){
        temp3[1, 3:9] <- temp2[1:7]
      } else {
        temp3[1, 3:10] <- temp2[1:8]
      }
      
      temp1 <- dplyr::bind_rows(temp1, temp3)
      #cat(paste0("Completed fit ", i, ". /n")) #For troubleshooting
    }
    
  } ##/if else multiFit
  
  drops <- c("Thiel's U")
  temp1 <- temp1[, !(names(temp1) %in% drops)]
  
  return(temp1)
  
} ##/GenerateFitSummary



#####Generates multiple forecast fits based on time series or forecast object
GenerateMultipleFits <- function(ts.df, h.int, incl.fit = c(1:15)) {
  #require(forecast)
  
  fit.x <- list()
  
  # #Naive mean forecast
  # if (1 %in% incl.fit) {
  #   fit1 <- tryCatch({forecast::meanf(ts.df, h = h.int)},
  #                    error = function(e) {e = NA})
  #   fit.x[[(length(fit.x) + 1)]] <- fit1
  # }
  # #cat("fit1 ")
  # 
  # #Niave forecast (yesterday)
  # if (2 %in% incl.fit) {
  #   fit2 <- tryCatch({forecast::naive(ts.df, h = h.int)},
  #                    error = function(e) {e = NA})
  #   fit.x[[(length(fit.x) + 1)]] <- fit2
  # }
  # #cat("fit2 ")
  # 
  # #Seasonal naive
  # if (3 %in% incl.fit) {
  #   fit3 <- tryCatch({forecast::snaive(ts.df, h = h.int)},
  #                    error = function(e) {e = NA})
  #   fit.x[[(length(fit.x) + 1)]] <- fit3
  # }
  # #cat("fit3 ")
  
  #Linear model
  if (1 %in% incl.fit) {
    fit1 <- tryCatch({forecast::tslm(ts.df ~ trend)},
                     error = function(e) {e = NA})
    fit1 <- forecast::forecast(fit1, h = h.int)
    fit.x[[(length(fit.x) + 1)]] <- fit1
  }
  #cat("fit1 ")
  
  #Linear model (with seasonal)
  if (2 %in% incl.fit) {
    if (length(ts.df) <= 35) {
      fit2 <- NA
    } else {
      fit2 <- tryCatch({forecast::tslm(ts.df ~ trend + season)},
                       error = function(e) {e = NA})
      fit2 <- forecast::forecast(fit2, h = h.int)
    }
    fit.x[[(length(fit.x) + 1)]] <- fit2
  }
  #cat("fit2 ")
  
  #Random Walk
  if (3 %in% incl.fit) {
    fit3 <- tryCatch({forecast::rwf(ts.df, drift = TRUE, h = h.int)},
                     error = function(e) {e = NA})
    fit.x[[(length(fit.x) + 1)]] <- fit3
  }
  #cat("fit3 ")
  
  #Seasonal decomposition (for seasonal data)
  if (4 %in% incl.fit) {
    fit4 <- tryCatch({forecast::stlf(ts.df, s.window = "period", h = h.int)},
                     error = function(e) {e = NA})
    fit.x[[(length(fit.x) + 1)]] <- fit4
  }
  #cat("fit4 ")
  
  # #Exponential Smoothing
  # if (8 %in% incl.fit) {
  #   fit8 <- tryCatch({forecast::ses(ts.df, h = h.int)},
  #                    error = function(e) {e = NA})
  #   fit.x[[(length(fit.x) + 1)]] <- fit8
  # }
  # #cat("fit8 ")
  
  #Simple HoltWinters
  if (5 %in% incl.fit) {
    fit5 <- tryCatch({forecast::holt(ts.df, h = h.int)},
                     error = function(e) {e = NA})
    fit.x[[(length(fit.x) + 1)]] <- fit5
  }
  #cat("fit5 ")
  
  #HoltWinters (additive & multplicative)
  if (6 %in% incl.fit) {
    if (frequency(ts.df) <= 24) {
      fit6 <- tryCatch({forecast::hw(ts.df, h = h.int)},
                       error = function(e) {e = NA})
      #cat("fit6-GO ")
    } else {
      fit6 <- NA
      #cat("fit6-NOGO ")
    } ##/ if h.int
    fit.x[[(length(fit.x) + 1)]] <- fit6
  }
  
  # #Simple exponential (models levels)
  # if (11 %in% incl.fit) {
  #   fit11 <- tryCatch({HoltWinters(ts.df, beta = FALSE, gamma = FALSE)},
  #                     error = function(e) {e = NA})
  #   fit11 <- forecast::forecast(fit11, h = h.int)
  #   fit.x[[(length(fit.x) + 1)]] <- fit11
  # }
  # #cat("fit11 ")
  
  #Double exponential (models level & trend)
  if (7 %in% incl.fit) {
    fit7 <- tryCatch({HoltWinters(ts.df, gamma = FALSE)},
                     error = function(e) {e = NA})
    fit7 <- forecast::forecast(fit7, h = h.int)
    fit.x[[(length(fit.x) + 1)]] <- fit7
  }
  #cat("fit7 ")
  
  #Triple exponential - (models level, trend, & seasonal)
  if (8 %in% incl.fit) {
    fit8 <- tryCatch({HoltWinters(ts.df)},
                     error = function(e) {e = NA})
    if (length(fit8) != 1) {
      fit8 <- forecast::forecast(fit8, h = h.int)
    }
    fit.x[[(length(fit.x) + 1)]] <- fit8
  }
  #cat("fit8 ")
  
  #Exponential smoothing state space model
  if (9 %in% incl.fit) {
    if (frequency(ts.df) <= 24) {
      fit9 <- tryCatch({forecast::ets(ts.df)},
                       error = function(e) {e = NA})
      if (length(fit9) != 1) {
        fit9 <- forecast::forecast(fit9, h = h.int)
      }
      #cat("fit9-GO ")
    } else {
      fit9 <- NA
      #cat("fit9-NOGO ")
    } ##/if h.int
    fit.x[[(length(fit.x) + 1)]] <- fit9
  }
  
  #ARIMA model (Auto fit)
  if (10 %in% incl.fit) {
    fit10 <- tryCatch({forecast::auto.arima(ts.df)},
                      error = function(e) {e = NA})
    if (length(fit10) != 1) {
      fit10 <- forecast::forecast(fit10, h = h.int)
    }
    fit.x[[(length(fit.x) + 1)]] <- fit10
  }
  #cat("fit10 ")
  
  # #Croston's method (for intermittent demand)
  # if (16 %in% incl.fit) {
  #   fit16 <- tryCatch({forecast::croston(ts.df, h = h.int)},
  #                     error = function(e) {e = NA})
  #   fit16 <- NA
  #   fit.x[[(length(fit.x) + 1)]] <- fit16
  # }
  # #cat("fit16-WIP ")
  
  #cat("\n fit & forecasts complete!!!")
  
  if (length(fit.x) == 1) {
    fit.x <- fit.x[[1]]
  }
  
  return(fit.x)
  
} ##/GenerateMultipleFits


#####This function returns the char string that ailignes with the methods used
# in the GenerateMultipleFits() function above
GenerateMultipleFitMethods <- function() {
  
  fit.x.methods <- c( # "Naive mean forecast",
    # "Naive forecast (yesterday)",
    # "Seasonal naive",
    "Linear model",
    "Linear model (with seasonal)",
    "Random walk",
    "Seasonal decomposition",
    # "Exponential smoothing",
    "Simple HoltWinters",
    "HoltWinters (add and multi)",
    # "Simple expon. (levels)",
    "Double expon. (level, trend)",
    "Triple expon. (level, trend, seasonal)",
    "Expon. smoothing state space model",
    "ARIMA model (auto fit)"
    # "Croston's method"
    
  )
  
  return(fit.x.methods)
  
} ##/GenerateMultipleFitMethods



#####GeneratePoints_V1
GeneratePoints_V1 <- function(df, end.use, end.time, full = TRUE) {
  
  if (max(df$Time.new) <= end.time) {
    end.time <- max(df$Time.new)
  }
  
  ##Point #1 - First Date
  pt1 <- df$Time.new[1]
  
  ##Point #2 - Last Date of Data
  pt2 <- df$Time.new[max(which(!is.na(df$freq.new), arr.ind = TRUE))]
  
  ##Point #3 -
  index.x <- min(which(df$Time.new >= end.time,
                       arr.ind = TRUE), na.rm = TRUE)
  
  if (max(df$forecast.upper95, na.rm = TRUE) >= end.use) {
    index.y <- min(which(df$forecast.upper95 >= end.use,
                         arr.ind = TRUE), na.rm = TRUE)
  } else {
    index.y <- Inf
  }
  
  if(index.x < index.y) {
    pt3 <- df$Time.new[index.x]
  } else {
    pt3 <- df$Time.new[index.y]
  }
  
  ##Point 4
  
  if (max(df$forecast.mean, na.rm = TRUE) >= end.use) {
    index.y <- min(which(df$forecast.mean >= end.use,
                         arr.ind = TRUE), na.rm = TRUE)
  } else {
    index.y <- Inf
  }
  
  if(index.x < index.y) {
    pt4 <- df$Time.new[index.x]
  } else {
    pt4 <- df$Time.new[index.y]
  }
  ##Point 5
  pt5 <- max(df$Time.new, na.rm = TRUE)
  
  ##Put all points together
  pt <- c(pt1, pt2, pt3, pt4, pt5)
  #point <- sort(unique(point))
  pt <- sort(pt)
  
  #Trim the first point for the truncated points vector
  if (!full) {
    pt <- pt[-1]
  }
  
  return (pt)
  
} ##/GeneratePoints_V1


#####GeneratePoints_V2
GeneratePoints_V2 <- function(df, end.use, end.time, full = TRUE) {
  
  if (max(df$Time.new) <= end.time) {
    end.time <- max(df$Time.new)
  }
  
  ##Point #1
  pt1 <- df$Time.new[1]
  
  ##Point #2
  pt2 <- df$Time.new[max(which(!is.na(df$freq.new), arr.ind = TRUE))]
  
  ##Point #3
  index.x <- min(which(df$Time.new >= end.time,
                       arr.ind = TRUE), na.rm = TRUE)
  
  if (max(df$opn.fore.upper95, na.rm = TRUE) >= end.use) {
    index.y <- min(which(df$opn.fore.upper95 >= end.use,
                         arr.ind = TRUE), na.rm = TRUE)
  } else {
    index.y <- Inf
  }
  
  if(index.x < index.y) {
    pt3 <- df$Time.new[index.x]
  } else {
    pt3 <- df$Time.new[index.y]
  }
  
  ##Point 4
  
  if (max(df$opn.fore.mean, na.rm = TRUE) >= end.use) {
    index.y <- min(which(df$opn.fore.mean >= end.use,
                         arr.ind = TRUE), na.rm = TRUE)
  } else {
    index.y <- Inf
  }
  
  if(index.x < index.y) {
    pt4 <- df$Time.new[index.x]
  } else {
    pt4 <- df$Time.new[index.y]
  }
  
  ##Point #5
  index.x <- min(which(df$Time.new >= end.time,
                       arr.ind = TRUE), na.rm = TRUE)
  
  if (max(df$forecast.upper95, na.rm = TRUE) >= end.use) {
    index.y <- min(which(df$forecast.upper95 >= end.use,
                         arr.ind = TRUE), na.rm = TRUE)
  } else {
    index.y <- Inf
  }
  
  if(index.x < index.y) {
    pt5 <- df$Time.new[index.x]
  } else {
    pt5 <- df$Time.new[index.y]
  }
  
  ##Point 6
  
  if (max(df$forecast.mean, na.rm = TRUE) >= end.use) {
    index.y <- min(which(df$forecast.mean >= end.use,
                         arr.ind = TRUE), na.rm = TRUE)
  } else {
    index.y <- Inf
  }
  
  if(index.x < index.y) {
    pt6 <- df$Time.new[index.x]
  } else {
    pt6 <- df$Time.new[index.y]
  }
  
  ##Put all points together
  pt <- c(pt1, pt2, pt3, pt4, pt5, pt6)
  #point <- sort(unique(point))
  pt <- sort(pt)
  
  #Trim the first point for the truncated points vector
  if (!full) {
    pt <- pt[-1]
  }
  
  return (pt)
  
} ##/GeneratePoints_V2


##### GenerateRiskPlotData
GenerateRiskPlotData <- function(df, point, end.use, end.time,
                                 n = 500) {
  # require(dplyr)
  # require(zoo)
  
  df$Date <- df$Time.new
  len <- length(point)
  
  if (len >= 3) {
    while (length(unique(point)) != len) {
      for (i in 2:(len - 1)) {
        if (point[i] == point[i+1]) {
          #back point i up 5%
          point[i] <- point[i - 1] + floor(0.95 * (point[i] - point[i-1]))
        } ##/if point
      } ##/ for i
      sort(point)
    } ##/while
  } ##/if len
  
  if(len <= 1) {
    x.min.int <- zoo::as.Date(point)
    x.max.int <- zoo::as.Date(point)
    x.min <- zoo::as.Date(point)
    x.max <- zoo::as.Date(point)
    y.min <- 0
    y.max <- 1
    shade.fill <- 1
    shade.group.fill <- 1
    
    df <- data.frame(x.min.int, x.max.int,
                     x.min, x.max,
                     y.min, y.max,
                     shade.fill, shade.group.fill)
  } else {
    ##Set up the df for plotting 'risk'
    for (i in 1:(length(point)-1)) {
      x.min.int <- rep(zoo::as.Date(point[i], n))
      x.max.int <- rep(zoo::as.Date(point[i+1], n))
      x.min <- seq(from = zoo::as.Date(point[i]),
                   to = zoo::as.Date(point[i+1]),
                   length.out = n)
      x.max <- rep(zoo::as.Date(point[i+1]), n)
      y.min <- rep(0, n)
      y.max <- rep(1, n)
      shade.fill <- seq(from = (i-1),
                        to = i,
                        length.out = n)
      shade.group.fill <- i
      
      df2 <- data.frame(x.min.int, x.max.int,
                        x.min, x.max,
                        y.min, y.max,
                        shade.fill, shade.group.fill)
      if (i <= 1) {
        df <- df2
      } else {
        df <- dplyr::bind_rows(df, df2)
      }
      
    } ##/ for i
  }
  
  
  return(df)
} ##/GenerateRiskPlotData


#####
get.set.global.vars <- function(x){
  
  #   Procurement and Termination/Sundown Criteria:
  #     IOCdate = Date when Initial Operationally Capable (ie date of manufacture)
  IOC.date <<- as.Date(origin = "1970-01-01",
                       format = "%m/%d/%Y",
                       fleet.data$IOC_Date[fleet.data$Tail_Num == x])
  #     IOCmileage = Mileage when Initial Operationally Capable
  # (Miles at purchase)
  IOC.hours <<- fleet.data$IOC_Hours[fleet.data$Tail_Num == x]
  #     FOCdate = Date when Full Operationally Capable
  # (when first available for use - used car)
  FOC.date <<- as.Date(origin = "1970-01-01",
                       format = "%m/%d/%Y",
                       fleet.data$FOC_Date[fleet.data$Tail_Num == x])
  #     FOCmileage = Mileage when Full Operationally Capable
  # (miles at first use - used car).
  FOC.hours <<- fleet.data$FOC_Hours[fleet.data$Tail_Num == x]
  #     EOLmileage = Total miles until replacement required.
  EOL.hours <<- fleet.data$EOL_Hours[fleet.data$Tail_Num == x]
  #     EOLage  = Number of years until replacement required.
  EOL.age <<- fleet.data$EOL_Time[fleet.data$Tail_Num == x]
  #     EOLdate = IOCdate + EOLage
  EOL.date <<- lubridate::ymd(IOC.date) + lubridate::years(EOL.age)
  
} ##/get.set.global.vars


#####GetTextOfKeyDayes
GetTextOfKeyDates <- function(date.vec) {
  temp.dates <- unique(date.vec)
  temp.dates <- temp.dates[-1] #remove the first date, it is the end of the data set
  txt <- paste0("The key dates are ")
  if (length(temp.dates) >= 2) {
    txt <- paste0("The key dates are ")
    for ( i in 1:(length(temp.dates) - 1)) {
      txt <- paste0(txt, as.character(temp.dates[i], format = "%B %Y"), ", ")
    }
    txt <- paste0(txt, "and ",
                  as.character(temp.dates[length(temp.dates)], format = "%B %Y"),
                  ".")
  } else {
    txt <- paste0("The key date is ", 
                  as.character(temp.dates[length(temp.dates)], format = "%B %Y"),
                  ".")
  }
  
  return(txt)
} ##/GetTextOfKeyDates


#####NormMatrix
NormMatrix <- function(df) {
  
  new.df <- df
  colnames(new.df) <- c("Fit", "Forecast_Method", "Norm_ME", "Norm_RMSE",
                        "Norm_MAE", "Norm_MPE", "Norm_MAPE", "Norm_MASE",
                        "Norm_ACF1")
  #Find the abs() then normailze from 0 to max into 0 to 100
  new.df$Norm_ME <- NormVec100(new.df$Norm_ME)
  new.df$Norm_RMSE <- NormVec100(new.df$Norm_RMSE)
  new.df$Norm_MAE <- NormVec100(new.df$Norm_MAE)
  new.df$Norm_MPE <- NormVec100(new.df$Norm_MPE)
  new.df$Norm_MAPE <- NormVec100(new.df$Norm_MAPE)
  new.df$Norm_MASE <- NormVec100(new.df$Norm_MASE)
  new.df$Norm_ACF1 <- NormVec100(new.df$Norm_ACF1)
  
  
  return(new.df)
} ##/NormMatrix

NormVec100 <- function(vec) {
  
  vec <- abs(vec)
  
  if(all(is.na(vec))) {
    return(vec)
  } else {
    vecMax <- max(vec, na.rm = TRUE)
    for(i in 1:length(vec)) {
      vec[i] <- (vec[i] / vecMax) * 100
    } ##for i
  }
  
  return(vec)
  
} ##NormVec100


#####Generates Rankings from accuracy matrix
RankMatrix <- function(df) {
  new.df <- df
  colnames(new.df) <- c("Fit", "Forecast_Method", "Rank_ME", "Rank_RMSE",
                        "Rank_MAE", "Rank_MPE", "Rank_MAPE", "Rank_MASE",
                        "Rank_ACF1")
  
  new.df$Rank_ME <- rank(abs(df$ME))
  new.df$Rank_RMSE <- rank(df$RMSE)
  new.df$Rank_MAE <- rank(df$MAE)
  new.df$Rank_MPE <- rank(abs(df$MPE))
  new.df$Rank_MAPE <- rank(df$MAPE)
  new.df$Rank_MASE <- rank(df$MASE)
  new.df$Rank_ACF1 <- rank(abs(df$ACF1))
  
  return(new.df)
  
} ##/ RankMatrix


#####Quickly parse dataframe into Date and element format
SpliceDataFrame <- function(df, elem, elem.data) {
  df.date <- df$Date
  df.elem <- elem.data
  
  df.new <- data.frame(Date = df.date, elem = df.elem, stringsAsFactors = FALSE)
  
  colnames(df.new) <- c("Date", elem)
  
  return(df.new)
} ##/SpliceDataFrame


#####Quickly parse dataframe into Date and element format
SpliceDataFrame_OPN <- function(df, elem, elem.data, opn.fac) {
  df.date <- df$Date
  df.miles <- df[,2]
  df.opn <- df[,3]
  df.miles <- df.miles - df.opn
  df.opn <- df.opn * (1 + opn.fac)
  df.opn <- df.opn + df.miles
  
  df.new <- data.frame(Date = df.date, elem = df.opn, stringsAsFactors = FALSE)
  
  colnames(df.new) <- c("Date", elem)
  
  return(df.new)
} ##/SpliceDataFrame_OPN


#####Function to trim a vector to a minimum or maximum level (requires a vector)
TrimVector <- function(vec, minimum = NULL, maximum = NULL) {
  #require()
  
  if (!is.null(minimum)) {
    vec[vec < minimum] <- minimum
  }
  
  if (!is.null(vec)) {
    vec[vec > maximum] <- maximum
  }
  
  return(vec)
} ##/TrimVector


##### V1_ComplexPlotData
V1_ComplexPlotData <- function(df, fm, t.period = "months", end.use = NA) {
  #Prepare Data for plotting
  date <- df$Date
  elem.name <- colnames(df[2])
  temp.series <- data.frame("Date" = df$Date, "elem" = df[, 2])
  min.feas1 <- temp.series$elem[length(temp.series$elem)]
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
  date.Add <- zoo::as.Date.yearmon(zoo::index(fm$mean), frac = 1)
  # date.Add <- seq(from = (seq(from = max(df$Date), by = t.period, length = 2)[2]),
  #                 by = t.period, length = length(fm$mean))
  Time.new <- c(date, date.Add)
  freq.new <- c(temp.series$elem, rep(NA, length(fm$mean)))
  model.fit.new <- c(model.fit, rep(NA, length(fm$mean)))
  
  #Trim the vectors to ensure no unfeasible solutions
  freq.new <- TrimVector(freq.new, minimum = 0)
  model.fit.new <- TrimVector(model.fit.new, minimum = 0)
  forecast.mean <- TrimVector(forecast.mean, minimum = 0, maximum = end.use)
  forecast.upper80 <- TrimVector(forecast.upper80, minimum = 0, maximum = end.use)
  forecast.lower80 <- TrimVector(forecast.lower80, minimum = min.feas1, maximum = end.use)
  forecast.upper95 <- TrimVector(forecast.upper95, minimum = 0, maximum = end.use)
  forecast.lower95 <- TrimVector(forecast.lower95, minimum = min.feas1, maximum = end.use)
  
  #Pull together the plot data
  plotdata <- data.frame(Time.new, freq.new, model.fit.new, forecast.mean,
                         forecast.lower80, forecast.upper80,
                         forecast.lower95, forecast.upper95)
  
  return(plotdata)
} ##/V1_ComplexPltData


#####V2_ComplexPlotData
V2_ComplexPlotData <- function(df,
                               fm,
                               df2,
                               fm2,
                               end.use = NULL) {
  #require(zoo)
  
  #Prepare Data for plotting
  date <- df$Date
  elem.name <- colnames(df[2])
  temp.series <- data.frame("Date" = df$Date, "elem" = df[, 2])
  temp.series2 <- data.frame("Date" = df2$Date, "elem" = df2[, 2])
  
  model.fit <- as.vector(fm[["fitted"]])
  opn.model.fit <- as.vector(fm2[["fitted"]])
  
  ##Build plot data with forecast included
  #arrange new plot data
  forecast.mean <- c(rep(NA, length = length(fm$x)), fm$mean)
  forecast.lower80 <- c(rep(NA, length = length(fm$x)), fm$lower[, 1])
  forecast.upper80 <- c(rep(NA, length = length(fm$x)), fm$upper[, 1])
  forecast.lower95 <- c(rep(NA, length = length(fm$x)), fm$lower[, 2])
  forecast.upper95 <- c(rep(NA, length = length(fm$x)), fm$upper[, 2])
  
  opn.fore.mean <- c(rep(NA, length = length(fm2$x)), fm2$mean)
  opn.fore.lower80 <- c(rep(NA, length = length(fm2$x)), fm2$lower[, 1])
  opn.fore.upper80 <- c(rep(NA, length = length(fm2$x)), fm2$upper[, 1])
  opn.fore.lower95 <- c(rep(NA, length = length(fm2$x)), fm2$lower[, 2])
  opn.fore.upper95 <- c(rep(NA, length = length(fm2$x)), fm2$upper[, 2])
  
  #Use the last ts value a start point for forecast - eliminates 'gap' in chart
  forecast.mean[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.lower80[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.upper80[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.lower95[length(fm$x)] <- model.fit[length(fm$x)]
  forecast.upper95[length(fm$x)] <- model.fit[length(fm$x)]
  
  opn.fore.mean[length(fm$x)] <- opn.model.fit[length(fm$x)]
  opn.fore.lower80[length(fm$x)] <- opn.model.fit[length(fm$x)]
  opn.fore.upper80[length(fm$x)] <- opn.model.fit[length(fm$x)]
  opn.fore.lower95[length(fm$x)] <- opn.model.fit[length(fm$x)]
  opn.fore.upper95[length(fm$x)] <- opn.model.fit[length(fm$x)]
  
  ##Watch this line - may be data dependent
  dates.Add <- zoo::as.Date.yearmon(zoo::index(fm$mean), frac = 1)
  
  Time.new <- c(date, dates.Add)
  freq.new <- c(temp.series$elem, rep(NA, length(fm$mean)))
  model.fit.new <- c(model.fit, rep(NA, length(fm$mean)))
  
  opn.freq.new <- c(temp.series2$elem, rep(NA, length(fm2$mean)))
  opn.model.fit.new <- c(opn.model.fit, rep(NA, length(fm2$mean)))
  
  min.feas1 <- temp.series$elem[length(temp.series$elem)]
  min.feas2 <- temp.series2$elem[length(temp.series2$elem)]
  
  #Trim the vectors to ensure no unfeasible solutions
  freq.new <- TrimVector(freq.new, minimum = 0)
  model.fit.new <- TrimVector(model.fit.new, minimum = 0)
  forecast.mean <- TrimVector(forecast.mean, minimum = 0, maximum = end.use)
  forecast.upper80 <- TrimVector(forecast.upper80, minimum = 0, maximum = end.use)
  forecast.lower80 <- TrimVector(forecast.lower80, minimum = min.feas1, maximum = end.use)
  forecast.upper95 <- TrimVector(forecast.upper95, minimum = 0, maximum = end.use)
  forecast.lower95 <- TrimVector(forecast.lower95, minimum = min.feas1, maximum = end.use)
  
  opn.freq.new <- TrimVector(opn.freq.new, minimum = 0)
  opn.model.fit.new <- TrimVector(opn.model.fit.new, minimum = 0)
  opn.fore.mean <- TrimVector(opn.fore.mean, minimum = 0, maximum = end.use)
  opn.fore.upper80 <- TrimVector(opn.fore.upper80, minimum = 0, maximum = end.use)
  opn.fore.lower80 <- TrimVector(opn.fore.lower80, minimum = min.feas2, maximum = end.use)
  opn.fore.upper95 <- TrimVector(opn.fore.upper95, minimum = 0, maximum = end.use)
  opn.fore.lower95 <- TrimVector(opn.fore.lower95, minimum = min.feas2, maximum = end.use)
  
  #Pull together the plot data
  plotdata <- data.frame(Time.new,
                         freq.new, model.fit.new,
                         forecast.mean,
                         forecast.lower80, forecast.upper80,
                         forecast.lower95, forecast.upper95,
                         opn.freq.new, opn.model.fit.new,
                         opn.fore.mean,
                         opn.fore.lower80, opn.fore.upper80,
                         opn.fore.lower95, opn.fore.upper95)
  
  return(plotdata)
  
} ##/V2_ComplexPlotData
