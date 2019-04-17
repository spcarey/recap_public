
server <- function(input, output) {
  
  hideTab(inputId = "tabs", target = "WIP-Operational Use Considerations")
  
  plotsObject <- reactiveValues()
  
  newref <- reactive({
    input$ref_num
  })
  
  fitMethod <- reactive({
    input$rbFit
  })
  
  observeEvent( list(input$ref_num, input$rbFit), {
    
    if ((input$ref_num == "") | is.null(input$ref_num)) {return()}
    
    withProgress(message = 'Performing analysis', style = "notification", value = 0.0, {
      
      incProgress(0.05, detail = paste("Updating global variables"))
      get.set.global.vars(x = input$ref_num)
      
      incProgress(0.05, detail = paste("Reading vehicle data"))
      
      raw.usage.data <<- read.csv(file = paste0("data/", newref(), "_Usage_Report.csv"),
                                  stringsAsFactors = FALSE)
      
      #OPN DATA TAB - SET APPROPIATE BOOLEAN
      if ("Opn_Hrs" %in% colnames(raw.usage.data)) {
        bOpnData <- TRUE
        opn.data <- FixMonthlyDateDataFrame(df = raw.usage.data)
        opn.data <- SpliceDataFrame_OPN(df = opn.data, elem = "Opn_Hrs",
                                        elem.data = opn.data[, 3],
                                        opn.fac = (input$opnUseFact / 100))
        ##### Create data sets by time interval and corresponding time series(ts)
        day.opn.data <- ConsolidateDailyUseData(df = opn.data,
                                                start.date = FOC.date)
        month.opn.data <- ConsolidateMonthlyUseData(df = day.opn.data)
        print(sum(day.opn.data[, 2]))  ###DELETE LATER - For Troubleshooting
        print(sum(month.opn.data[, 2]))  ###DELETE LATER - For Troubleshooting
        ##### Create data sets by time interval and corresponding time series(ts)
        cum.month.opn.data <- GenerateCumData(df = month.opn.data, init = FOC.hours)
        ts.opn.month <- ts(cum.month.opn.data[,2],
                           frequency = 12,
                           start = c(lubridate::year(min(cum.month.opn.data[,1])),
                                     lubridate::month(min(cum.month.opn.data[,1]))))
        print(tail(cum.month.opn.data[, 2], n = 1))  ###DELETE LATER - For Troubleshooting
      } else {
        bOpnData <- FALSE
      } ##/if-else for bOpnData
      
      incProgress(0.05, detail = paste("Building data frames and tables"))
      plotsObject$tbl1 <- raw.usage.data
      usage.data <- FixMonthlyDateDataFrame(df = raw.usage.data)
      usage.data <- SpliceDataFrame(df = usage.data, elem = "Hours",
                                    elem.data = usage.data[, 2])
      day.usage.data <- ConsolidateDailyUseData(df = usage.data,
                                                start.date = FOC.date)
      month.usage.data <- ConsolidateMonthlyUseData(df = day.usage.data)
      ts.day <- ts(day.usage.data[, 2],
                   frequency = 365,
                   start = c(lubridate::year(min(day.usage.data[,1])),
                             lubridate::month(min(day.usage.data[,1])),
                             lubridate::day(min(day.usage.data[,1]))))
      
      incProgress(0.1, detail = paste("Generating data plots"))
      plotsObject$pl1 <- SimpleDataPlot(df = day.usage.data,
                                        main.title = paste("Daily Data Plot"),
                                        x.lab = paste("Time (Year)"),
                                        y.lab = paste("Daily Use (Flight Hours)"))
      ts.month <- ts(month.usage.data[,2],
                     frequency = 12,
                     start = c(lubridate::year(min(month.usage.data[,1])),
                               lubridate::month(min(month.usage.data[,1]))))
      plotsObject$pl2 <- SimpleDataPlot(df = month.usage.data,
                                        main.title = "Monthly Data Plot",
                                        x.lab = "Time (Year)",
                                        y.lab = "Monthly Use (Flight Hours)")
      incProgress(0.05, detail = paste("Finding length of time to forecast"))
      int.h <<- ceiling(lubridate::time_length(lubridate::interval(max(month.usage.data[,1]),
                                                                   EOL.date),
                                               unit = "months"))
      cum.day.usage.data <- GenerateCumData(df = day.usage.data, init = FOC.hours)
      ts.day.cum <- ts(cum.day.usage.data[, 2],
                       frequency = 365,
                       start = c(lubridate::year(min(cum.day.usage.data[,1])),
                                 lubridate::month(min(cum.day.usage.data[,1])),
                                 lubridate::day(min(cum.day.usage.data[,1]))))
      cum.month.usage.data <<- GenerateCumData(df = month.usage.data, init = FOC.hours)
      ts.month.cum <<- ts(cum.month.usage.data[,2],
                          frequency = 12,
                          start = c(lubridate::year(min(cum.month.usage.data[,1])),
                                    lubridate::month(min(cum.month.usage.data[,1]))))
      plotsObject$pl3 <- SimpleDataPlot(df = cum.month.usage.data,
                                        main.title = "Cumulative Monthly Data Plot",
                                        x.lab = "Time (Year)",
                                        y.lab = "Monthly Use (Flight Hours)")
      ##Need to ensure training data does not reduce data set below 3 complete cycles
      incProgress(0.05, detail = paste("Building model training dataframes"))
      if (length(ts.month.cum) < 36) {
        ts.month.train <<- subset(ts.month.cum, end = (length(ts.month.cum) - 1))
      } else {
        ts.month.train <<- subset(ts.month.cum, end = (length(ts.month.cum) - 12))
      } ##/if length(ts.month.cum)
      
      ##FIT METHOD(S) SECTION
      incProgress(0.05, detail = paste("Generating Forecast"))
      fitMethods <- GenerateMultipleFitMethods()
      if (input$rbFit == 0) {  #DEFAULT case
        ##begin of auto deterine fit methods
        print(paste0("Current Fit Index: ", input$rbFit, "."))
        
        ##Begin Auto Fit
        fits <- GenerateMultipleFits(ts.df = ts.month.train, h.int = 12)
        fits.summary <- GenerateFitSummary(fit.x = fits,
                                           ts.x = ts.month.cum,
                                           fit.x.methods = fitMethods)
        plotsObject$tbl2 <- data.table(fits.summary)
        normFits <- NormMatrix(df = fits.summary)
        plotsObject$tbl3 <- data.table(normFits)
        orderNormFits <- AvgRank(df = normFits)
        plotsObject$tbl4 <- data.table(orderNormFits)
        plotsObject$tbl5 <- orderNormFits
        fit.index <<- as.numeric(gsub("Fit", "", orderNormFits$Fit[1]))
        
        incProgress(0.1, detail = paste("Generating Final Fit"))
        fit.choice <<- GenerateMultipleFits(ts.df = ts.month.cum, h.int = int.h, 
                                            incl.fit = fit.index)
        ##end of auto determine fit
      } else {
        
        ##begin user selected fit methods
        print(paste0("Current Fit Index: ", input$rbFit, "."))
        
        fit.index <<- as.numeric(input$rbFit)
        
        fits <- GenerateMultipleFits(ts.df = ts.month.train, h.int = 12,
                                     incl.fit = fit.index)
        
        fitSummary <- GenerateFitSummary(fit.x = fits,
                                         ts.x = ts.month.cum,
                                         fit.x.methods = fitMethods,
                                         multiFit = FALSE,
                                         indexNum = fit.index)
        plotsObject$tbl2 <- data.table(fitSummary)
        normFits <- NormMatrix(df = fitSummary)
        plotsObject$tbl3 <- data.table(normFits)
        orderNormFits <- AvgRank(df = normFits)
        plotsObject$tbl4 <- data.table(orderNormFits)
        plotsObject$tbl5 <- orderNormFits
        
        incProgress(0.1, detail = paste("Generating Final Fit"))
        fit.choice <<- GenerateMultipleFits(ts.df = ts.month.cum, h.int = int.h, 
                                            incl.fit = fit.index)
        ##end user selected fit methods
      } ##/if else fit selection
      
      
      plotsObject$pl4 <- DataFitPlot(df = cum.month.usage.data,
                                     fm = fit.choice,
                                     ID = paste0(input$ref_num, " - Fit Plot"),
                                     subt = "Monthly Cumulative Flight Hours with Fit",
                                     cap = paste0("Method: ", fit.choice$method),
                                     ylab = "Monthly Flight Hours (Cumulative)",
                                     llab = "Monthly Flight Hours",
                                     t.period = "days")
      
      plotsObject$pl5 <- DataFitForecastPlot(df = cum.month.usage.data, 
                                             fm = fit.choice,
                                             ID = paste0(input$ref_num, " - Forecast Plot"),
                                             subt = "Monthly Cumulative Flight Hours with Fit and Forecast (with 95% FI)",
                                             cap = paste0("Method: ", fit.choice$method),
                                             ylab = "Monthly Flight Hours (Cumulative)",
                                             llab = "Monthly Flight Hours",
                                             t.period = "months")
      
      
      
      
      
      incProgress(0.1, detail = paste("Generating Final Fit"))
      plotdata <- V1_ComplexPlotData(df = cum.month.usage.data,
                                     fm = fit.choice,
                                     end.use = EOL.hours)
      
      f1 <- DataFitForecastPlotwEOL(df = plotdata,
                                    ID = paste0(input$ref_num, " - Forecast Plot with EOL Criteria"),
                                    subt = "Cumulative Monthly Flight Hours with Fit and Forecast (with 95% FI)",
                                    cap = NULL,
                                    ylab = "Monthly Flight Hours (Cumulative)",
                                    llab = "Monthly Mile",
                                    t.period = "months",
                                    end.use = EOL.hours)
      
      plotsObject$pl6 <- f1
      key.points <- GeneratePoints_V1(df = plotdata, end.use = EOL.hours,
                                      end.time = EOL.date, full = TRUE)
      plotdata2 <- GenerateRiskPlotData(df = plotdata, point = key.points,
                                        end.use = EOL.hours, end.time = EOL.date)
      plotsObject$pl7 <- GenerateRiskPlot_V1(df = plotdata2, 
                                             key.dates = key.points)
      fin.key.points <- GeneratePoints_V1(df = plotdata, end.use = EOL.hours,
                                          end.time = EOL.date, full = FALSE)
      
      plotdata3 <- GenerateRiskPlotData(df = plotdata, point = fin.key.points,
                                        end.use = EOL.hours, end.time = EOL.date)
      plotsObject$pl8 <- GenerateRiskPlot_V1(df = plotdata3, 
                                             key.dates = fin.key.points,
                                             ID = paste0(input$ref_num, " - Replacement Decision Space"),
                                             subt = paste0("Risk in Time - Relating key dates to EOL criteria for ", input$ref_num),
                                             cap = paste0("Tail Number: ", input$ref_num))
      plotsObject$txt1 <- GetTextOfKeyDates(date.vec = fin.key.points)
      
      if (bOpnData) {
        showTab(inputId = "tabs", target = "WIP-Operational Use Considerations")
        incProgress(1.5, detail = paste("Generating Additional Data"))
        ##Need the plot data (1) to extract the dates for final plot & (2) to simplify
        # the ggplot code as the data frame becomes more complex than the plot.
        fit.choice.opn <- GenerateMultipleFits(ts.df = ts.opn.month, 
                                               h.int = int.h, 
                                               incl.fit = fit.index)
        
        plotdata4 <- V2_ComplexPlotData(df = cum.month.usage.data,
                                        fm = fit.choice,
                                        df2 = cum.month.opn.data,
                                        fm2 = fit.choice.opn,
                                        end.use = EOL.hours)
        
        f3 <- DataFitForecastPlotwEOL_V2(df = plotdata4,
                                         ID = paste0(input$ref_num, " - Forecast Plot with EOL Criteria"),
                                         subt = "Cumulative Monthly Flight Hours and Operational Flight Hours with Fit and Forecast (with 95% FI)",
                                         cap = NULL,
                                         ylab = "Monthly Flight Hours (Cumulative)",
                                         llab = "Monthly Flight Hours",
                                         llab2 = "Operational Flight Hours (weighted)",
                                         t.period = "months",
                                         end.use = EOL.hours)
        plotsObject$pl9 <- f3
        keyOpnPoints <- GeneratePoints_V2(df = plotdata4, end.use = EOL.hours,
                                          end.time = EOL.date, full = TRUE)
        plotdata5 <- GenerateRiskPlotData(df = plotdata4, point = keyOpnPoints,
                                          end.use = EOL.hours, end.time = EOL.date)
        
        plotsObject$pl10 <- GenerateRiskPlot_V2(df = plotdata5, key.dates = keyOpnPoints)
        finKeyOpnPoints <- GeneratePoints_V2(df = plotdata4, end.use = EOL.hours, 
                                             end.time = EOL.date, full = FALSE)
        plotdata6 <- GenerateRiskPlotData(df = plotdata4, point = finKeyOpnPoints,
                                          end.use = EOL.hours, end.time = EOL.date)
        plotsObject$pl11 <- GenerateRiskPlot_V2(df = plotdata6, key.dates = finKeyOpnPoints,
                                                ID = paste0(input$ref_num, " - Replacement Decision Space"),
                                                subt = paste0("Risk in Time - Relating key dates to EOL criteria for ", input$ref_num),
                                                cap = paste0("Tail Number: ", input$ref_num))
        plotsObject$txt2 <- GetTextOfKeyDates(date.vec = finKeyOpnPoints)
        
      } else {
        
        incProgress(1.5, detail = paste("No Additional Calculations"))
        hideTab(inputId = "tabs", target = "WIP-Operational Use Considerations")
        
        plotsObject$pl9 <- NULL
        plotsObject$pl10 <- NULL
        plotsObject$pl11 <- NULL
        plotsObject$txt2 <- NULL
      } ##/if/else(bOpnData)
      
      incProgress(0.05, detail = paste("COMPLETE"), Sys.sleep(1.0))
      
    }) ##/withProgress
  }) ##/observeEvent
  
  
  observeEvent( input$reCalcOpnInfo, {
    withProgress(message = 'Updating information', style = "notification", value = 0.0, {
      opn.data <- FixMonthlyDateDataFrame(df = raw.usage.data)
      opn.data <- SpliceDataFrame_OPN(df = opn.data, elem = "Opn_Hours",
                                      elem.data = opn.data[, 3],
                                      opn.fac = (input$opnUseFact/100))
      incProgress(0.1)
      ##### Create data sets by time interval and corresponding time series(ts)
      day.opn.data <- ConsolidateDailyUseData(df = opn.data,
                                              start.date = FOC.date)
      month.opn.data <- ConsolidateMonthlyUseData(df = day.opn.data)
      incProgress(0.1)
      print(sum(day.opn.data[, 2]))  ###DELETE LATER - For Troubleshooting
      print(sum(month.opn.data[, 2]))  ###DELETE LATER - For Troubleshooting
      ##### Create data sets by time interval and corresponding time series(ts)
      cum.month.opn.data <- GenerateCumData(df = month.opn.data, init = FOC.hours)
      incProgress(0.1)
      ts.opn.month <- ts(cum.month.opn.data[,2],
                         frequency = 12,
                         start = c(lubridate::year(min(cum.month.opn.data[,1])),
                                   lubridate::month(min(cum.month.opn.data[,1]))))
      print(tail(cum.month.opn.data[, 2], n = 1))  ###DELETE LATER - For Troubleshooting
      
      ##Need the plot data (1) to extract the dates for final plot & (2) to simplify
      # the ggplot code as the data frame becomes more complex than the plot.
      fit.choice.opn <- GenerateMultipleFits(ts.df = ts.opn.month,
                                             h.int = int.h,
                                             incl.fit = fit.index)
      incProgress(0.1)
      plotdata4 <- V2_ComplexPlotData(df = cum.month.usage.data,
                                      fm = fit.choice,
                                      df2 = cum.month.opn.data,
                                      fm2 = fit.choice.opn,
                                      end.use = EOL.hours)
      incProgress(0.1)
      f3 <- DataFitForecastPlotwEOL_V2(df = plotdata4,
                                       ID = paste0(input$ref_num, " - Forecast Plot with EOL Criteria"),
                                       subt = "Cumulative Monthly Flight Hours and Operational Flight Hours with Fit and Forecast (with 95% FI)",
                                       cap = NULL,
                                       ylab = "Monthly Flight Hours (Cumulative)",
                                       llab = "Monthly Flight Hours",
                                       llab2 = "Operational Flight Hours (weighted)",
                                       t.period = "months",
                                       end.use = EOL.hours)
      plotsObject$pl9 <- f3
      incProgress(0.1)
      keyOpnPoints <- GeneratePoints_V2(df = plotdata4, end.use = EOL.hours,
                                        end.time = EOL.date, full = TRUE)
      plotdata5 <- GenerateRiskPlotData(df = plotdata4, point = keyOpnPoints,
                                        end.use = EOL.hours, end.time = EOL.date)
      incProgress(0.1)
      plotsObject$pl10 <- GenerateRiskPlot_V2(df = plotdata5, key.dates = keyOpnPoints)
      finKeyOpnPoints <- GeneratePoints_V2(df = plotdata4, end.use = EOL.hours,
                                           end.time = EOL.date, full = FALSE)
      incProgress(0.1)
      plotdata6 <- GenerateRiskPlotData(df = plotdata4, point = finKeyOpnPoints,
                                        end.use = EOL.hours, end.time = EOL.date)
      incProgress(0.1)
      plotsObject$pl11 <- GenerateRiskPlot_V2(df = plotdata6, key.dates = finKeyOpnPoints,
                                              ID = paste0(input$ref_num, " - Replacement Decision Space"),
                                              subt = paste0("Risk in Time - Relating key dates to EOL criteria for ", input$ref_num),
                                              cap = paste0("Tail Number: ", input$ref_num))
      
      incProgress(0.1)
      plotsObject$txt2 <- GetTextOfKeyDates(date.vec = finKeyOpnPoints)
      
    }) ##/withProgress
  }) ##reactive input$opnUseFact
  
  output$dataTableHeader1 <- renderText({
    paste0("Available A/C Data")
  })
  
  output$fleet <- renderDataTable({
    fleet.data
  })
  
  output$dataTableHeader2 <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    paste0("Usage data for ", input$ref_num)
  })
  
  output$usage <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl1
  })
  
  output$plot1 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl1
  }) ##/output$plot1
  
  output$plot2 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl2
  }) ##/output$plot2
  
  output$plot3 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl3
  }) ##/output$plot4
  
  output$fit.summary <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl2 
  }, options = list(searching=0, ordering=0, processing=0, paging=0, info=0) #Removes extra stuff on datatable  
  ) ##/output$fits.summary
  
  output$fit.ranking <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl3
  }, options = list(searching=0, ordering=0, processing=0, paging=0, info=0) #Removes extra stuff on datatable
  ) ##/output$fits.rank
  
  output$final.rank <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl4
  }, options = list(searching=0, ordering=0, processing=0, paging=0, info=0) #Removes extra stuff on datatable
  ) ##/output$final.rank
  
  output$best.fit <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    fitrank <- plotsObject$tbl5
    paste0("The forecasting method applied: ", fitrank$Name[1],
           " [", fitrank$Fit[1], "]")
  }) ##/output$best.fit
  
  output$plot4 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl4
  }) ##/output$plot4
  
  
  output$plot5 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 5")
    suppressWarnings(print(plotsObject$pl5))
  }) ##/output$plot5  
  
  output$plot6 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 6")
    suppressWarnings(print(plotsObject$pl6))
  }) ##/output$plot6
  
  
  output$plot7 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 7 to Strip and Combine")
    suppressMessages(
      f2 <- StripGGPlot(plot = plotsObject$pl6, 
                        ID = paste0(input$ref_num, " - Aligning Forecast to Replacement Decision Space (Risk in Time)"))
    )
    r2 <- plotsObject$pl7
    suppressWarnings(
      gridExtra::grid.arrange(f2, r2,
                              nrow = 2,
                              heights = c(75,25))
    )
  })##/output$plot7
  
  output$plot8 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 8 Risk Window")
    plotsObject$pl8
  }) ##/output$plot8
  
  output$text1 <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$txt1
  }) ##/output$text1
  
  ##ADD REACTIVE INPUT SLIDER BAR HERE
  
  output$plot9 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 9")
    suppressWarnings(print(plotsObject$pl9))
  }) ##/output$plot6
  
  output$plot10 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 10 to Strip and Combine")
    suppressMessages(
      f4 <- StripGGPlot(plot = plotsObject$pl9, 
                        ID = paste0(input$ref_num, " - Aligning Forecast to Replacement Decision Space (Risk in Time)"))
    )
    r4 <- plotsObject$pl10
    suppressWarnings(
      gridExtra::grid.arrange(f4, r4,
                              nrow = 2,
                              heights = c(75,25))
    )
  }) ##/output$plot10
  
  output$plot11 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    print("Plot 11 Risk Window")
    plotsObject$pl11
  }) ##/output$plot11
  
  output$text2 <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$txt2
  }) ##/output$txt2
  
} ##/server
