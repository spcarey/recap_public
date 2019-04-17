


ui = fluidPage(
  
  
  
  # Application title
  title = "RECAP app",  # This is what shows up in the browser tab
  
  h4(tags$b("UNCLASSIFIED"), # UPDATE: Insert your classfication level. 
     style = "color : green", # UPDATE: Classification Color
     align = "Center"),       # Another classification banner at bottom of UI
  
  div(id = "header", align = "center", 
      img(src='USSOCOM.png', align = "left",width = 125, height = 125 ),
      img(src='Center_for.png', align = "right", width = 175, height = 130 ),
      img(src='JSOC.png', align = "right", width = 125, height = 125 ),
      h1("Aircraft Recapitalization Forecast")), # UPDATE: Add Custom  Title
  
  tabsetPanel(id = "tabs",
              tabPanel("Introduction",
                       div(id = "intro", 
                           wellPanel(h2("Purpose"),
                                     
                                     h4("This app was developed to forecast when to consider recapitalizing aircraft (A/C) based on end of life (EOL) criteria for flight hours and time.  These criteria are also derived from the airworthiness limitation (AL) for the A/C. The app takes a flight log as an input and transforms it into cumulative monthly flight hours (usage) for forecasting.  These forecasts establish a reasonable expectation as to when an A/C should be considered for recapitalization."),
                                     h1(" "),
                                     hr(style="border-color: black;"),
                                     h2("Instructions"),
                                     h4(
                                       tags$b("Step 1"),
                                       " (Required): In the next tab, ",
                                       tags$i("Data Select"),
                                       ", use the drop down to select the A/C of interest by tail number."),
                                     h4(
                                       tags$b("Step 2"),
                                       "(Optional): The ",
                                       tags$i("Data View"), 
                                       " tab displays the usage data visually."),
                                     h4(
                                       tags$b("Step 3"),
                                       " (Optional): The ", 
                                       tags$i("Forecasting Methods & Fit"), 
                                       " tab provides information on the forecasting methods examined, summaries and rankings of forecasting methods, and initial visualizations."),
                                     h4(
                                       tags$b("Step 4"), 
                                       " (Required): The ",
                                       tags$i("Forecast & Risk Window"),
                                       " tab displays the answer.  It contains visualizations that summarize the time series forecast, the development of the risk window to forecast replacement, and the key dates to consider replacement."),
                                     h4(
                                       tags$b("Step 5"), 
                                       " (Conditional): The ",
                                       tags$i("WIP-Operational Use Considerations"),
                                       " tab displays a methodology that additionally accounts for operational use - use beyond 'normal' operations.  Note:  This tab only appears when there are 'operational use' data to augment the answer derived within the ",
                                       tags$i("Forecast & Risk Window"),
                                       " tab."),
                                     h1()),
                           wellPanel(h2("Package information"),
                                     h4("The following software/packages, with versions, were used in the development of this application."), 
                                     h5("R 3.4.4"),   
                                     h5("RStudio 1.1.456"),
                                     h5("data.table 1.11.8"),
                                     h5("dplyr 0.7.8"),
                                     h5("forecast 8.4"),
                                     h5("ggplot2 3.1.0"),
                                     h5("grid 3.4.4"),
                                     h5("gridExtra 2.3"),
                                     h5("lubridate 1.7.4"),
                                     h5("scales 1.0.0"),
                                     h5("shiny 1.0.5"),
                                     h5("stats 3.4.4"),
                                     h5("zoo 1.8-4")
                                     
                           ) ##/wellPanel
                       )
              ), ##/tabPanel - Introduction
              
              tabPanel("Data Select",
                       wellPanel(h4("The analysis begins with the selection of the A/C tail number from the drop down menu below."),
                                 h4("A complete list of A/C is available in the first table below. This table of A/C includes other information such as procurement dates and end of lifecycle (EOL or AL) criteria specific to the A/C."),
                                 h4("The second table appears after making an A/C choice.  The second table displays the A/C flight log that is the foundational raw data that establishes a pattern of usage for the analysis."),
                                 h4("Please note that the majority of the data analysis is performed when an A/C is selected below.  The app may pause, with a progress bar appearing.  This is normal.")),
                       
                       selectInput("ref_num", h4(em(p(strong("Please select a tail number."))), style = "color: green" ), 
                                   choices = c(Choose="", as.list(fleet.data$Tail_Num)),
                                   selectize = TRUE),
                       
                       wellPanel(
                         wellPanel(
                           h3(textOutput("dataTableHeader1")), 
                           style = "background-color: lightskyblue" 
                         ),
                         dataTableOutput("fleet"), align = "center"),
                       
                       wellPanel(
                         wellPanel(
                           h3(textOutput("dataTableHeader2")),  
                           style = "background-color: lightskyblue"
                         ),
                         dataTableOutput("usage"), align = "center")
                       
              ), ##/tabPanel - Data SELECT
              
              tabPanel("Data View",
                       wellPanel(
                         h4("Visualizations of the data are displayed below.  The following plots provide a quick idea of what the data looks like, providing an indication of frequency, variance, and sparsity."),
                         h4("The first two plots are paired by use per day and month, respectively."),
                         h4("For the third plot the data is transformed to a running total or cumulative use by month over time.  This cumulative data is used to forecast the usage.")
                       ),
                       
                       wellPanel(
                         wellPanel(
                           h3("Daily and Monthly Data"), 
                           style = "background-color: lightskyblue" ),
                         plotOutput("plot1"), align = "center"),
                       
                       wellPanel(
                         plotOutput("plot2"), align = "center"),
                       wellPanel(
                         wellPanel(
                           h3("Cumulative Monthly Data"), 
                           style = "background-color: lightskyblue" ),
                         plotOutput("plot3"), align = "center")
              ), ##/tabPanel - Data View
              
              tabPanel("Forecasting Methods & Fit", 
                       wellPanel(
                         wellPanel(
                           h4("There are 10 forecast methods explored, ranging from linear regression to more complex machine learning techniques such as Seasonal Trend-Loess, Holt Winters, and ARIMA."),
                           style = "background-color: lightskyblue", align = "center" ),
                         h4("The selection below defaults to use all 10 forecast methods and apply one of the best forecast fits.  This selection enables an analyst to apply other forecast methods.  Be advised that not all forecast methods are applicable to all data sets and/or a sub-optimal answer is highly possible.  Use individual methods with caution and prudent review.")
                       ), ##/wellPanel
                       
                       radioButtons("rbFit", "Manually Select Forecast Method (Optional):",
                                    choiceNames = c("Auto fit - Test all (DEFAULT)",
                                                    GenerateMultipleFitMethods()),
                                    choiceValues = c(0:10),
                                    selected = 0,
                                    inline = TRUE
                       ),
                       
                       wellPanel(                     
                         tags$div("The forecast methods and the resulting accuracy metrics (expected performance) are displayed in the tables below.  These metrics include:",
                                  tags$br(),
                                  tags$br(),
                                  tags$ul(
                                    tags$li("ME - Mean error is the average of all the errors in a set."),
                                    tags$li("RMSE - Root-mean-squared error (RMSE) is used to measure the differences between values predicted by a model the values observed.  Also known as root-mean-squared deviation (RMSD)."),
                                    tags$li("MAE - Mean absolute error is a measure of difference between two continuous variables."),
                                    tags$li("MPE - Mean percentage error (MPE) is the computed average of percentage errors by which forecasts of a model differ from actual values of the quantity being forecast.") ,
                                    tags$li("MAPE - Mean absolute percentage error is a measure of prediction accuracy of a forecasting method. Also known as mean absolute percentage deviation (MAPD). Heuristic goal < 20%."),
                                    tags$li("MASE - Mean absolute scaled error is a measure of the forecast accuracy."),
                                    tags$li("ACF1 - Autocorrelation of errors at lag 1 is a measure that determines the correlation between past and future data points in a time series.  Autocorrelation values are the most obvious way to measure the linearity of the dependence between the current values of the time series versus the lagged values of the time series."), 
                                    type = "disc"),
                                  tags$br(),
                                  tags$br()
                         ),
                         
                         wellPanel(h3("Fit & Accuracy Summary Table"),
                                   style = "background-color: lightskyblue", align = "center" ),
                         
                         dataTableOutput("fit.summary"),
                         tags$div(tags$br(),
                                  tags$br()
                         ),
                         wellPanel(h3("Normalized Fit & Accuracy Summary Table"),
                                   style = "background-color: lightskyblue", align = "center" ),
                         dataTableOutput("fit.ranking"),
                         wellPanel(h3("Ordered List of Average Normalized Fit & Accuracy"),
                                   style = "background-color: lightskyblue", align = "center" ),
                         tags$div(h3("The 'best fit' (the best model to use for the forecast) is selected by the lowest average normalized values of the fit & accuracy metrics."),
                                  tags$br()
                         ),
                         dataTableOutput("final.rank"),
                         wellPanel(h3(textOutput("best.fit")),
                                   style = "background-color: lightgreen", align = "center" )
                       ),
                       wellPanel(wellPanel(h3("Initial Fit & Forecast Plots"),
                                           style = "background-color: lightskyblue", align = "center" ),
                                 plotOutput("plot4")
                       ),
                       wellPanel(plotOutput("plot5")
                       )
              ), ##/tabPanel - Forecast Methods
              
              tabPanel("Forecast & Risk Window",
                       wellPanel(
                         wellPanel(
                           h3("Forecast (to EOL criteria)"),
                           style = "background-color: lightskyblue", 
                           align = "center" 
                         ),
                         h4("The usage data is forecast to determine when EOL conditions are met.  The visualization below displays the usage data (green bars), the fit of the usage data (black line), the forecast (blue line), the forecast interval (gray shading), and the EOL criteria for usage and time (red lines)."),
                         h4("The intersection of the forecast (blue line) with the EOL criteria (red lines) indicates the most likely date of replacement.  The earliest intersection of the forecast interval (gray shading) indicates when the earliest projected replacement date could occur.  Rather than a specific date, the solution could include a range of dates, or window."),
                         
                         plotOutput("plot6"),
                         
                         tags$br(),
                         tags$br(),
                         
                         wellPanel(
                           h3("Developing the Forecast Window"),
                           style = "background-color: lightskyblue", 
                           align = "center" 
                         ),
                         
                         h4("The plot below is a transition visualization, aligning the forecast plot with a color band that includes the key dates.  The key dates are identified to inform the solution of when replacement is needed.  The dates associated with these points [and meaning of colors] are:"),
                         
                         tags$ul(
                           tags$li("The FOC date of the A/c -- the beginning of the usage data. [Green - The A/c does not require replacement.]"),
                           tags$li("The last known point of data. [Green - The A/c does not require replacement.]"),
                           tags$li("The first instance of the 95% forecast interval intersecting the EOL criteria. [Yellow - Earliest forecasted replacement point, consider replacement.] "),
                           tags$li("The first instance of the forecast mean intersecting either of the EOL criteria. [Red - Projected most likely replacement point, must replace.]"), 
                           style = "disc"),
                         tags$br(),
                         tags$br(),
                         
                         plotOutput("plot7"),
                         
                         tags$br(),
                         tags$br(),
                         
                         wellPanel(
                           h3("Forecast Window"),
                           style = "background-color: lightskyblue", 
                           align = "center" 
                         ),
                         h4("The plot below is the forecast window of when to plan for replacement. The forecast window begins at the end of the known data, which is also the beginning of the forecast.  This color band ends with the forecast meeting the EOL criteria.  As above, key dates are established.  Specifically for this visual, the key dates [and colors] are:"),
                         
                         tags$ul(
                           tags$li("The last known point of data. [Green - The A/c does not require replacement.]"),
                           tags$li("The first instance of the 95% forecast interval intersecting the EOL criteria. [Yellow - Earliest forecasted replacement point, consider replacement.] "),
                           tags$li("The first instance of the forecast mean intersecting either of the EOL criteria. [Red - Projected most likely replacement point, must replace.]"), 
                           style = "disc"),
                         tags$br(),
                         
                         div(plotOutput("plot8", width = "40%"), align = "center"),
                         
                         tags$br(),       
                         wellPanel(
                           h3(textOutput("text1")),
                           style = "background-color: lightgreen", 
                           align = "center"
                         )
                         
                       ) ##/wellPanel
              ), ##/tabPanel - Forecast & Risk Window
              
              tabPanel(title = "WIP-Operational Use Considerations", 
                       wellPanel(
                         wellPanel(
                           h3("WIP - Operational Use Considerations"),
                           style = "background-color: lightpink", align = "center" ),
                         h4('This section augments the previous section by adding the concept of operational usage.  Operational usage is A/c usage conducted outside normal operating limits.  There are two components to operational usage: (1) mileage or time and (2) amount of force applied beyond normal use.  The operational usage time is weighted and combined with the non-operational time, thus increasing the usage time.   The amount of time the A/c performs these activities is weighted by the % increase over normal operating conditions.  This operational usage approach was stimulated by articles such as "Usage Based Fatigue Damage Calculation" (Shanthakumaran, 2010) and "Military Equipment Usefulness Life Study - Phase II." (OSD-AT&L, 2008)'),
                         h4("The operational usage miles are imported, weighted accordingly by the operational usage factor, and added with the total number of 'normal' miles (no double tapping of miles.)  The forecasting method used for the operational use is the same as the previous method established. The operational use factor, the amount of force applied beyond normal use, is input below.")
                       ), ##/wellPanel
                       
                       wellPanel(
                         wellPanel(h3("Operational Use Factor"),
                                   style = "background-color: lightskyblue", align = "center"),
                         fluidRow(
                           column(width=8,
                                  h4("Select the operational use factor (the amount of force applied beyond normal use) on the right."),
                                  h4("As an example, a truck with a normal operating cargo capacity of up to 100 units is used to transport 125 units. The operational use factor in this case is 25%."),
                                  h4("[(125 - 100) / 100 = 0.25 = 25%]")
                           ),
                           
                           column(width=4,
                                  sliderInput(inputId = "opnUseFact", 
                                              label = "Selest Operational Use Factor (% over normal use)",
                                              min = 0,
                                              max = 100,
                                              value = 25,
                                              step = 1
                                  ),
                                  actionButton(inputId = "reCalcOpnInfo",
                                               label = "Update with New Factor")
                           )
                         )
                       ), ##wellPanel
                       
                       wellPanel(
                         wellPanel(
                           h3("Forecast with Operational Considerations"),
                           style = "background-color: lightskyblue", align = "center"),
                         
                         
                         h4("The plots below are generated using the same methodology as the previous tab, Developing the Forecast Window.  The addition is that the operational use data is also included. The 'normal' and 'operational' usage data are both forecast to determine when EOL conditions are met.  The visualization below display the 'normal' usage data (green bars), 'operational' usage data (tan bars), the fit of the usage data (black line), the forecast (blue line), the forecast interval (gray shading), and the EOL criteria for usage and time (red lines)."),
                         h4("The intersection of the forecasts (blue lines) with the EOL criteria (red lines) indicates the most likely date of replacement.  The earliest intersection of the forecast interval (gray shading) indicates when the earliest projected replacement date could occur.  Rather than a specific date, the solution could include a range of dates, or window."),
                         
                         plotOutput("plot9"),
                         
                         tags$br(),
                         tags$br(),
                         
                         wellPanel(
                           h3("Developing the Forecast Window"),
                           style = "background-color: lightskyblue", 
                           align = "center" 
                         ),
                         
                         h4("The plot below is a transition visualization, aligning the forecast plot with a color band that includes the key dates.  The key dates are identified to inform the solution of when replacement is needed."),
                         
                         tags$br(),
                         
                         plotOutput("plot10"),
                         
                         tags$br(),
                         tags$br(),
                         
                         wellPanel(
                           h3("Forecast Window"),
                           style = "background-color: lightskyblue", 
                           align = "center" 
                         ),
                         h4("The plot below is the forecast window of when to plan for replacement. The forecast window begins at the end of the known data, which is also the beginning of the forecast.  This color band ends with the forecast meeting the EOL criteria.  The colors are defined as:"),
                         
                         tags$ul(
                           tags$li("Green - The A/c does not require replacement."),
                           tags$li("Yellow - Earliest forecasted replacement point, consider replacement."),
                           tags$li("Red - Projected most likely replacement point, must replace."), 
                           style = "disc"),
                         tags$br(),
                         
                         div(plotOutput("plot11", width = "40%"), align = "center"),
                         
                         tags$br(),
                         
                         wellPanel(
                           h3(textOutput("text2")),
                           style = "background-color: lightgreen", 
                           align = "center"
                         )
                         
                       ) ##/wellPanel 
              ), ##/tabPanel - WIP-Operational Use Considerations
              h4(tags$b("UNCLASSIFIED"), # UPDATE: Insert your classfication level. 
                 style = "color : green", # UPDATE: Classification Color
                 align = "Center")
              
  ), ##/tabSetPanel
  
  tagList(
    singleton(
      tags$head(
        tags$style(type="text/css", "tfoot {display:none;}")
      )
    )
  ),
  dataTableOutput('customtable') 
) ##/ ui
