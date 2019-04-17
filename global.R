
rm(list = ls())

library(shiny)
library(data.table)
library(forecast)

source("Func.R", local = TRUE)
source("PlotFunc.R", local = TRUE)

fleet.data <- read.csv("data/Fleet_Info.csv",
                       stringsAsFactors = FALSE)
# Reads Data from Fleet Data csv. This csv  sets the base for the 
# EOL time and hours parameters and the drop down menu choices on the Data
# select tab. Make sure the tail numbers match the file name tail numbers in 
# the data csv's