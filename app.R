#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("ggplot2")
library("plotly")
library("dplyr")
library("openxlsx")
library(tidyverse)
library(shiny)
source('ui.R')
source('server.R')
library("htmltools")

oscars_df <- read.xlsx("oscars.xlsx")

# Run the application 
shinyApp(ui = ui, server = server)
