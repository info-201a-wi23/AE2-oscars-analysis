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

#load oscars data
oscars_df <- read.xlsx("oscars.xlsx")
#adjust gender labeling
oscars_df$gender[str_detect(oscars_df$gender, "female")] <- "Female"


intro_page <- tabPanel(
     "Intro",
     textOutput("intro_desc")
)


viz1_page <- tabPanel(
     "Viz1",
     numericInput(
          "top_n", 
          label = h4("Top N Films to Display"), 
          value = 5, 
          min = 1, 
          max = 20
     ),
     plotlyOutput("oscars1_plot")
)

viz2_page <- tabPanel(
     "Viz2",
     fluidPage(selectInput("select", label = h4("Select Demographic"), 
                   choices = list("Race" = 1, "Gender" = 2), 
                   selected = 1),
       
       hr(),
       fluidRow(column(2, verbatimTextOutput("identity")))
       
     ),
     plotOutput("oscars2_plot")
)

viz3_page <- tabPanel(
     "Viz3",
     sliderInput(
       inputId = "year_slider",
       label = h5("Year Range"),
       min = 1928,
       max = 2020,
       value = c(1928, 2020),
       step = 1,
       ticks = T
     ),
     plotlyOutput("oscars3_plot")
)

conclusion_page <- tabPanel(
     "Conclusion",
     textOutput("conclusion_desc")
)

# Define UI for application that draws a histogram
ui <- navbarPage(
     "Project Title",
     intro_page,
     viz1_page,
     viz2_page,
     viz3_page,
     conclusion_page
)