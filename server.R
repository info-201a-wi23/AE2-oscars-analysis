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

oscars_df <- read.xlsx("oscars.xlsx")


# Define server logic required to draw a histogram
server <- function(input, output) {
     
     #intro
     output$intro_desc <- renderText({
          
     })
     #viz1
     output$oscars1_plot <- renderPlotly({
          
          oscars1_plot <- oscars_df %>% 
               drop_na(film, winner) %>% 
               group_by(film) %>% 
               summarize(wins = sum(winner, na.rm = TRUE)) %>% 
               arrange(desc(wins)) %>% 
               top_n(5)
          
          
          ggplot(data = oscars1_plot) +
               geom_col(mapping = aes(
                    x = reorder(film, wins),
                    y = wins,
                    fill = film
               )) +
               
               labs(
                    title = "Films with most Oscar wins",
                    x = "film",
                    y = "wins"
               ) +
               coord_flip() +
               scale_y_continuous(breaks = seq(1, 13, 1)) +
               theme(legend.position = "none")
          
          ggplotly(oscars1_plot)
          
          return(oscars1_plot)
     })
     #viz2
     output$oscars2_plot <- renderPlotly({
          
          ggplotly(oscars2_plot)
          
          return(oscars2_plot)
     })
     #viz3
     output$oscars3_plot <- renderPlotly({
          
          ggplotly(oscars3_plot)
          
          return(oscars3_plot)
     })
     #conclusion
     output$conclusion_desc <- renderText({
          
     })

}

