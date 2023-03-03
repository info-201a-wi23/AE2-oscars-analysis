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
          "This is the introduction page."
     })
     #viz1
     output$oscars1_plot <- renderPlotly({
          
          selectedPlot <- oscars_df %>% 
               drop_na(film, winner) %>% 
               group_by(film) %>% 
               summarize(wins = sum(winner, na.rm = TRUE)) %>% 
               arrange(desc(wins)) %>% 
               top_n(5)
          
          
          oscars1_plot <- ggplot(data = selectedPlot) +
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
     output$oscars2_plot <- renderPlot({
          
          race_data <- oscars_df %>% 
               drop_na(Race, winner) %>%
               group_by(Race) %>% 
               summarize(wins = sum(winner)) %>% 
               mutate(percent = wins / sum(wins) * 100) %>% 
               arrange(desc(wins))
          
          # create a pie chart
          oscars2_plot <- ggplot(race_data, aes(x = "", y = percent, fill = Race)) +
               geom_bar(width = 1, stat = "identity") +
               coord_polar(theta = "y", start = 0) +
               labs(title = "Oscar Wins by Race", fill = "Race") +
               theme_void()
          
          return(oscars2_plot)
     })
     #viz3
     output$oscars3_plot <- renderPlotly({
          
          oscars_data <- oscars_df %>%
               mutate(gender = if_else(gender == "female", "Female", gender))
          
          # create a data frame with number of winners by gender and year
          gender_data <- oscars_data %>% 
               drop_na(gender, year_ceremony, winner) %>%
               group_by(gender, year_ceremony) %>% 
               summarize(wins = sum(winner)) 
          
          # create a line plot
          oscars3_plot <- ggplot(gender_data, aes(x = year_ceremony, y = wins, color = gender)) +
               geom_line() +
               labs(title = "Oscar Winners by Gender per year", x = "Ceremony Year", y = "Number of Winners", color = "Gender") +
               scale_x_continuous(breaks = seq(1920, 2020, 10))
          
          
          ggplotly(oscars3_plot)
          
          return(oscars3_plot)
     })
     #conclusion
     output$conclusion_desc <- renderText({
          "This is the conclusion page."
     })
     
}