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
library("htmltools")

#load oscars data
oscars_df <- read.xlsx("oscars.xlsx")
#adjust gender labeling
oscars_df$gender[str_detect(oscars_df$gender, "female")] <- "Female"


# Define server logic required to draw a histogram
server <- function(input, output) {
     
     output$image <- renderImage({
          # Specify the path to your image
          filename <- "Oscars_Image.png"
          # Return a list containing the filename and the image format
          list(src = filename, contentType = "image/png")
     }, deleteFile = FALSE)
     
    #viz1
     output$oscars1_plot <- renderPlotly({
          
          selectedPlot <- oscars_df %>% 
               drop_na(film, winner) %>% 
               group_by(film) %>% 
               summarize(wins = sum(winner, na.rm = TRUE)) %>% 
               arrange(desc(wins)) %>% 
               top_n(input$top_n)
          
               
               output$viz1_desc <- renderText({
                    paste("This plot shows the top", input$top_n, "films with the most Oscar wins. The films are                     ranked in descending order of their wins and are represented by bars. The height of each bar                     represents the number of wins for a particular film. The film names are listed on the y-axis                     , and the number of wins is shown on the x-axis. The plot can be used to compare the number                     of wins between different films.")
               })
               
          
          oscars1_plot <- ggplot(data = selectedPlot) +
               geom_col(mapping = aes(
                    x = reorder(film, wins),
                    y = wins,
                    fill = film
               )) +
               
               labs(
                    title = paste0("Top ", input$top_n, " Films with Most Oscar Wins"),
                    x = "Film",
                    y = "Wins"
               ) +
               coord_flip() +
               scale_y_continuous(breaks = seq(1, 13, 1)) +
               theme(legend.position = "none")
          
          ggplotly(oscars1_plot)
          
          return(oscars1_plot)
     })
     
     #viz2
     output$oscars2_plot <- renderPlotly({
       
       race_data <- oscars_df %>% 
         drop_na(Race, winner) %>%
         group_by(Race) %>% 
         summarize(wins = sum(winner)) %>% 
         mutate(percent = wins / sum(wins) * 100) %>% 
         arrange(desc(wins))
       
       gender_data <- oscars_df %>% 
         drop_na(gender, winner) %>%
         group_by(gender) %>% 
         summarize(wins = sum(winner)) %>% 
         mutate(percent = wins / sum(wins) * 100) %>% 
         arrange(desc(wins))
       
       # create a pie chart
       if (input$select == 1) {
         oscars2_plot <- plot_ly(race_data, labels = ~Race, values = ~percent, type= "pie") %>% 
           layout(title = "Oscar Wins by Race",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       } else if (input$select == 2) {
         oscars2_plot <- plot_ly(gender_data, labels = ~gender, values = ~percent, type= "pie") %>% 
           layout(title = "Oscar Wins by Gender",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         
       }
       return(ggplotly(oscars2_plot))
     })
     
     #viz3
     output$oscars3_plot <- renderPlotly({
          
          oscars_data <- oscars_df %>%
               mutate(gender = if_else(gender == "female", "Female", gender))
          
          # create a data frame with number of winners by gender and year
          gender_data <- oscars_data %>% 
               drop_na(gender, year_ceremony, winner) %>%
               group_by(gender, year_ceremony) %>% 
               summarize(wins = sum(winner)) %>% 
               filter(year_ceremony >= input$year_slider[1], year_ceremony <= input$year_slider[2])
          
          # create a line plot
          oscars3_plot <- ggplot(gender_data, aes(x = year_ceremony, y = wins, color = gender)) +
               geom_line() +
               labs(title = "Oscar Winners by Gender per year", x = "Ceremony Year", y = "Number of Winners", color = "Gender") +
               scale_x_continuous(breaks = seq(1920, 2020, 10))
          
          
          ggplotly(oscars3_plot)
          
          return(oscars3_plot)
     })
}