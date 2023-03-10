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
                    paste("This bar plot shows the top", input$top_n, "films with the most Oscar wins. This specific graph represents the top award-worthy films with the greatest amount of Oscar wins. By using the widget, the number of films ranked can be changed in order to see the different number of wins in descending order. The initial value set is 5, so we can see that Titanic won 12 awards, Lord of the Rings and Ben-Hur tied for 11 awards, West side Story won 10 awards, and The Last Emperor, The English Patient, and Gigi tied for winning 9 awards at the Oscars. As the value in the widget is adjusted, so will the number of films shown on the bar graph. Overall, the graph provides an overview of the films with the highest amount of Oscar wins, showcasing the accomplishments of these movies. The graph layout is deliberately set up to make it easy to compare film nominations with one another. In addition, the chart reveals trends of Oscar wins over the history of the Oscars, critiques, and film enthusiasts to find and discover which films tend to succeed at the Oscars.")
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
     
     output$viz2_desc <- renderText({
          if (input$select == 1) {
               "This pie chart shows the distribution of Oscar wins by race. In the pie plotly above, we decided to represent the proportions of race diversity at the Oscars. Pie charts are a great way to organize and show data by representing the different slices as a percentage of the whole. By using the widget, you can switch between the race-based graph and the gender-based graph. Our pie chart slices are broken up into four recorded races: White, Black, Asian, and Hispanic. Through this, we were able to identify that the majority of the winners/nominees were White. When hovering over the pie section, the exact value can be read."
          } else {
               "This pie chart shows the distribution of Oscar wins by gender. In the pie plotly above, we decided to represent the proportions of race diversity at the Oscars. Pie charts are a great way to organize and show data by representing the different slices as a percentage of the whole. By using the widget, you can switch between the race-based graph and the gender-based graph. Our pie chart slices are broken up into ftwo recorded genders: Male and Female. Through this, we were able to identify that the majority of the winners/nominees were White. When hovering over the pie section, the exact value can be read."
          }
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
     
     output$viz3_desc <- renderText({
          paste("The bar graph above features Oscar data between the years 1928 and 2020. This specific graph represents the top award-worthy films with the greatest amount of Oscar wins. By using the widget, the number of films ranked can be changed in order to see the different number of wins in descending order. The initial value set is 5, so we can see that Titanic won 12 awards, Lord of the Rings and Ben-Hur tied for 11 awards, West side Story won 10 awards, and The Last Emperor, The English Patient, and Gigi tied for winning 9 awards at the Oscars. As the value in the widget is adjusted, so will the number of films shown on the bar graph. Overall, the graph provides an overview of the films with the highest amount of Oscar wins, showcasing the accomplishments of these movies. The graph layout is deliberately set up to make it easy to compare film nominations with one another. In addition, the chart reveals trends of Oscar wins over the history of the Oscars, critiques, and film enthusiasts to find and discover which films tend to succeed at the Oscars.")
     })
}