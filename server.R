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


# Define server logic required to draw a histogram
server <- function(input, output) {
     
     #intro
     output$intro_desc <- renderText({
       
"Terresa Tran (tran10@uw.edu), Jacqueline Nguyen (jackietn@uw.edu), Quinton Pharr (qpharr@uw.edu), Connor Chan (cachan25@uw.edu)

For our final project deliverable, we examined variables including, but not limited to, race, gender, and nominees/winners within the Oscars history. To ensure a wide pool of data and representation in the film industry, we chose an Oscars dataset spanning between 1927 and 2020. This aspect is important because we hope to provide findings to push toward additional diversity and inclusivity within the film industry. In addition to the three variables in focus, we were also introduced to variables including release date, nomination year, categories, and ceremony number. Although we focused on specific data within the dataset, we used all provided information to best inform our audience with our findings.
To best showcase our knowledge, we created three visualizations in the form of a bar graph, pie chart, and line graph. These graphs represent the “Top N Films to Display,” “Oscar Wins by Race,” and “Oscar Winners by Gender Per Year;” the graphs showcase our data in a visual manner, while also providing an interactive aspect for the audience’s understanding of the material.
To create these visualizations, we compiled variable data related questions including: 1. Which nominated films have the most amount of wins? 2. How is the spectrum of racial groups represented in the Oscars, specifically the Oscar winners? 3. Is there equal representation of men and women within the category of Oscar winners? These are the main things we are going to be examining, but we also look to make other discoveries along the way.

The dataset is sourced from Kaggle, which is a hub of online datasets. This specific dataset was created by Dharmik Donga. In the description, Donga mentions, “I found a basic dataset from Kaggle, but it did not have gender and race of the nominations and winners.” As a result, Donga most likely drew inspiration from another dataset and added gender and race categories. Donga also gives an acknowledgment to Raphael Fontes, who is the source for the basic dataset.
The main thing we need to consider is the limited data availability: The dataset only covers the nominations and winners from 1927 to 2020 and could be considered not as complete as there is no information available past 2020. Additionally, there could be biases in the Academy's voting process: The Academy's voting decides the nominations and winners, and might not be representative of the wider population. Another thing to take into account is user error, as Donga may have incorrectly transferred completely accurate data before publication of his dataset on Kaggle.
There could also be some discrepancies within how the categories are recorded. Within the dataset, the only things recorded as variables are for cinematography, writing, best picture, best actress/supporting actress, and best actor/supporting actor. As a result, the data is missing all of the other very important categories that come into play when making a movie. Another aspect to consider is how measuring/voting on these categories/nominees has changed over time, as the process in 1927 is possibly different than the process in 2020, and probably even again in 2023. These aspects, which are not in our control, potentially adjust dataset's criteria.

URL link to the original source of the data:
[https://www.kaggle.com/datasets/dharmikdonga/academy-awards-dataset-oscars?resource=download]"

          })
     

     
     output$oscars1_plot <- renderPlotly({
          
          selectedPlot <- oscars_df %>% 
               drop_na(film, winner) %>% 
               group_by(film) %>% 
               summarize(wins = sum(winner, na.rm = TRUE)) %>% 
               arrange(desc(wins)) %>% 
               top_n(input$top_n)
          
          
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
     #conclusion
     output$conclusion_desc <- renderText({
          "In conclusion, our project analyzed various aspects of the Oscars including categories such as race, gender, nominees, and winners to contribute to the larger discussion on diversity and inclusivity within the film industry. Our findings provide important insights into the representation of marginalized groups in the cinamtic industry.
One major takeaway from our analysis is that it is extremely uncommon for films to surpass the threshold of 9 wins, with only four films ever achieving this feat in the history of the Oscars. This highlights the difficulty of creating a film that is not only critically acclaimed, but also successful in winning multiple awards.
Another major takeaway is that people who are white encompass 95.2% of all Oscar winners. This statistic shows the lack of representation of people of color in the film industry, despite the increasing awareness and efforts towards greater diversity.
Lastly, our analysis revealed that in the history of the Oscars, there have always been more men than women winners. However, in the past century, men winners have steadily decreased, while women winners continue to increase in Oscar winnings. This trend indicates a shift towards greater representation and inclusivity for women in the film industry.
Despite the limitations of our dataset, our project provides a starting point for future research and discussion on the topic of diversity and inclusivity within the film industry. Further analysis of marginalized groups, such as individuals with disabilities or non-binary gender identities, or in independent films, could contribute to a more comprehensive understanding of the challenges these marginalized groups face in the industry. By continuing to engage in dialogue, we can work towards creating a more representative industry for all."
     })
     
}