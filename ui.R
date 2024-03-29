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
library(htmltools)

#load oscars data
oscars_df <- read.xlsx("oscars.xlsx")
#adjust gender labeling
oscars_df$gender[str_detect(oscars_df$gender, "female")] <- "Female"


intro_page <- tabPanel(
  "Intro",
    h1("Are the Oscars Racist, Sexist, or Both? An Exploratory Analysis"),
    hr(),
  h4("Authors"),
  p("Terresa Tran, Jacqueline Nguyen, Quinton Pharr, and Connor Chan"),
  hr(),
    h2("Introduction"),
    p("For our final project deliverable, we examined variables including, but not limited to, race, gender, and nominees/winners within the Oscars history. To ensure a wide pool of data and representation in the film industry, we chose an Oscars dataset spanning between 1927 and 2020. This aspect is important because we hope to provide findings to push toward additional diversity and inclusivity within the film industry. In addition to the three variables in focus, we were also introduced to variables including release date, nomination year, categories, and ceremony number. Although we focused on specific data within the dataset, we used all provided information to best inform our audience with our findings."),
    p("To best showcase our knowledge, we created three visualizations in the form of a bar graph, pie chart, and line graph. These graphs represent the “Top N Films to Display,” “Oscar Wins by Race,” and “Oscar Winners by Gender Per Year;” the graphs showcase our data in a visual manner, while also providing an interactive aspect for the audience’s understanding of the material."),
    p("To create these visualizations, we compiled variable data related questions including: 1. Which nominated films have the most amount of wins? 2. How is the spectrum of racial groups represented in the Oscars, specifically the Oscar winners? 3. Is there equal representation of men and women within the category of Oscar winners? These are the main things we are going to be examining, but we also look to make other discoveries along the way."),
    p("The dataset is sourced from Kaggle, which is a hub of online datasets. This specific dataset was created by Dharmik Donga. In the description, Donga mentions, “I found a basic dataset from Kaggle, but it did not have gender and race of the nominations and winners.” As a result, Donga most likely drew inspiration from another dataset and added gender and race categories. Donga also gives an acknowledgment to Raphael Fontes, who is the source for the basic dataset."),
    p("The main thing we need to consider is the limited data availability: The dataset only covers the nominations and winners from 1927 to 2020 and could be considered not as complete as there is no information available past 2020. Additionally, there could be biases in the Academy's voting process: The Academy's voting decides the nominations and winners, and might not be representative of the wider population. Another thing to take into account is user error, as Donga may have incorrectly transferred completely accurate data before publication of his dataset on Kaggle."),
    p("There could also be some discrepancies within how the categories are recorded. Within the dataset, the only things recorded as variables are for cinematography, writing, best picture, best actress/supporting actress, and best actor/supporting actor. As a result, the data is missing all of the other very important categories that come into play when making a movie. Another aspect to consider is how measuring/voting on these categories/nominees has changed over time, as the process in 1927 is possibly different than the process in 2020, and probably even again in 2023. These aspects, which are not in our control, potentially adjust dataset's criteria."),
  a(href = "https://www.kaggle.com/datasets/dharmikdonga/academy-awards-dataset-oscars?resource=download", "Kaggle Data Source"),
     imageOutput("image")
)

viz1_page <- tabPanel(
  "Viz1",
  fluidPage(
    sidebarLayout(
      sidebarPanel(numericInput(
        "top_n", 
        label = h4("Top N Films to Display"), 
        value = 5, 
        min = 1, 
      )),
      mainPanel(
        plotlyOutput("oscars1_plot"),
        h3("Chart Purpose", align = "center"),
        textOutput("viz1_desc"))
    )
  )
)

viz2_page <- tabPanel(
  "Viz2",
  fluidPage(
    sidebarLayout(
      sidebarPanel(selectInput("select", label = h4("Select Demographic"), 
                               choices = list("Race" = 1, "Gender" = 2), 
                               selected = 1
      )),
      mainPanel(
        plotlyOutput("oscars2_plot"),
        h3("Chart Purpose", align = "center"),
        textOutput("viz2_desc"))
    )
  )
)


viz3_page <- tabPanel(
  "Viz3",
  fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput(
        inputId = "year_slider",
        label = h4("Year Range"),
        min = 1928,
        max = 2020,
        value = c(1928, 2020),
        step = 1,
        ticks = T
      )),
      mainPanel(
        plotlyOutput("oscars3_plot"),
        h3("Chart Purpose", align = "center"),
        textOutput("viz3_desc"))
    )
  )
)


conclusion_page <- tabPanel(
     "Conclusion",
     h2("Conclusion"),
     hr(),
     p("In conclusion, our project analyzed various aspects of the Oscars including categories such as race, gender, nominees, and winners to contribute to the larger discussion on diversity and inclusivity within the film industry. Our findings provide important insights into the representation of marginalized groups in the cinamtic industry."),
     p("One major takeaway from our analysis is that it is extremely uncommon for films to surpass the threshold of 9 wins, with only four films ever achieving this feat in the history of the Oscars. This highlights the difficulty of creating a film that is not only critically acclaimed, but also successful in winning multiple awards. Another major takeaway is that people who are white encompass 95.2% of all Oscar winners. This statistic shows the lack of representation of people of color in the film industry, despite the increasing awareness and efforts towards greater diversity. Lastly, our analysis revealed that in the history of the Oscars, there have always been more men than women winners. However, in the past century, men winners have steadily decreased, while women winners continue to increase in Oscar winnings. This trend indicates a shift towards greater representation and inclusivity for women in the film industry."),
     p("Despite the limitations of our dataset, our project provides a starting point for future research and discussion on the topic of diversity and inclusivity within the film industry. Further analysis of marginalized groups, such as individuals with disabilities or non-binary gender identities, or in independent films, could contribute to a more comprehensive understanding of the challenges these marginalized groups face in the industry. By continuing to engage in dialogue, we can work towards creating a more representative industry for all.")
     
)

# Define UI for application that draws a histogram
ui <- navbarPage(
     "Oscars Diversity Analysis",
     intro_page,
     viz1_page,
     viz2_page,
     viz3_page,
     conclusion_page
)