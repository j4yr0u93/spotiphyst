library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(ggcorrplot)
library(Hmisc)


load("./data/j4yr0u93_music_data_2019.RData")

`Genre Options` <- list("pop", "hip hop", "rock", "rap", "alternative metal", "alternative r&b", 
                        "alternative rock", "neo-psychedelic", "indie rock", "classical",
                        "lo-fi beats", "indietronica", "trap", "sludge metal", "post-grunge")


ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Spotiphyst"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("name"),
      uiOutput("project"),
      br(),
      uiOutput("genreOutput"),
      uiOutput("timeOutput")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("TLDR; What are we looking at?", p("I set out to try and make a shiny app that would analyze every quantifiable part of an individual's music listening trends/tastes. For rather obvious reasons my appetite was far too large for the timeframe I had, so using an alpha version of my data wrangling package I am writing, a sample dataset was created. Using lastfm listening reports created from recording my listening history and spotify scalar attributes & genre tags of track/albums/artists I made some pretty cool basic visuals."),
                                                  br(),
                                                  p("The eventual goal is to automate taking a lastfm userID, generating genre and time scopes to select, and generate many pretty significant plots/tests telling users about their potential mood, secret genre favorites, most broadspread tastes in genre, overlapping tastes, and all sort of cool trends. Another pending feature will be comparing multiple users to quantify genre, listening time, artist, track, and album overlaps and trends. If your friends can't decide what the best roadtrip playlist for everyone would be, why not use R and massive datasets to do it!"),
                                                  br(),
                                                  p("This project was only possible because of the work done by Rcharlie on the spotifyr package used to access the spotify API!")
                                                  ),
        tabPanel("Valence x Energy", plotOutput("vxeplot"),
                 p("Spotify defines valence derived with their proprietary algorithms as the scalar 'negativity' or 'positivity' of a track. Energy is defined as 'intensity' or also as the amount of expression or volume of a track. If you use the concept of a Thayer valence/arousal chart, this would allow you to 'determine' the mood of the track or visualize the mood of a scope of genre, album, artist, etc."),
                 img(src="thayer_example.png")
                 ), 
        tabPanel("Genre Attributes Correlation", plotOutput("gcplot"),
                 p("Correlation of attributes indicates that within a genre when one of the listed attributes decreases or increases, the linked attribute is likely to changes accordingly. Positive correlation for example if 'valence' & 'energy' were linked for a genre that it is likely they both increase together. Abstracting this example with a Thayer valence/arousal chart, this means the music has a trend of becoming more happy in extremes of attributes assuming correlation remains the same.")
                 ),
        tabPanel("Time Listening Trends", plotOutput("tltplot"), verbatimTextOutput("tlttext")),
        tabPanel("Raw Dataset", dataTableOutput("rdtable"), verbatimTextOutput("rdtext"))
      )
    )
  )
)
server <- function(input, output){
###
  #Sidepanel Hyperlinks
  git_url <- a("j4yr0u93", href="https://github.com/j4yr0u93")
  output$name = renderUI({
    tagList("my github:", git_url)
  })
  rcharlie_url <- a("spotifyr", href="https://www.rcharlie.com/spotifyr/")
  output$project = renderUI({
    tagList("rcharlie:", rcharlie_url)
  })
###
  #Genre Dropdown
  output$genreOutput <- renderUI({
    selectInput("genreInput", "Genre",
                choices = `Genre Options`)
  })
###
  #
  output$timeOutput <- renderUI({
    selectInput("timeInput", "Time Scale",
                choices = c("Month", "Day of the Week", "Hour"))
  })
###
  #Intro Page
  output$summary = renderText({
    #Put text here for intro
  })
###
  #Valence x Energy tab
  output$vxeplot = renderPlot({
    vxe_data <- j4yr0u93_music_data_2019 %>%
      filter(str_detect(genres, input$genreInput))
    ggplot(vxe_data, mapping = aes(energy, valence)) +
      geom_point() +
      coord_fixed() +
      xlim(0, 1) +
      ylim(0, 1) +
      theme(text = element_text(size = 18), element_line(size = 0.6))
  })
###
  #Genre Correlation tab
  output$gcplot = renderPlot({
    correlation_clean <- j4yr0u93_music_data_2019 %>% 
      filter(str_detect(genres, input$genreInput)) %>% 
      select(-c(track, artist, album, `Day of the Week`, genres, Month, Hour))
    correlation_data <- cor(correlation_clean)
    ggcorrplot(correlation_data, hc.order = TRUE, type = "lower", lab = TRUE)
  })
###
  #Time Listening Trends
  output$tltplot = renderPlot({
    time_data <- j4yr0u93_music_data_2019 %>%
      filter(str_detect(genres, input$genreInput))
    ggplot(time_data, mapping = aes(x = time_data[[input$timeInput]])) +
      geom_bar() +
      xlab(input$timeInput) +
      theme(text = element_text(size = 18), element_line(size = 0.6))
  })
  output$tlttext = renderText({
    
  })
###
  #Raw Dataset Tab
  output$rdtable = renderDataTable({
    j4yr0u93_music_data_2019
  })
  output$rdtext = renderText({
    "Just my data from 2019 that you can search!"
  })
  
  
  
}

shinyApp(ui = ui, server = server)