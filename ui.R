#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Movie Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h2(textOutput("focal_movie_title")),
       h2(textOutput("focal_movie_release_year")),
            textOutput("movies_rated_out"),
        sliderInput("focal_movie_rating",
                   "How would you rate this movie?",
                   min = 1,
                   max = 5,
                   value = 3),
       actionButton("user_rating_action","Submit rating"),
       actionButton("dont_know_action","I don't know this movie"),
       actionButton("predict_movies", "Run prediction!"),
       h1(textOutput("all_movies_rated"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            tabsetPanel(
                    tabPanel("Prediction Results",
                             dataTableOutput("recommended_movies")),
                    tabPanel("Documentation",
                             p("This app allows you to find movies that you 
                               might want to watch based on movies you have 
                               watched in the past. Rate movies in the box on 
                               the left.If you have already seen the movie that 
                               is displayed choose a score on the slider bar 
                               and click 'Submit rating'. If you don't know the 
                               movie, click 'I don't know this movie' and you 
                               will be shown a new movie to rate. Once you 
                               rated 10 movies you can click the 'Run 
                               prediction!' button to see rating predictions 
                               for all movies in our database that you didn't 
                               rate so far. Maybe you'll find one you like!"), 
                             p("Once you clicked 'Run prediction!' you can see 
                               your results in the Prediction Results tab. It 
                               may take some seconds until the prediction 
                               calculation finishes and the results appear."),
                             p("Have fun rating!")
                             )
            )
    )
  )
))
