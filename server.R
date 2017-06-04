#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        # ----------------------------------------------------------------------
        ### Initially disable the predict movies buttong
        shinyjs::disable("predict_movies")
        
        # ----------------------------------------------------------------------
        ### Load movies titles list if it doesn't exist
        if(!exists("movie_titles")) {
                load("movie_titles.RData")
        }
        
        # ----------------------------------------------------------------------
        ### Create prediction ongoing measurement variable
        if(!exists("prediction_ongoing")) {
                prediction_ongoing <- 0
        }
        
        # ----------------------------------------------------------------------
        ### Define reactive values
        # Initialize reactive value list
        values <- reactiveValues()
        
        # Movies rated variable
        values$movies_rated <- 0
        
        # User ratings data table
        values$user_ratings <- data.table(MovieID = integer(), Rating = integer())
        
        # Don't know integer
        values$dont_know <- integer()
        
        
        # Focal movie
        while(isolate({!exists("focal_movie") || focal_movie$MovieID %in% values$dont_know || focal_movie$MovieID %in% values$user_ratings$MovieID})) {
                        focal_movie <- movie_titles[sample(.N, 1)]
                }
                
        values$focal_movie_title <- focal_movie$MovieName
        values$focal_movie_release_year <- focal_movie$DateYear
        values$focal_movie_ID <- focal_movie$MovieID
        
        # ----------------------------------------------------------------------
        ### Rendering output
        output$movies_rated_out <- renderText({values$movies_rated})
        output$focal_movie_title <- renderText({values$focal_movie_title})
        output$focal_movie_release_year <- renderText({values$focal_movie_release_year})
        output$all_movies_rated <- renderText({values$all_rated})
        output$recommended_movies <- renderDataTable({values$predicted_scores})
        
        # ----------------------------------------------------------------------
        ### User Rating Event
        observeEvent(input$user_rating_action, {
                if (nrow(values$user_ratings) + length(values$dont_know) + 1 < nrow(movie_titles)) {
                        # bind the MovieID and rating to the user_ratings table
                        values$user_ratings <- rbind(values$user_ratings, isolate({data.table(MovieID = values$focal_movie_ID, Rating = input$focal_movie_rating)}))
                
                        # get a new random movie to rate
                        while(isolate({focal_movie$MovieID %in% values$dont_know || focal_movie$MovieID %in% values$user_ratings$MovieID})) {
                                focal_movie <- movie_titles[sample(.N, 1)]
                        }
                
                values$focal_movie_title <- focal_movie$MovieName
                values$focal_movie_release_year <- focal_movie$DateYear
                values$focal_movie_ID <- focal_movie$MovieID
                
                # increment the movies_rated counter
                values$movies_rated <- values$movies_rated + 1
                
                # if the icrement is greater than 10, allow user to run 
                # prediction
                if (values$movies_rated >= 10 & prediction_ongoing == 0) {
                        shinyjs::enable("predict_movies")
                } 
                } else {
                        values$all_rated <- "You have rated all movies in our database."
                        shinyjs::disable("user_rating_action")
                        shinyjs::disable("dont_know_action")
                        values$focal_movie_title <- " "
                        values$focal_movie_release_year <- " "
                }
                
        })
        
        # ----------------------------------------------------------------------
        ### User "Don't Know" event
        observeEvent(input$dont_know_action, { 
                if (nrow(values$user_ratings) + length(values$dont_know) + 1 < nrow(movie_titles)) {
                        # add the MovieID to the "dont_know" vector
                        values$dont_know <- c(values$dont_know, values$focal_movie_ID)
                        
                        # get a new random movie to rate
                        while(isolate({focal_movie$MovieID %in% values$dont_know || focal_movie$MovieID %in% values$user_ratings$MovieID})) {
                                focal_movie <- movie_titles[sample(.N, 1)]
                        }
                        
                        values$focal_movie_title <- focal_movie$MovieName
                        values$focal_movie_release_year <- focal_movie$DateYear
                        values$focal_movie_ID <- focal_movie$MovieID
                } else {
                        values$all_rated <- "You have rated all movies in our database."
                        shinyjs::disable("user_rating_action")
                        shinyjs::disable("dont_know_action")
                        values$focal_movie_title <- " "
                        values$focal_movie_release_year <- " "
                }
        })

        # ----------------------------------------------------------------------
        ### Prediction Event
        observeEvent(input$predict_movies, { 
                # --------------------------------------------------------------
                ### Temporarily disabling predict movies action button
                shinyjs::disable("predict_movies")
                prediction_ongoing <- 1
                
                # --------------------------------------------------------------
                # Temporaryly storing user_ratings as data frame
                user_ratings <- values$user_ratings
                
                # --------------------------------------------------------------
                ### MeanUserRatingVsGlobalAverage
                load("movie_data.RData")
                load("average_rating.RData")
                
                movie_data$MeanUserRatingVsGlobalAverage <- mean(user_ratings$Rating) - average_rating

                # --------------------------------------------------------------
                ### UserRatingsCount
                movie_data$UserRatingsCount <- nrow(user_ratings)
                
                # --------------------------------------------------------------
                ### SimilarityRating
                # Check: There are too many columns in this table (UserID, Rating, Date, MeanUserRatingVsGlobalAverage, SimilarityRating...)
                load("movie_similarities.RData")
                setkey(movie_similarities, MovieID_1)
                
                movie_data[, SimilarityRating := 0]
                
                user_ratings_calc <- user_ratings
                setkey(user_ratings_calc, MovieID)
                
                iterations <- nrow(movie_data)
                for (i in 1:iterations) {
                        movie <- movie_data[i,MovieID]
                        focal_movie_similarities <- movie_similarities[.(movie)]
                        setkey(focal_movie_similarities, MovieID_2)

                        # merge similarity into user_ratings_calc
                        user_ratings_calc[focal_movie_similarities, Similarity := Similarity]

                        # saving similarity rating result
                        movie_data[i, SimilarityRating := user_ratings_calc[!is.na(Similarity)][, .(Relative_Similarity = abs(Similarity)/sum(abs(Similarity)), Adjusted_Rating = ifelse(Similarity > 0, Rating, 6 - Rating))][, sum(Relative_Similarity * Adjusted_Rating)]]

                        # deleting similarity column
                        user_ratings_calc[,Similarity := NULL]
                }; rm(iterations, user_ratings_calc, movie, 
                      focal_movie_similarities, movie_similarities)

                # setting data to average if no similarity rating is possible
                movie_data[SimilarityRating == 0, SimilarityRating := average_rating]
                
                # --------------------------------------------------------------
                ### User_Concept
                load("user_svd_table.RData")
                svd_match <- match(as.integer(rownames(user_svd_table)), user_ratings$MovieID)
                svd_vector <- user_ratings$Rating[svd_match]
                svd_vector[is.na(svd_vector)] <- 0
                user_concept_vector <- as.vector(svd_vector %*% user_svd_table)
                user_concept_matrix <- t(matrix(rep(user_concept_vector, nrow(movie_data)), nrow = length(user_concept_vector) , ncol = nrow(movie_data)))
                colnames(user_concept_matrix) <- paste0("User_Concept_",1:ncol(user_concept_matrix))
                movie_data <- cbind(movie_data, as.data.table(user_concept_matrix))

                # cleaning up
                rm(svd_match, svd_vector, user_concept_vector,
                   user_concept_matrix, user_svd_table)

                # --------------------------------------------------------------
                # Prediction
                # remove videos already voted on
                setkey(movie_data, NULL)
                movie_data <- movie_data[!(MovieID %in% user_ratings$MovieID)]
                
                # Predict user movie scores
                load("prediction_model.RData")
                predicted_scores <- data.table(MovieID = movie_data$MovieID, Prediction = predict(prediction_model, movie_data))
                rm(prediction_model, movie_data)
                
                # Add movie meta data
                setkey(movie_titles, MovieID)
                setkey(predicted_scores, MovieID)
                predicted_scores <- predicted_scores[movie_titles, nomatch = 0]
                setkey(movie_titles, NULL)

                # Sort by score, use number of votes as tie breaker
                predicted_scores <- predicted_scores[order(-Prediction, -MovieRatingsCount)]
                
                # Get top 10 scores
                # if (nrow(predicted_scores > 10)) {
                #         predicted_scores <- predicted_scores[1:10]
                # }
                
                # delete ID and number of rating columns
                predicted_scores[, c("MovieID", "MovieRatingsCount") := NULL]
                
                # renaming columns
                colnames(predicted_scores) <- c("Predicted_Movie_Score", "Movie_Release_Year", "Movie_Name")

                # pass as output
                values$predicted_scores <- predicted_scores
                
                # cleaning up
                rm(predicted_scores, user_ratings)
                
                # --------------------------------------------------------------
                ### Re-enabling predict movies action button
                shinyjs::enable("predict_movies")
                prediction_ongoing <- 0
        })
})
