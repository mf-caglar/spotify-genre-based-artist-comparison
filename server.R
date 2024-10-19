library(shiny)
library(spotifyr)
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(plotly)
library(bslib)



# Define Server
server <- function(input, output, session) {
    
    # Set up Spotify API credentials
    id <- "your-id"
    secret <- "your-secret"
    Sys.setenv(SPOTIFY_CLIENT_ID = id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
    access_token <- get_spotify_access_token()
    
    observeEvent(input$search, {
        req(input$artist)  # Ensure artist input is not null or empty
        
        # Search for the entered artist by name
        artist_data <- search_spotify(tolower(input$artist), type = "artist")
        
        if (nrow(artist_data) == 0) {
            showNotification("Artist not found. Please try a different name.", type = "error")
            return(NULL)
        }
        
        artist_id <- artist_data$id[1]  # Get the first artist's ID
        artist_genres <- artist_data$genres[[1]]  # Get the genres of the artist
        # Render the artist information inside a red box
        output$artist_info_box <- renderUI({
            tags$div(
                style = "border: 2px solid green; padding: 10px; background-color: #000000; margin-top: 10px; text-color:white",
                tags$h4(input$artist, style = "color: red;"),
                tags$p(HTML(paste("<strong>Popularity out of 100:</strong>", artist_data$popularity[1]))),
                tags$p(HTML(paste("<strong>Genres:</strong>", paste(artist_data$genres[[1]], collapse = ", ")))),
            )
        })
        if (length(artist_genres) == 0) {
            showNotification("No genres found for this artist.", type = "error")
            return(NULL)
        }
        
        # Find similar artists based on genre
        similar_artists <- list()
        artists_df <- data.frame(name = NA, popularity = NA, genres = NA)
        
        for(genre in artist_genres){
            genre_artists <- search_spotify(genre, type = "artist", limit = 5) %>%
                arrange(-popularity) %>%
                select(id, name, popularity, genres)
            artists_df <- bind_rows(artists_df, genre_artists) %>%
                distinct() %>%
                arrange(-popularity)
        }
        
        artists_df <- na.omit(artists_df)
        
        artists_df <- artists_df %>% filter(popularity > 10)
        
        if(nrow(artists_df) > 10){
            artists_df <- artists_df %>% filter(popularity > 50)
        }
        
        artists_df <- bind_rows(artist_data[1,] %>% select(id,name,popularity,genres),artists_df)
        artists_df$genres <- sapply(artists_df$genres, function(genre_list) {
            paste(genre_list, collapse = ", ")
        })
        
        artists_df <- artists_df %>% distinct()
        # Display artists in a table
        output$artist_table <- renderTable({
            artists_df %>%
                select(-id)  # Remove the id column for display
        })
        
        
        # Plot the popularity of these artists
        output$artist_popularity_plot <- renderPlotly({
            # Create the ggplot for artist popularity
            p <- ggplot(artists_df, aes(x = reorder(name, -popularity), y = popularity, fill = popularity, text = paste("Artist: ", name, "<br>Popularity: ", popularity))) +
                geom_bar(stat = "identity", color = "black", size = 0.3) +
                scale_fill_viridis(discrete = FALSE, option = "rocket") +
                theme_minimal(base_size = 14) +  # Set base font size
                labs(
                    x = "Artist<br>(hover over the bars)", 
                    y = "Popularity", 
                    title = "Popularity of Similar Artists"
                ) +
                theme(
                    axis.text.x = element_blank(),  # Hide x-axis text
                    axis.text.y = element_text(size = 12, color = "darkblue"),  # Customize y-axis text style
                    axis.title.x = element_text(size = 16, face = "bold"),  # Customize x-axis title
                    axis.title.y = element_text(size = 16, face = "bold"),  # Customize y-axis title
                    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "darkred"),  # Center and customize title
                    panel.grid.major = element_line(color = "gray90", size = 0.5),  # Light grid lines
                    panel.grid.minor = element_blank(),  # Remove minor grid lines
                    panel.background = element_rect(fill = "lightgreen"),  # Light background color
                    legend.position = "top",  # Move legend to top
                    legend.title = element_text(size = 14),  # Customize legend title size
                    legend.text = element_text(size = 12)  # Customize legend text size
                )
            
            # Convert ggplot to Plotly for interactivity
            ggplotly(p, tooltip = "text")
        })
        
        
        # Fetch the top tracks for each artist
        all_top_tracks <- lapply(artists_df$id, function(artist_id) {
            top_tracks <- get_artist_top_tracks(artist_id,market = "US")
            top_tracks$artist_name <- rep(get_artist(artist_id)$name,nrow(top_tracks))
            if(nrow(top_tracks) > 5){
                return(top_tracks[1:5,])
            }else{
                return(top_tracks)
            }
        })
        
        all_top_tracks <- bind_rows(all_top_tracks)
         
        all_top_tracks <- all_top_tracks %>% filter(popularity > 50)
        
        all_top_tracks <- all_top_tracks %>% distinct()
        
        output$track_popularity_plot <- renderPlotly({
    # Create the ggplot with hover information
        p <- ggplot(all_top_tracks, aes(x = reorder(name, -popularity), y = popularity, fill = artist_name, text = paste("Artist: ", artist_name, "<br>Song: ", name, "<br>Popularity: ", popularity))) +
        geom_bar(stat = "identity", color = "red", size = 0.3) +  # Add red border around bars
        scale_fill_viridis(discrete = TRUE, option = "rocket", name = "Artist") +  # Fancy color palette
        theme_minimal(base_size = 14) +  # Set base font size
        labs(
            x = "Track<br>(hover over the bars)", 
            y = "Popularity", 
            title = "Top 5 Tracks of Similar Artists"
        ) +
        theme(
            axis.text.x = element_blank(),  # Hide x-axis text
            axis.text.y = element_text(size = 12, color = "darkblue"),  # Customize y-axis text style
            axis.title.x = element_text(size = 16, face = "bold"),  # Customize x-axis title
            axis.title.y = element_text(size = 16, face = "bold"),  # Customize y-axis title
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "darkred"),  # Center and customize title
            panel.grid.major = element_line(color = "gray90", size = 0.5),  # Light grid lines
            panel.grid.minor = element_blank(),  # Remove minor grid lines
            panel.background = element_rect(fill = "lightgreen"),  # Light background color
            legend.position = "top",  # Move legend to top
            legend.title = element_text(size = 14),  # Customize legend title size
            legend.text = element_text(size = 12)  # Customize legend text size
        )
    
    # Convert the ggplot to a Plotly plot
        ggplotly(p,tooltip = "text")
    
        })
        
            
    })
    
}
