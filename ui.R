library(shiny)
library(spotifyr)
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(plotly)
library(bslib)

ui <- fluidPage(
    theme = bs_theme(bootswatch = "cyborg"),
    
    # CSS to style the search box and button
    tags$head(
        tags$style(HTML("
            /* Style for the search box */
            #artist {
                background-color: #2a2a2a;
                color: #ffffff;
                border: 1px solid #4CAF50;
                padding: 10px;
                font-size: 16px;
                border-radius: 5px;
                width: 100%;
                box-sizing: border-box;
                transition: 0.3s;
            }
            #artist:focus {
                border: 1px solid #66ff66;
                box-shadow: 0 0 10px #66ff66;
            }
            
            /* Style for the search button */
            #search {
                background-color: #4CAF50;
                border: none;
                color: white;
                padding: 10px 20px;
                font-size: 16px;
                border-radius: 5px;
                cursor: pointer;
                transition: background-color 0.3s, transform 0.3s;
            }
             #search.clicked {
                background-color: #5a180a;  /* Yellow background after click */
                color: #FFFFFF
            }
            #search:hover {
                background-color: #45a049;
                transform: scale(1.05);
            }
            #search:active {
                background-color: #3e8e41;
            }
        "))
    ),
    
    # JavaScript to submit search when pressing "Enter"
    tags$script(HTML("
        $(document).on('keypress', function(e) {
            if(e.which == 13) {
                $('#search').click();  // Trigger click on Enter
            }
        });
        $(document).on('click', '#search', function() {
                $(this).addClass('clicked');
        });
    ")),
    
    titlePanel("Spotify Genre-Based Artist Comparison"),
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            textInput("artist", "Enter an Artist Name:", value = "Coldplay", 
                      placeholder = "e.g. Billie Eilish"),
            actionButton("search", "Search"),
            hr(),
            h4("Instructions"),
            p("Enter an artist's name to find other artists with similar genres, 
               and view their popularity and the popularity of their top tracks."),
            hr(),
            p(em("Note: The data displayed is fetched directly from Spotify's API. 
                  Please ensure that the artist name is spelled correctly."))
        ),
        
        mainPanel(
            htmlOutput("artist_info_box"),
            tabsetPanel(
                tabPanel("Artist Popularity", plotlyOutput("artist_popularity_plot")),
                tabPanel("Track Popularity", plotlyOutput("track_popularity_plot")),
                tabPanel("Artists Table", tableOutput("artist_table"))
            )
        )
    )
)
