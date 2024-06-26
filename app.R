## Shiny app for NHL dashboard


# Load Libraries ----------------------------------------------------------
library(shiny)
library(tidyverse)
library(data.table)

# Load in Data ------------------------------------------------------------

mp_df <- read_csv("./Data/moneypuck_team_data.csv") |>
  filter(season == 2023)

# Loads 2023 data
download.file("https://moneypuck.com/moneypuck/playerData/seasonSummary/2023/regular/teams.csv", "./Data/test.csv")


# Create Team Names -------------------------------------------------------

# NOTE: Need to address teams that have relocated
team_names <- mp_df |>
  rename("Team" = "team") |>
  mutate(Team = case_when(
    Team == "VGK" ~ "Vegas Golden Knights",
    Team == "NYR" ~ "New York Rangers",
    Team == "BOS" ~ "Boston Bruins",
    Team == "COL" ~ "Colorado Avalanche",
    Team == "VAN" ~ "Vancouver Canucks",
    Team == "LAK" ~ "Los Angeles Kings",
    Team == "FLA" ~ "Florida Panthers",
    Team == "DET" ~ "Detroit Red Wings",
    Team == "DAL" ~ "Dallas Stars",
    Team == "WPG" ~ "Winnipeg Jets",
    Team == "CAR" ~ "Carolina Hurricanes",
    Team == "TBL" ~ "Tampa Bay Lightning",
    Team == "PHI" ~ "Philadelphia Flyers",
    Team == "ARI" ~ "Arizona Coyotes",
    Team == "TOR" ~ "Toronto Maple Leafs",
    Team == "STL" ~ "St. Louis Blues",
    Team == "NYI" ~ "New York Islanders",
    Team == "NSH" ~ "Nashville Predators",
    Team == "WSH" ~ "Washington Capitals",
    Team == "NJD" ~ "New Jersey Devils",
    Team == "PIT" ~ "Pittsburgh Penguins",
    Team == "MTL" ~ "Montreal Canadiens",
    Team == "CGY" ~ "Calgary Flames",
    Team == "BUF" ~ "Buffalo Sabres",
    Team == "MIN" ~ "Minnesota Wild",
    Team == "SEA" ~ "Seattle Kraken",
    Team == "EDM" ~ "Edmonton Oilers",
    Team == "CBJ" ~ "Columbus Blue Jackets",
    Team == "OTT" ~ "Ottawa Senators",
    Team == "ANA" ~ "Anaheim Ducks",
    Team == "SJS" ~ "San Jose Sharks",
    Team == "CHI" ~ "Chicago Blackhawks"
  )) |>
  distinct(Team) |>
  arrange(Team)




# Define User Interface ---------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NHL Dashboard"),
    
    selectInput("team",
                "Select Team",
                team_names),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
