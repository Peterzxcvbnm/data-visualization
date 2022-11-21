# libraries
library(shiny)
# Define UI for application that draws a figure
ui <- fluidPage(

# Application title
  titlePanel(""),

# Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("positions_selected",
                  "Select positions:",
                  c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                  multiple = TRUE,
                  selected = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY")),
      selectInput("champion_pick_lane",
                  "Lane:",
                  c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                  multiple = FALSE,
                  selected = c("TOP")),
      textInput("champion_pick",
                "Champion:",
                "Aatrox")
    ),

# Show a plot of the generated distribution
    mainPanel(
      plotOutput("violin_kills_assists_diff"),
      plotOutput("vision_score_distribution"),
      plotOutput("wards_placed_by_position"),
      plotOutput("wards_killed_by_position"),
      plotOutput("vision_score_by_wins"),
      plotOutput("best_champion_pick"),
      plotOutput("top_champion_winrate"),
      plotOutput("jungle_champion_winrate"),
      plotOutput("middle_champion_winrate"),
      plotOutput("bottom_champion_winrate"),
      plotOutput("utility_champion_winrate")
    )
  )
)