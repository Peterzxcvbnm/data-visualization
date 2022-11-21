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
                "Aatrox"),
      selectInput("animation_y_value",
                  "Stat:",
                  c("assists", "deaths", "goldEarned", "kills", "totalDamageDealt", "visionScore", "wardsKilled", "wardsPlaced", "totalMinionsKilled"),
                  multiple = FALSE,
                  selected = c("assists")),
      sliderInput("animation_length_of_intervals",
                  "Length of intervals [s]:",
                  min = 60,
                  max = 600,
                  value = 120,
                  step = 30),
       actionButton(
        inputId = "submit_animation",
        label = "Submit animation properties"
      )
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
      plotOutput("utility_champion_winrate"),
      plotOutput("vision_score_by_deaths"),
      plotOutput("gameDuration_to_visionScore"),
      plotOutput("gameDuration_to_deaths"),
      plotOutput("teamid_teamposition_win"),
      plotOutput("wards_Killed_violinPlot"),
      plotOutput("wards_Placed_violinPlot"),
      plotOutput("animation_plot")
    )
  )
)