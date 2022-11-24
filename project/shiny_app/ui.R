# libraries
library(shiny)
# Define UI for application that draws a figure
ui <- fluidPage(

# Application title
  titlePanel("Data Visualization - Group 12 - League of Legends"),

# Sidebar with a slider input for number of bins
  fluidRow(
    column(6,
      h3("Role and champion selection"),
      h5("Select role and champion to find out which champion is the superior one."),
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
    ),
    
    column(6,
      h3("Animation"),
      h5("The animation plot can show any of the variables below over time. This will show differences between victories and defeats."),
      selectInput("animation_y_value",
                  "Variable:",
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
    )
  ),

  hr(),

# Show a plot of the generated distribution
  plotOutput("violin_kills_assists_diff"),

  fluidRow(
    column(6, plotOutput("vision_score_distribution")),
    column(6, plotOutput("vision_score_by_wins"))
  ),

  fluidRow(
    column(6, plotOutput("wards_placed_by_position")),
    column(6, plotOutput("wards_killed_by_position"))
  ),

  plotOutput("best_champion_pick"),
  plotOutput("top_champion_winrate"),
  plotOutput("jungle_champion_winrate"),
  plotOutput("middle_champion_winrate"),
  plotOutput("bottom_champion_winrate"),
  plotOutput("utility_champion_winrate"),

  fluidRow(
    column(4, plotOutput("vision_score_by_deaths")),
    column(4, plotOutput("gameDuration_to_visionScore")),
    column(4, plotOutput("gameDuration_to_deaths"))
  ),

  plotOutput("teamid_teamposition_win"),
  plotOutput("wards_Killed_violinPlot"),
  plotOutput("wards_Placed_violinPlot"),
  plotOutput("animation_plot")
)