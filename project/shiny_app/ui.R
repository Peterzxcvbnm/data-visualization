# libraries
library(shiny)
library(shinydashboard)

introductionPage = fluidPage(titlePanel("Introduction"),
                             h5("Data Visualization (SM-DV) - Autumn 2022"),
                             h5("Group 12"),
                             h5("Kristian Østergaard - krust19@student.sdu.dk"),
                             h5("Oliver Winther - olvan18@student.sdu.dk"),
                             h5("Peter Andreas Brændgaard - pebra18@student.sdu.dk"),
                             h5("Troels Zink Kristensen - tkris17@student.sdu.dk"),
                             
                             h3("Dataset"),
                             h5("The used dataset origins from Riot Developer Portal:"), a("Riot API", href="https://developer.riotgames.com/apis#match-v5/GET_getMatch"),
                             h5("The original dataset has been reduced to contain less variables for simplicity of the project."),
                             downloadButton("download_dataset", "Download dataset"),
                             
                             h3("Report"),
                             h5("The report can be downloaded below."),
                             downloadButton("download_report", "Download report"),
                             )

visionPage = fluidPage(# App title ----
                       titlePanel("Vision"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(# Sidebar panel for inputs ----
                                     sidebarPanel(
                                       selectInput(
                                         "positions_selected",
                                         "Select positions:",
                                         c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                         multiple = TRUE,
                                         selected = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY")
                                       ),
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       # Output: Tabset w/ plot, summary, and table ----
                                       tabsetPanel(
                                         type = "tabs",
                                         tabPanel("Overall", fluidRow(
                                           column(6, plotOutput("vision_score_distribution")),
                                           column(6, plotOutput("vision_score_by_wins"))
                                         )),
                                         tabPanel("Wards", fluidRow(
                                           column(6, plotOutput("wards_placed_by_position")),
                                           column(6, plotOutput("wards_killed_by_position"))
                                         )),
                                         tabPanel("Deaths", fluidRow(
                                           column(4, plotOutput("vision_score_by_deaths")),
                                           column(4, plotOutput("gameDuration_to_visionScore")),
                                           column(4, plotOutput("gameDuration_to_deaths"))
                                         )),
                                         tabPanel("Violin", fluidPage(
                                           plotOutput("wards_Killed_violinPlot"),
                                           plotOutput("wards_Placed_violinPlot")
                                         ))
                                       )
                                       
                                     )))

championsPage = fluidPage(titlePanel("Champions"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                "champion_pick_lane",
                                "Lane:",
                                c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                multiple = FALSE,
                                selected = c("TOP")
                              ),
                              #textInput("champion_pick",
                              #          "Champion:",
                              #          "Aatrox"
                              #),
                              selectInput(
                                "champion_pick",
                                "Champion:",
                                c("Aatrox", "Ahri", "Akali", "Alistar", "Amumu", "Anivia", "Annie", "Ashe", "Aurelion Sol", "Azir",
                                  "Bard", "Blitzcrank", "Brand", "Braum",
                                  "Caitlyn", "Camille", "Cassiopeia", "Cho'Gath", "Corki",
                                  "Darius", "Diana", "Dr. Mundo", "Draven",
                                  "Ekko", "Elise", "Evelynn", "Ezreal",
                                  "Fiddlesticks", "Fiora", "Fizz",
                                  "Galio", "Gangplank", "Garen", "Gnar", "Gragas", "Graves",
                                  "Hecarim", "Heimerdinger",
                                  "Illaoi", "Irelia", "Ivern",
                                  "Janna", "Jarvan IV", "Jax", "Jayce", "Jhin", "Jinx",
                                  "Kai'Sa", "Kalista", "Karma", "Karthus", "Kassadin", "Katarina", "Kayle", "Kayn", "Kennen", "Kha'Zix", "Kindred", "Kled", "Kog'Maw",
                                  "LeBlanc", "Lee Sin", "Leona", "Lissandra", "Lucian", "Lulu", "Lux",
                                  "Malphite", "Malzahar", "Maokai", "Master Yi", "Miss Fortune", "Mordekaiser", "Morgana",
                                  "Nami", "Nasus", "Nautilus", "Nidalee", "Nocturne", "Nunu & Willump",
                                  "Olaf", "Orianna", "Ornn",
                                  "Pantheon", "Poppy", "Pyke",
                                  "Quinn",
                                  "Rakan", "Rammus", "Rek'Sai", "Renekton", "Rengar", "Riven", "Rumble", "Ryze",
                                  "Sejuani", "Shaco", "Shen", "Shyvana", "Singed", "Sion", "Sivir", "Skarner", "Sona", "Soraka", "Swain", "Sylas", "Syndra",
                                  "Tahm Kench"
                                ),
                                multiple = FALSE,
                                selected = c("TOP")
                              ),
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Best champion", plotOutput("best_champion_pick")),
                                tabPanel("Top", plotOutput("top_champion_pick")),
                                tabPanel("Jungle", plotOutput("jungle_champion_pick")),
                                tabPanel("Middle", plotOutput("middle_champion_pick")),
                                tabPanel("Bottom", plotOutput("bottom_champion_pick")),
                                tabPanel("Utility", plotOutput("utility_champion_pick"))
                              )
                            )
                          ))

animationPage = fluidPage(
  titlePanel("Animation"),
  h5(
    "The animation plot can show any of the variables below over time. This will show differences between victories and defeats."
  ),
  selectInput(
    "animation_y_value",
    "Variable:",
    c(
      "assists",
      "deaths",
      "goldEarned",
      "kills",
      "totalDamageDealt",
      "visionScore",
      "wardsKilled",
      "wardsPlaced",
      "totalMinionsKilled"
    ),
    multiple = FALSE,
    selected = c("assists")
  ),
  sliderInput(
    "animation_length_of_intervals",
    "Length of intervals [s]:",
    min = 60,
    max = 600,
    value = 120,
    step = 30
  ),
  actionButton(inputId = "submit_animation",
               label = "Submit animation properties"),
  plotOutput("animation_plot")
)

# Define UI for application that draws a figure
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Data Visualization - Group 12 - League of Legends"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info")),
      menuItem("Vision", tabName = "vision", icon = icon("eye")),
      menuItem("Champions", tabName = "champions", icon = icon("user")),
      menuItem("Animation", tabName = "animation", icon = icon("film")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", introductionPage),
    tabItem(tabName = "vision", visionPage),
    tabItem(tabName = "champions", championsPage),
    tabItem(tabName = "animation", animationPage),
    tabItem(tabName = "conclusion", h3("Conclusion"))
    )
  )
)