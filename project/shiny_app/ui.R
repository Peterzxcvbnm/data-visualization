# libraries
library(shiny)
library(shinydashboard)
library(plotly)

introductionPage = fluidPage(titlePanel("Introduction"),
                             h5("Data Visualization (SM-DV) - Autumn 2022"),
                             h5("Group 12"),
                             h5("Kristian Østergaard - krust19@student.sdu.dk"),
                             h5("Oliver Winther - olvan18@student.sdu.dk"),
                             h5("Peter Andreas Brændgaard - pebra18@student.sdu.dk"),
                             h5("Troels Zink Kristensen - tkris17@student.sdu.dk"),
                             
                             h3("Project Guidelines"),
                             h5("The project guidelines can be downloaded below."),
                             downloadButton("download_project_guidelines", "Download project guidelines"),
                             
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
                          
                          h5("Pick a lane (team position) and a corresponding champion."),
                          h5("OBS: Some champions have no data for specific lanes."),
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                "champion_pick_lane",
                                "Lane / Team position:",
                                c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                multiple = FALSE,
                                selected = c("TOP")
                              ),
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
                              
                              tableOutput("table_champions"),
                              
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Best champion", plotlyOutput("best_champion_pick", width = "600px", height = "750px")),
                                tabPanel("Top", plotlyOutput("top_champion_pick", height = "1000px", width = "600px")),
                                tabPanel("Jungle", plotlyOutput("jungle_champion_pick", height = "1000px", width = "600px")),
                                tabPanel("Middle", plotlyOutput("middle_champion_pick", height = "1000px", width = "600px")),
                                tabPanel("Bottom", plotlyOutput("bottom_champion_pick", height = "1000px", width = "600px")),
                                tabPanel("Utility", plotlyOutput("utility_champion_pick", height = "1000px", width = "600px"))
                              )
                            )
                          )
                        )

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
      "totalMinionsKilled",
      "visionScore",
      "wardsKilled",
      "wardsPlaced"
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
  actionButton(inputId = "submit_animation", label = "Generate animation"),
  br(),
  plotOutput("animation_plot")
)

conclusionPage = fluidPage(
  titlePanel("Conclusion"),
  
  h5("League of Legends (LoL) is an online competitive game with a massive player base."),
  h5("With its competitive nature and high stakes in both amateur and professional tournaments, a competitive edge is crucial."),
  h5("This edge can be gained by utilizing data visualization."),
  h5("Providing players with a quick overview of the most important factors contributing to a victory."),
  h5("The data was collected using Riot's API, and with it an investigation was launched into answering the following question:"),
  br(),
  
  h4("Which variables are most important to examine in order to maximize the probability to win a game in League of Legends?"),
  br(),
  h5("15 out of hundreds of variables were chosen and examined."),
  h5("These were things such as 'championName', 'deaths', 'wards killed', etc."),
  h5("From them, graphs of different types were created."),
  h5("One density plot, two line plots, two scatter plots, two violin plots, two box plots, six bar charts and one animation plot were created to properly investigate and visualize the data."),
  h5("It was done in the programming language R."),
  
  h5("From these graphs, a few different results were found. Vision score had quite a significant effect."),
  h5("The bigger the difference in overall vision scores, the larger chance for a victory up towards 80% win rate."),
  h5("Another important variable that was found was the champion pick against a specific opponent."),
  h5("If you are able to pick a champion that directly counters the opponent you have a significant win rate boost."),
  h5("An interesting observation came when looking into the variable 'deaths'. Visualizing it gave some unpredictable results."),
  h5("This was because long-lasting games in general had more deaths, and so, the data visualization would be more focused on long-lasting games vs. short games."),
  h5("Therefore, it had nothing to do with deaths."),
  h5("Here it would have been necessary to divide the games up into intervals to make sure games of equal lengths were compared."),
  
  h5("Furthermore, a different type of graph was made. An animation plot which plots nine of the 15 variables individually."),
  h5("When plotting the variable 'minions killed', it was expected to be very important for the win rate, but it proved only to be the case in early game."),
  h5("As soon as the game reached late game, it evened out and did not matter as much."),
  h5("Other variables such as deaths, kills etc. were also of great significance, as one would expect.")
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
    tabItem(tabName = "conclusion", conclusionPage)
    )
  )
)