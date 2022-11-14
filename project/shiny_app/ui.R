# Define UI for application that draws a figure
ui <- fluidPage(

  # Application title
  titlePanel(""),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("five_point_ideology",
                  "Select Five Point Ideology (1=Very liberal, 5=Very conservative):",
                  min = 1,
                  max = 5,
                  value = 3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("violin_kills_assists_diff")
    )
  )
)