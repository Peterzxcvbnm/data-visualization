library(shiny)
library(tidyverse)

#####Import Data

#type in your data path
dat <- read_csv("DataExerciseShinyApps.csv")
dat <- dat %>% select(c("pid7", "ideo5"))

#remove missing values 
dat <- drop_na(dat)

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
      plotOutput("ideology_plot")
    )
  )
)
  
server <- function(input, output) {
  
  output$ideology_plot <- renderPlot({
    
    ggplot(
      filter(dat, ideo5 == input$five_point_ideology), 
      aes(x = factor(pid7))) +
      geom_histogram(mapping = aes(pid7), bins = 7, color = "black", fill = "gray") +
      scale_x_continuous(breaks = dat$pid7) +
      xlab("7 Point Party ID, 1 = very D, 7 = very R") +
      ylab("Count")
  })
}

# Run the application 
shinyApp(ui, server)