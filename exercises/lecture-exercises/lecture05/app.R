#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("My First Shiny App"),
    textInput(inputId ="text", label = "Type in here"),
    textOutput(outputId = "print_text")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$print_text <- renderText(input$text)
}

# Run the application 
shinyApp(ui = ui, server = server)
