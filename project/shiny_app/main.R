# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)


source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui, server)