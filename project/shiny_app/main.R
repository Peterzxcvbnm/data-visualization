# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

# loading the data
data <- read.csv("project/selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)


source("project/shiny_app/ui.R")
source("project/shiny_app/server.R")

# Run the application 
shinyApp(ui, server)