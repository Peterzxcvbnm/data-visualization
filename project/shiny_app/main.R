# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
setwd("../..")



source("project/shiny_app/ui.R")
source("project/shiny_app/server.R")

# Run the application 
shinyApp(ui, server)