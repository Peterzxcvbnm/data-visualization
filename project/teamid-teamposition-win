
# setting working directory
setwd("project")

# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# loading the data
data <- read.csv("selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)

# selecting data
selected_data <- data_frame %>% select(teamId, win)
#selected_data
wins <- selected_data %>% filter(win == "True")
#wins

# teamId vs. win
# blue team vs. red team
ggplot(wins, aes(x = win)) +
    geom_bar(aes(fill = teamId), position = "dodge") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 1), vjust = -1) +
    theme_bw()