# setting working directory
setwd("project")

httpgd::hgd()
httpgd::hgd_browse()

# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# loading the data
data <- read.csv("selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)
data_frame[1, ]

data_frame = data_frame %>%
      group_by(gameId, teamId) %>%
      summarise(visionScore = mean(visionScore), win = first(win)) %>%
      group_by(gameId) %>%
      summarise(visionScore = diff(visionScore), win = first(win)) %>%
      mutate(visionScoreDiff = cut(visionScore,
      breaks = c(-Inf, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, Inf),
      right = TRUE)) %>%
      group_by(visionScoreDiff) %>%
      summarise(wins = sum(win == "True"), loses = sum(win == "False"), winrate = wins / (wins + loses))
      


print(data_frame)

plot.default(data_frame$visionScoreDiff, data_frame$winrate,
        type = "o",
        xlab = "visionScoreDiff",
        ylab = "Winrate",
        main = "Winrate by vision score diff")

# plotting number of times champions have been used