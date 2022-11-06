
# use the relationship between two variables over game duration (order by time, make intervals)
# visionScore, wardsKilled, wardsPlaced, gameDuration

# setting working directory
#setwd("project")

# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gganimate)

# loading the data
data <- read.csv("E:/git_projects/data-visualization/project/selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)

# sort based on gameDuration
data_sorted <- data_frame %>% arrange(gameDuration)

# 

# removing duplicates based on gameId
data_team <- data_sorted[!duplicated(data_sorted[, c('gameId')]),]
data_team

# animation
"ggplot(data_frame, aes(x = wardsPlaced, y = wardsPlaced, size = visionScore, colour = win)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  # Here comes the gganimate specific bits
  labs(title = 'Game duration [s]: {frame_time}', x = 'Wards placed', y = 'Wards killed') +
  transition_time(gameDuration) +
  ease_aes('linear')"


# sum up visionScore, wardsKilled, wardsPlaced (separately) per game
# make intervals for gameDuration
# slow down animation speed