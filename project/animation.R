
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

# summing up visionScore, wardsKilled, and wardsPlaced (individually) per game
#data_mutated <- data_sorted
game_id <- c()
game_duration <- c()
vision_score_total <- c()
wards_killed_total <- c()
wards_placed_total <- c()

i <- 0
new_index <- 0
for (row in 1:nrow(data_sorted))
{
  # every 10th time (per game)
  if (i %% 10 == 0)
  {
    new_index = new_index + 1
    game_id_temp <- data_sorted$gameId[row]
    game_duration_temp <- data_sorted$gameDuration[row]
    
    vision_score_total_temp <- with(data_sorted, sum(visionScore[gameId == game_id]))
    wards_killed_total_temp <- with(data_sorted, sum(wardsKilled[gameId == game_id]))
    wards_placed_total_temp <- with(data_sorted, sum(wardsPlaced[gameId == game_id]))
    
    #data_mutated <- mutate(data_mutated, visionScoreTotal = vision_score_sum)
    #data_mutated <- mutate(data_mutated, wardsKilledTotal = wards_killed_sum)
    #data_mutated <- mutate(data_mutated, wardsPlacedTotal = wards_placed_sum)
    
    game_id[new_index] <- game_id_temp
    game_duration[new_index] <- game_duration_temp
    vision_score_total[new_index] <- vision_score_total_temp
    wards_killed_total[new_index] <- wards_killed_total_temp
    wards_placed_total[new_index] <- wards_placed_total_temp
  }
  i <- i + 1
}

data_mutated <- data.frame(game_id, game_duration, vision_score_total, wards_killed_total, wards_placed_total)
data_mutated


# removing duplicates based on gameId
#data_team <- data_sorted[!duplicated(data_sorted[, c('gameId')]),]
#data_team


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