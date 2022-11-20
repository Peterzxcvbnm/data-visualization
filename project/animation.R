
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
team_id <- c()
vision_score_total <- c()
wards_killed_total <- c()
wards_placed_total <- c()
win <- c()

i <- 0
new_index <- 0
for (row in 1:nrow(data_sorted))
{
  # every 10th time (per game)
  if (i %% 5 == 0)
  {
    new_index = new_index + 1
    game_id_temp <- data_sorted$gameId[row]
    game_duration_temp <- data_sorted$gameDuration[row]
    team_id_temp <- data_sorted$teamId[row]
    win_temp <- data_sorted$win[row]
    
    vision_score_total_temp <- with(data_sorted, sum(visionScore[gameId == game_id & teamId == team_id]))
    wards_killed_total_temp <- with(data_sorted, sum(wardsKilled[gameId == game_id & teamId == team_id]))
    wards_placed_total_temp <- with(data_sorted, sum(wardsPlaced[gameId == game_id & teamId == team_id]))
    
    game_id[new_index] <- game_id_temp
    game_duration[new_index] <- game_duration_temp
    team_id[new_index] <- team_id_temp
    vision_score_total[new_index] <- vision_score_total_temp
    wards_killed_total[new_index] <- wards_killed_total_temp
    wards_placed_total[new_index] <- wards_placed_total_temp
    win[new_index] <- win_temp
  }
  i <- i + 1
}

data_mutated <- data.frame(game_id, game_duration, team_id,
                           vision_score_total, wards_killed_total, wards_placed_total,
                           win)
#data_mutated


# remove irrelevant data points
data_filtered <- data_mutated %>% filter(game_duration > 240)
#data_filtered
#data_filtered2 <- data_filtered[order(nrow(data_filtered):1), ]
#data_filtered2


# making intervals
length_of_intervals <- 60
data_intervals <- data_filtered %>% mutate(interval_index = 
                                  floor((data_filtered$game_duration - min(data_filtered$game_duration)) / length_of_intervals) + 1)
data_intervals


# removing duplicates based on gameId
#data_team <- data_sorted[!duplicated(data_sorted[, c('gameId')]),]
#data_team


# animation
"plot <- ggplot(data_intervals, aes(x = wards_killed_total, y = wards_placed_total, size = vision_score_total, colour = win)) +
#plot <- ggplot(data_intervals, aes(x = game_duration, y = vision_score_total, colour = win)) +
         geom_point(alpha = 0.7, show.legend = TRUE) +
          scale_size(range = c(1, 10)) +
          # Here comes the gganimate specific bits
          labs(title = 'Game duration [s]: {frame_time}',
               x = 'Wards killed', y = 'Wards placed') +
          transition_time(as.integer(interval_index)) +
          ease_aes('linear')"
#plot

#{frame_time * data_intervals[1]$game_duration}-{frame_time * data_intervals[nrows(data_intervals)]$game_duration}

last_value <- data_intervals[nrow(data_intervals), 2]

# https://stackoverflow.com/questions/53092216/any-way-to-pause-at-specific-frames-time-points-with-transition-reveal-in-gganim/53093389
plot <- ggplot(data_intervals, aes(x = game_duration, y = vision_score_total, group = win, color = win)) + 
  geom_line() + 
  geom_segment(aes(xend = last_value, yend = vision_score_total), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = last_value, label = win), hjust = 0) + 
  transition_reveal(game_duration) +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Team vision score through time - victory vs. defeat', x = 'Game duration [s]', y = 'Team vision score') + 
  theme_minimal()
  #theme(plot.margin = margin(5.5, 40, 5.5, 5.5))    

#plot
animate(plot, duration = 60, fps = 10)


# sum up visionScore, wardsKilled, wardsPlaced (separately) per game
# make intervals for gameDuration
# slow down animation speed