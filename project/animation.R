
# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gganimate)

# loading the data
data <- read.csv("E:/git_projects/data-visualization/project/selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)

# remove irrelevant data points
data_filtered <-  data_frame %>%
                  filter(gameDuration > 240) %>%
                  group_by(gameId, teamId) %>%
                  summarise(gameDuration = gameDuration[1],
                            assists = sum(assists),
                            deaths = sum(deaths),
                            goldEarned = sum(goldEarned),
                            kills = sum(kills),
                            totalDamageDealt = sum(totalDamageDealt),
                            visionScore = sum(visionScore),
                            wardsKilled = sum(wardsKilled),
                            wardsPlaced = sum(wardsPlaced),
                            win = win[1],
                            totalMinionsKilled = sum(totalMinionsKilled))

data_filtered <- data_filtered %>% arrange(gameDuration)

# making intervals
length_of_intervals <- 120 # user defined
data_intervals <- data_filtered %>% mutate(intervalIndex = 1)

for (row in 1:nrow(data_filtered))
{
  x = floor((data_filtered[row, 3] - min(data_filtered$gameDuration)) / length_of_intervals) + 1
  data_intervals[row, ncol(data_intervals)] = x * length_of_intervals / 60
}

data_intervals <- data_intervals %>% group_by(intervalIndex, win) %>% summarise(assists = mean(assists),
                                                                                 deaths = mean(deaths),
                                                                                 goldEarned = mean(goldEarned),
                                                                                 kills = mean(kills),
                                                                                 totalDamageDealt = mean(totalDamageDealt),
                                                                                 visionScore = mean(visionScore),
                                                                                 wardsKilled = mean(wardsKilled),
                                                                                 wardsPlaced = mean(wardsPlaced),
                                                                                 totalMinionsKilled = mean(totalMinionsKilled))
data_intervals

last_value <- data_intervals[nrow(data_intervals), 1]
test <- "assists"

# The animation
# https://stackoverflow.com/questions/53092216/any-way-to-pause-at-specific-frames-time-points-with-transition-reveal-in-gganim/53093389
plot <- ggplot(data_intervals, aes(x = intervalIndex, y = !!sym(test), group = win, color = win)) + 
  geom_line() + 
  geom_segment(aes(xend = as.integer(last_value), yend = !!sym(test)), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = as.integer(last_value), label = win), hjust = 0) + 
  transition_reveal(intervalIndex) +
  coord_cartesian(clip = 'off') + 
  labs(title = paste(test, ' through time - victory vs. defeat'), x = 'Game duration [min.]', y = test) + 
  theme_minimal() +
  view_follow()

animate(plot, duration = 15, fps = 5, end_pause = 25)