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

data_frame_deaths = data_frame %>%
      group_by(gameId, teamId) %>%
      summarise(visionScore = mean(visionScore), deaths = mean(deaths)) %>%
      group_by(gameId) %>%
      summarise(visionScore = diff(visionScore), deaths = first(deaths)) %>%
      mutate(visionScoreDiff = cut(visionScore,
                                   breaks = c(-Inf, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, Inf),
                                   right = TRUE)) %>%
      group_by(visionScoreDiff) %>%
      summarise(deaths = mean(deaths))
      


print(data_frame_deaths)

qplot(data_frame_deaths$visionScoreDiff, data_frame_deaths$deaths, group = 1, geom=c("point", "line"),
        xlab = "visionScoreDiff",
        ylab = "Deaths",
        main = "Deaths by vision score diff",
        )


# game time vs visionScore

plot(data_frame$gameDuration, data_frame$visionScore,
     ylab = "visionScore",
     xlab = "Game Duration",
     main = "Game Duration to VisionScore", col = alpha("gray", 0.5))
abline(reg = lm(data_frame$visionScore ~ data_frame$gameDuration), col = "red")



# game time vs deaths,

plot(data_frame$gameDuration, data_frame$deaths,
     ylab = "Deaths",
     xlab = "Game Duration",
     main = "Game Duration to deaths", col = alpha("gray", 0.5))
abline(reg = lm(data_frame$deaths ~ data_frame$gameDuration), col = "red")

