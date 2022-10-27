
# setting working directory
setwd("project")

# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# loading the data
data <- read.csv("selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)
data_frame[1, ]

data_frame_reduced <- filter(data_frame, championName == "Draven" |
                                         championName == "Jinx" |
                                         championName == "Kai'Sa" |
                                         championName == "Lucian" |
                                         championName == "Miss Fortune" |
                                         championName == "Sivir" |
                                         championName == "Tristana" |
                                         championName == "Twitch" |
                                         championName == "Varus" |
                                         championName == "Vayne" |
                                         championName == "Xayah" |
                                         championName == "Ziggs")

# plotting gameDuration to each gameID
plot(data_frame$gameId, data_frame$gameDuration,
        type = "l",
        xlab = "Game ID",
        ylab = "Game Duration",
        main = "Game Duration to Game ID")

# plotting number of times champions have been used
ggplot(
      data_frame_reduced,
      aes(x = championName)) +
      geom_bar(mapping = aes(championName),
                     color = "black",
                     fill = "gray") +
      xlab("ChampionName") +
      ylab("Count")