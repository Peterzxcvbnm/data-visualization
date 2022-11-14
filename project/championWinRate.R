# setting working directory
setwd("project")
install.packages("shadowtext")
# libraries
library(grid)
library(tidyverse)
library(shadowtext)


# loading the data
data <- read.csv("/Users/oliver/OfflineDocuments/git/Uddannelse/data-visualization/project/selected-data.csv", sep = ",")
df <- as.data.frame(data)
df <- subset(df, select = c("championName", "win", "gameDuration", "teamPosition"))
df <- df[df$gameDuration >= 240,] # outliers. The ones ending in a draw

df$win <- as.logical(df$win)
topDf <- df[df$teamPosition == "TOP",]
jungleDf <- df[df$teamPosition == "JUNGLE",]
middleDf <- df[df$teamPosition == "MIDDLE",]
bottomDf <- df[df$teamPosition == "BOTTOM",]
supportDf <- df[df$teamPosition == "UTILITY",]

make_lane_graph <- function(df, color) {
  df_grouped = df %>% group_by(championName) %>% filter(n() > 9) %>% summarise(wins = sum(win, na.rm = TRUE), totalGames = n(), winRate = sum(win, na.rm = TRUE) / n())
  df_grouped = df_grouped[order(df_grouped$winRate), ]
  
  data <- data.frame(
    winRate = df_grouped$winRate,
    name = factor(df_grouped$championName, levels = df_grouped$championName)
  )
  
  plt <- ggplotly(ggplot(data) +
    geom_col(aes(winRate, name), fill = color, width = 0.5))
  
  plt
}

make_lane_graph(topDf, "#1338BE")
make_lane_graph(jungleDf, "#048243")
make_lane_graph(middleDf, "#C21807")
make_lane_graph(bottomDf, "#5a3d46")
make_lane_graph(supportDf, "#FF8300")
