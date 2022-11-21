library(tidyverse)
library(ggplot2)
library(grid)
library(shadowtext)

championVariable = "Aatrox"
teamPositionVariable = "TOP"

data <- read.csv("/Users/oliver/OfflineDocuments/git/Uddannelse/data-visualization/project/selected-data.csv", sep = ",")
df <- as.data.frame(data)
df <- df[df$gameDuration >= 240,] # outliers. The ones ending in a draw
df$win <- as.logical(df$win)

df_grouped <- df %>% group_by(gameId) %>% filter(any(championName==championVariable && teamPosition == teamPositionVariable)) %>% filter(teamPosition == teamPositionVariable)
df_grouped <- df_grouped[df_grouped$championName != championVariable,]
df_grouped <- df_grouped %>% ungroup() %>% group_by(championName) %>% summarise(wins = sum(win, na.rm = TRUE), totalGames = n(), winRate = 1- (sum(win, na.rm = TRUE) / n()))
df_grouped = df_grouped[order(df_grouped$winRate), ]

graph_data <- data.frame(
  winRate = df_grouped$winRate,
  name = factor(df_grouped$championName, levels = df_grouped$championName)
)

plt <- ggplot(graph_data) +
  geom_col(aes(winRate, name), width = 0.5)
plt
  
