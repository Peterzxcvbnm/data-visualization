# setting working directory
setwd("project")

# libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(GGally)
library(dplyr)

# loading the data
data <- read.csv("/Users/oliver/OfflineDocuments/git/Uddannelse/data-visualization/project/selected-data.csv", sep = ",")
df <- as.data.frame(data)
df <- subset(df, select = c("championName", "win", "gameDuration"))
df$win <- as.logical(df$win)
summary(df)

df_grouped = df %>% group_by(championName) %>% summarise(wins = sum(win, na.rm = TRUE), totalGames = n(), winRate = sum(win, na.rm = TRUE) / n())
df_grouped <- subset(df_grouped, select = c("championName", "winRate"))

df_grouped <- df_grouped[order(df_grouped$winRate),]

p <- ggplot(df_grouped, aes(x = reorder(championName, -winRate), y = winRate)) +
  geom_bar(stat='identity', fill = "#FF6666")

p <- ggplotly(p)
p
