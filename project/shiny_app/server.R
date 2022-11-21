# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(shadowtext)
library(tidyverse)

# loading the data
data <- read.csv("project/selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)
data_frame = data_frame %>%
    filter(teamPosition != "") %>%
    filter(gameDuration > 240)
data_frame$teamPosition = factor(data_frame$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

server <- function(input, output) {
  output$violin_kills_assists_diff <- renderPlot({
    data = data_frame %>%
      mutate(kill_assist_diff = kills - assists) %>%
      filter(teamPosition %in% input$positions_selected)

    data$teamPosition = factor(data$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

    ggplot(data, aes(x = teamPosition, y = kill_assist_diff, fill=win)) +
        geom_violin(position = position_dodge(1)) +
        labs(title = "Kills assists difference", x = "Kills - Assists", y = "Count") + # nolint
    theme(plot.title = element_text(hjust = 0.5)) +
        geom_boxplot(width = 0.25, position = position_dodge(1))
  })

  output$vision_score_distribution <- renderPlot({
    data = data_frame %>%
      filter(teamPosition %in% input$positions_selected)

    ggplot(data, aes(x = visionScore, group = win, fill = win)) +
      geom_density(adjust = 1.5, alpha = .4)
  })

  output$wards_placed_by_position <- renderPlot({
    data = data_frame %>%
      filter(teamPosition %in% input$positions_selected) %>%
      group_by(teamPosition, win) %>%
      summarise(wardsKilled = mean(wardsKilled), wardsPlaced = mean(wardsPlaced))

    ggplot(data, aes(x = teamPosition, y = ifelse(win == "True", wardsPlaced, - wardsPlaced), fill = win)) +
      geom_bar(stat = "identity", position = "identity") +
      scale_y_continuous(limits = c(-max(data$wardsPlaced), max(data$wardsPlaced))) +
      labs(title = "Wards placed", x = "Position", y = "Wards placed") + # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15)) +
      coord_flip()
  })

  output$wards_killed_by_position <- renderPlot({
    data = data_frame %>%
      filter(teamPosition %in% input$positions_selected) %>%
      group_by(teamPosition, win) %>%
      summarise(wardsKilled = mean(wardsKilled), wardsPlaced = mean(wardsPlaced))

    ggplot(data, aes(x = teamPosition, y = ifelse(win == "True", wardsKilled, - wardsKilled), fill = win)) +
      geom_bar(stat = "identity", position = "identity") +
      scale_y_continuous(limits = c(-max(data$wardsKilled), max(data$wardsKilled))) +
      labs(title = "Wards killed", x = "Position", y = "Wards killed") + # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15)) +
      coord_flip()
  })

  output$vision_score_by_wins = renderPlot({
    data = data_frame %>%
      filter(teamPosition %in% input$positions_selected) %>%
      group_by(gameId, teamId) %>%
      summarise(visionScore = mean(visionScore), win = first(win)) %>%
      group_by(gameId) %>%
      summarise(visionScore = diff(visionScore), win = first(win)) %>%
      mutate(visionScoreDiff = cut(visionScore,
      breaks = c(-Inf, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, Inf),
      right = TRUE)) %>%
      group_by(visionScoreDiff) %>%
      summarise(wins = sum(win == "True"), loses = sum(win == "False"), winrate = wins / (wins + loses))


    plot.default(data$visionScoreDiff, data$winrate,
        type = "o",
        xlab = "visionScoreDiff",
        ylab = "Winrate",
        main = "Winrate by vision score diff")
  })
  
  output$best_champion_pick = renderPlot({
    championVariable = input$champion_pick
    teamPositionVariable = input$champion_pick_lane
  
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
  })
}