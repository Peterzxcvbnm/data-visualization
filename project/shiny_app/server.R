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

make_lane_graph <- function(df, color, title) {
  df_grouped = df %>% group_by(championName) %>% filter(n() > 9) %>% summarise(wins = sum(win, na.rm = TRUE), totalGames = n(), winRate = sum(win, na.rm = TRUE) / n())
  df_grouped = df_grouped[order(df_grouped$winRate), ]
  
  data <- data.frame(
    winRate = df_grouped$winRate,
    name = factor(df_grouped$championName, levels = df_grouped$championName)
  )
  
  x_breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  
  ggplot(data) +
            geom_col(aes(winRate, name), fill = color, width = 0.5, position = position_dodge(0.7)) +
            labs(title = title, x = "WIN RATE", y = "CHAMPIONS") +
            theme(axis.title = element_text(size = 20), axis.text = element_text(size = 14)) + 
            scale_x_continuous(breaks=x_breaks, labels=x_breaks,limits=c(0.0,1.0))
  }

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
    
    ggplot(graph_data) +
      geom_col(aes(winRate, name), width = 0.5) +
      labs(title = paste(championVariable, teamPositionVariable, "Win Rate", sep = " "), x = "WIN RATE", y = "CHAMPIONS") +
      theme(axis.title = element_text(size = 20))
  })
  
  output$top_champion_winrate = renderPlot({
    df <- as.data.frame(data)
    df$win <- as.logical(df$win)
    topDf <- df[df$teamPosition == "TOP",]
    make_lane_graph(topDf, "#1338BE", "TOP WIN RATE")
  })
  
  output$jungle_champion_winrate = renderPlot({
    df <- as.data.frame(data)
    df$win <- as.logical(df$win)
    jungleDf <- df[df$teamPosition == "JUNGLE",]
    make_lane_graph(jungleDf, "#048243", "JUNGLE WIN RATE")
  })
  
  output$middle_champion_winrate = renderPlot({
    df <- as.data.frame(data)
    df$win <- as.logical(df$win)
    middleDf <- df[df$teamPosition == "MIDDLE",]
    make_lane_graph(middleDf, "#C21807", "MIDDLE WIN RATE")
  })
  
  output$bottom_champion_winrate = renderPlot({
    df <- as.data.frame(data)
    df$win <- as.logical(df$win)
    bottomDf <- df[df$teamPosition == "BOTTOM",]
    make_lane_graph(bottomDf, "#5a3d46", "BOTTOM WIN RATE")
  })
  
  output$utility_champion_winrate = renderPlot({
    df <- as.data.frame(data)
    df$win <- as.logical(df$win)
    supportDf <- df[df$teamPosition == "UTILITY",]
    make_lane_graph(supportDf, "#FF8300", "SUPPORT WIN RATE")
  })
}