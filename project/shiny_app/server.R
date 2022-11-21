# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(shadowtext)
library(tidyverse)
library(gganimate)

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


    qplot(data$visionScoreDiff, data$winrate, group = 1, geom=c("point", "line"),
        xlab = "visionScoreDiff",
        ylab = "Winrate",
        main = "Winrate by vision score diff")
  })
  
  output$vision_score_by_deaths = renderPlot({
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
    
    
    qplot(data_frame_deaths$visionScoreDiff, data_frame_deaths$deaths, group = 1, geom=c("point", "line"),
          xlab = "visionScoreDiff",
          ylab = "Deaths",
          main = "Deaths by vision score diff",
    )
  })
  output$gameDuration_to_visionScore = renderPlot({
    plot(data_frame$gameDuration, data_frame$visionScore,
         ylab = "visionScore",
         xlab = "Game Duration",
         main = "Game Duration to VisionScore",
         col.lab = "darkgreen", col.main = "darkgreen",
         col.axis = "darkgreen")
    abline(reg = lm(data_frame$visionScore ~ data_frame$gameDuration), col = "blue")
  })
  output$gameDuration_to_deaths = renderPlot({
    plot(data_frame$gameDuration, data_frame$deaths,
         ylab = "Deaths",
         xlab = "Game Duration",
         main = "Game Duration to deaths",
         col.lab = "darkgreen", col.main = "darkgreen",
         col.axis = "darkgreen")
    abline(reg = lm(data_frame$deaths ~ data_frame$gameDuration), col = "blue")
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
  
  output$teamid_teamposition_win = renderPlot({
    selected_data <- data_frame %>% select(teamId, win)
    wins <- selected_data %>% filter(win == "True")
    ggplot(wins, aes(x = win)) +
      geom_bar(aes(fill = teamId), position = "dodge") +
      geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 1), vjust = -1) +
      theme_bw()
  }) 
  
  output$wards_Killed_violinPlot = renderPlot({
    data_frame = data_frame %>%
      filter(teamPosition != "")
    data_frame$teamPosition = factor(data_frame$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))
    ggplot(data_frame, aes(x = teamPosition, y = wardsKilled, fill=win)) +
      geom_violin(position = position_dodge(1)) +
      labs(title = "Wards killed", x = "Wards killed", y = "Count") + # nolint
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_boxplot(width=0.25, position = position_dodge(1))
  }) 
  
  output$wards_Placed_violinPlot = renderPlot({
    data_frame = data_frame %>%
      filter(teamPosition != "")
    data_frame$teamPosition = factor(data_frame$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))
    ggplot(data_frame, aes(x = teamPosition, y = wardsPlaced, fill=win)) +
      geom_violin(position = position_dodge(1)) +
      labs(title = "Wards placed", x = "Wards placed", y = "Count") + # nolint
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_boxplot(width=0.25, position = position_dodge(1))
  })

  output$animation_plot = renderImage({
    data_animation <-  data_frame %>%
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

    data_animation <- data_animation %>% arrange(gameDuration)

    # making intervals
    data_animation <- data_animation %>% mutate(intervalIndex = 1)

    for (row in 1:nrow(data_animation))
    {
      x = floor((data_animation[row, 3] - min(data_animation$gameDuration)) / input$animation_length_of_intervals) + 1
      data_animation[row, ncol(data_animation)] = x * input$animation_length_of_intervals / 60
    }

    data_animation <- data_animation %>% group_by(intervalIndex, win) %>% summarise(assists = mean(assists),
                                                                                    deaths = mean(deaths),
                                                                                    goldEarned = mean(goldEarned),
                                                                                    kills = mean(kills),
                                                                                    totalDamageDealt = mean(totalDamageDealt),
                                                                                    visionScore = mean(visionScore),
                                                                                    wardsKilled = mean(wardsKilled),
                                                                                    wardsPlaced = mean(wardsPlaced),
                                                                                    totalMinionsKilled = mean(totalMinionsKilled))

    last_value <- data_animation[nrow(data_animation), 1]

    # The animation
    # https://stackoverflow.com/questions/53092216/any-way-to-pause-at-specific-frames-time-points-with-transition-reveal-in-gganim/53093389
    plot <- ggplot(data_animation, aes(x = intervalIndex, y = !!sym(input$animation_y_value), group = win, color = win)) + 
      geom_line() + 
      geom_segment(aes(xend = as.integer(last_value), yend = !!sym(input$animation_y_value)), linetype = 2, colour = 'grey') + 
      geom_point(size = 2) + 
      geom_text(aes(x = as.integer(last_value), label = win), hjust = 0) + 
      transition_reveal(intervalIndex) +
      coord_cartesian(clip = 'off') + 
      labs(title = paste(input$animation_y_value, ' through time - victory vs. defeat'), x = 'Game duration [min.]', y = input$animation_y_value) + 
      theme_minimal() +
      view_follow()

    outfile <- tempfile(fileext = '.gif')
    anim_save("outfile.gif", animate(plot, duration = 15, fps = 5, end_pause = 25))
      # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
  }, deleteFile = TRUE)
}