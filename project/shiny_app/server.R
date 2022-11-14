server <- function(input, output) {

  output$violin_kills_assists_diff <- renderPlot({
    data = data_frame %>%
        mutate(kill_assist_diff = kills - assists)

    data$teamPosition = factor(data$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

    ggplot(data, aes(x = teamPosition, y = kill_assist_diff, fill = win)) +
        geom_violin(position = position_dodge(1)) +
        labs(title = "Kills assists difference", x = "Kills - Assists", y = "Count") + # nolint
    theme(plot.title = element_text(hjust = 0.5)) +
        geom_boxplot(width = 0.25, position = position_dodge(1))
  })

  output$vision_score_distribution <- renderPlot({
    ggplot(data, aes(x = visionScore, group = win, fill = win)) +
      geom_density(adjust = 1.5, alpha = .4)
  })

  output$wards_killed_by_position <- renderPlot({
    data = data_frame
    data$teamPosition = factor(data$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

    data = data %>%
    group_by(teamPosition, win) %>%
    summarise(wardsKilled = mean(wardsKilled), wardsPlaced = mean(wardsPlaced))

    plot = ggplot(data, aes(x = teamPosition, y = ifelse(win == "True", wardsKilled, - wardsKilled), fill = win)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_y_continuous(limits = c(-max(data$wardsKilled), max(data$wardsKilled))) +
    labs(title = "Wards killed", x = "Position", y = "Wards killed") + # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15)) +
    coord_flip()
  })

  output$wards_placed_by_position <- renderPlot({
    data = data_frame
    data$teamPosition = factor(data$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

    data = data %>%
    group_by(teamPosition, win) %>%
    summarise(wardsKilled = mean(wardsKilled), wardsPlaced = mean(wardsPlaced))

    plot2 = ggplot(data_frame, aes(x = teamPosition, y = ifelse(win == "True", wardsPlaced, - wardsPlaced), fill = win)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_y_continuous(limits = c(-max(data_frame$wardsPlaced), max(data_frame$wardsPlaced))) +
    labs(title = "Wards placed", x = "Position", y = "Wards placed") + # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15)) +
    coord_flip()
  })

  output$vision_score_by_wins = renderPlot({
    data = data_frame %>%
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
}