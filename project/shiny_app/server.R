server <- function(input, output) {
  
  output$violin_kills_assists_diff <- renderPlot({
    data = data_frame %>%
        filter(teamPosition != "") %>%
        mutate(kill_assist_diff = kills - assists)

    data$teamPosition = factor(data$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

    ggplot(data, aes(x = teamPosition, y = kill_assist_diff, fill=win)) +
        geom_violin(position = position_dodge(1)) +
        labs(title = "Kills assists difference", x = "Kills - Assists", y = "Count") + # nolint
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_boxplot(width=0.25, position = position_dodge(1))
  })
}