httpgd::hgd()
httpgd::hgd_browse()

# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# loading the data
data <- read.csv("selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)

data_frame = data_frame %>%
    filter(teamPosition != "") %>%
    filter(gameDuration > 240)
    
data_frame$teamPosition = factor(data_frame$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

data_frame = data_frame %>%
    group_by(teamPosition, win) %>%
    summarise(wardsKilled = mean(wardsKilled), wardsPlaced = mean(wardsPlaced))

plot = ggplot(data_frame, aes(x = teamPosition, y = ifelse(win == "True", wardsKilled, -wardsKilled), fill=win)) +
    geom_bar(stat="identity", position="identity")+
    scale_y_continuous(limits = c(-max(data_frame$wardsKilled), max(data_frame$wardsKilled))) +
    labs(title = "Wards killed", x = "Position", y = "Wards killed") + # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15))+
    coord_flip()

plot2 = ggplot(data_frame, aes(x = teamPosition, y = ifelse(win == "True", wardsPlaced, -wardsPlaced), fill=win)) +
    geom_bar(stat="identity", position="identity")+
    scale_y_continuous(limits = c(-max(data_frame$wardsPlaced), max(data_frame$wardsPlaced))) +
    labs(title = "Wards placed", x = "Position", y = "Wards placed") + # nolint
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15))+
    coord_flip()

print(plot)
print(plot2)