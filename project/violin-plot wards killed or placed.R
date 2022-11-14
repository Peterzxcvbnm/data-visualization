# setting working directory
#setwd("project")

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
    filter(teamPosition != "")
data_frame$teamPosition = factor(data_frame$teamPosition, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"))

plot = ggplot(data_frame, aes(x = teamPosition, y = wardsKilled, fill=win)) +
    geom_violin(position = position_dodge(1)) +
    labs(title = "Wards killed", x = "Wards killed", y = "Count") + # nolint
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_boxplot(width=0.25, position = position_dodge(1))

plot2 = ggplot(data_frame, aes(x = teamPosition, y = wardsPlaced, fill=win)) +
    geom_violin(position = position_dodge(1)) +
    labs(title = "Wards placed", x = "Wards placed", y = "Count") + # nolint
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_boxplot(width=0.25, position = position_dodge(1))

print(plot)
print(plot2)