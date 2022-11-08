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


plot = ggplot(data_frame, aes(x = win, y = wardsKilled)) +
    geom_violin(position = position_dodge(1)) +
    labs(title = "Kills assists difference", x = "Kills - Assists", y = "Count") + # nolint
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_boxplot(width=0.25, position = position_dodge(1))

print(plot)