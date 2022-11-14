httpgd::hgd()
httpgd::hgd_browse()

# libraries
library(sm)
library(dplyr)
library(ggplot2)
library(tidyverse)

# loading the data
data <- read.csv("selected-data.csv", sep = ",")
data_frame <- as.data.frame(data)

data_frame = data_frame %>%
    filter(teamPosition != "") %>%
    filter(gameDuration > 240)

#plot = hist((data_frame %>% filter(win == "True"))$visionScore, main="Vision score victory")
#plot2 = hist((data_frame %>% filter(win != "True"))$visionScore, main="Vision score defeat")
plot <- ggplot(data_frame, aes(x=visionScore, group=win, fill=win)) +
    geom_density(adjust=1.5, alpha=.4)

#plot = plot(density((data_frame %>% filter(win == "True"))$visionScore), main="Vision score victory") +
#plot2 = plot(density((data_frame %>% filter(win != "True"))$visionScore), main="Vision score defeat")

print(plot)
#print(plot2)