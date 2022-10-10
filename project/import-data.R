
# setting working directory
setwd("project")

# libraries
library(rjson)
library(dplyr)

# loading the data
json_file <- "data.json"
json_data <- fromJSON(file = json_file)
json_data_frame <- as.data.frame(json_data$SummonerIds$kills)

json_data_frame[1:10,]