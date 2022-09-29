
# ----- Libraries ----- #
library(ggplot2)
library(dplyr)
library(mosaicData)
library(gapminder)
library(maps)

# ----- Practise 1 ----- #
"Use mpg data and produce the following graph: - see slide 11"
ggplot(data = mpg,
       mapping = aes(x = cty, y = hwy, color = "green", label = class)) +
       geom_point(alpha = 0.7, size = 3) +
       geom_text(hjust = 0, vjust = 0) +
       geom_smooth(method = "lm",
                   se = FALSE,
                   size = 1.5)

"With calculated means:"
mean_cty <- mpg %>%
                filter(!is.na(cty)) %>%
                group_by(class) %>%
                summarise(mean(cty))
mean_cty

mean_hwy <- mpg %>%
                filter(!is.na(hwy)) %>%
                group_by(class) %>%
                summarise(mean(hwy))
mean_hwy

mpg_means <- data.frame(mean_cty[2], mean_hwy[2], mean_cty[1])
mpg_means
ggplot(data = mpg_means,
       mapping = aes(x = mean.cty., y = mean.hwy., color = "blue", label = class)) +
       geom_point(alpha = 0.7, size = 3, color = "black") +
       geom_text(hjust = 0, vjust = -1) +
       geom_smooth(method = "lm",
                   se = FALSE,
                   size = 1.5)


# ----- Practise 2 ----- #
"Predict what the code below will return and then run it."
ggplot(data = diamonds) +
       geom_bar(mapping = aes(x = cut, color = cut))


# ----- Practise 3 ----- #
"Diagnose the error below and then fix the code chunk to make a plot."
ggplot(data = pressure) +
       geom_bar(mapping = aes(x = temperature, y = pressure))

"fixed to: (need to tell ggplot that we provide the y-value ourselves, and not use the default one: 'count')"
ggplot(data = pressure) +
       geom_bar(mapping = aes(x = temperature, y = pressure), stat = "identity")


# ----- Practise 4 ----- #
ggplot(Marriage, aes(x = age)) +
       geom_histogram(fill = "cornflowerblue",
                      color = "white",
                      bins = 20) +
       labs(title = "Participants by age",
            subtitle = "number of bins = 20",
            x = "Age")

"changing the bins or removing it:"
ggplot(Marriage, aes(x = age)) +
       geom_histogram(fill = "cornflowerblue",
                      color = "white",
                      bins = 50) +
       labs(title = "Participants by age",
            subtitle = "number of bins = 20",
            x = "Age")
ggplot(Marriage, aes(x = age)) +
       geom_histogram(fill = "cornflowerblue",
                      color = "white") +
       labs(title = "Participants by age",
            subtitle = "number of bins = 20",
            x = "Age")


# ----- Practise 5 ----- #
"Produce the following barchart for the diamond data set:"
ggplot(diamonds,
       aes(x = cut, fill = clarity)) +
       geom_bar(position = position_dodge(preserve = "single"))


# ----- Practise 6 ----- #
"Use the geom_line() function to make line graphs. Like geom_point(), it
requires x and y aesthetics."
"Use geom_line() to recreate the graph above.
The graph uses the economics dataset that comes with ggplot2 and maps the date and unemploy
variables to the x and y axes."
ggplot(economics,
       aes(x = date, y = unemploy)) +
       geom_line()


# ----- Practise 7 ----- #
"Use the gapminder package to assemble a new data set named asia to
plot. Among other things, asia contains the per capita GDP of four countries from 1952 to 2007.
Plot the asia data with gdpPercap as y and year as x axis. Do you get an
odd looking graph? How does it look like?"
gapminder_asia <- gapminder %>%
                    filter(continent == "Asia")
ggplot(gapminder_asia,
       aes(x = year, y = gdpPercap)) +
       geom_line()

"The graph shows that the Asian countries becomes richer and richer."
"The graph is whipsawing (can be seen if it displayed as line graph."
"The whipsawing effect can be easier to see if using the asia.xlsx file that is on Itslearning"


# ----- Practise 8 ----- #
"Redraw the graph as a scatterplot. Can you spot more than one “line” in the data?"
ggplot(gapminder_asia,
       aes(x = year, y = gdpPercap)) +
       geom_point()

"Yes, more than one 'line' can be seen."

"You can use the group aesthetic to instruct these geoms to draw separate
objects for different groups of observations.
In the code below, you can map group to the grouping variable country to
create a separate line for each country. Try it. Be sure to place the group
mapping inside of aes()."
ggplot(gapminder_asia) +
       geom_line(aes(x = year, y = gdpPercap, group = country))


# ----- Practise 9 ----- #
"To use map_data() give it the name of a dataset to retrieve. You can
retrieve a subset of the data by providing an optional region argument.
For example, I can use this code to retrieve a map of Florida from state,
which is the dataset that contains all 50 US states."
fl <- map_data("state", region = "florida")
ggplot(fl) +
    geom_polygon(mapping = aes(x = long, y = lat))

"Alter the code to retrievae and plot your home state. Notice the capitalization."
f2 <- map_data("state", region = "new york")
ggplot(f2) +
    geom_polygon(mapping = aes(x = long, y = lat))