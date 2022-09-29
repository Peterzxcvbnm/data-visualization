
# ----- Practise 1 ----- #
"Compute the difference between 2022 and the year you
started at this university and divide this by the difference
between 2022 and the year you were born. Multiply this
with 100 to get the percentage of your life you have spent
at this university. Use brackets if you need them."
(2022 - 2017) / (2022 - 1997) * 100


# ----- Practise 2 ----- #
"Repeat Practice1, but with several steps in between. You
can give the variables any name you want, but the name
has to start with a letter."
diff_uni <- 2022 - 2017
diff_birth <- 2022 - 1997
percentage <- 100
diff_uni / diff_birth * percentage


# ----- Practise 3 ----- #
"Compute the sum of 4, 5, 8 and 11 by first combining them
into a vector and then using the function sum."
vec <- c(4, 5, 8, 11)
sum(vec)


# ----- Practise 4 ----- #
"Generate 1000 samples from the N(mean=10, sd=1.4)"
result_04 = rnorm(n = 1000, mean = 10, s = 1.4)


# ----- Practise 5 ----- #
"Plot what you generated in Practice 4."
plot(result_04)


# ----- Practise 6 ----- #
"Find help for the sqrt function."
help(sqrt)


# ----- Practise 7 ----- #
"Make a file called firstscript.R containing Rcode that
generates 100 random numbers and plots them, and run
this script several times (you can also use
source('firstscript.R') in the console command)."
#setwd("E:/OneDrive - Syddansk Universitet/Universitet SDU/Software Engineering 3. semester (MSc)/Data Visualization/R-exercices/lecture-exercises")
#source("lecture04-firstscript.R")


# ----- Practise 8 ----- #
"Put the numbers 31 to 60 in a vector named P and in a
matrix with 6 rows and 5 columns named Q. Tip: use the
function seq. Look at the different ways scalars, vectors
and matrices are denoted in the workspace window."
P <- c(seq(31, 60))
P
Q <- matrix(P, nrow = 6, ncol = 5)
Q


# ----- Practise 9 ----- #
"Make a script file which constructs three random normal
vectors of length 100. Call these vectors x1, x2 and x3.
Make a data frame called t with three columns (called a, b
and c) containing respectively x1, x1+x2 and x1+x2+x3.
Call the following functions for this data frame: plot(t)
and sd(t). Can you understand the results? Rerun this
script a few times."
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
t <- data.frame(a = x1, b = x1 + x2, c = x1 + x2 + x3)
plot(t)
#sd(t)


# ----- Practise 10 ----- #
"Add the following lines to the script file of the Practice9.
Try to find out, either by experimenting or by using the
help, what the meaning is of rgb, the last argument of
rgb, lwd, pch, cex."
plot(t$a, type = "l", ylim = range(t), lwd = 3, col = rgb(1, 0, 0, 0.3))
lines(t$b, type = "s", lwd = 2, col = rgb(0.3, 0.4, 0.3, 0.9))
points(t$c, pch = 20, cex = 4, col = rgb(0, 0, 1, 0.3))


# ----- Practise 11 ----- #
"Make a graph with on the x-axis: today, Christmas 2022
and your next birthday and on the y-axis the number of
presents you expect on each of these days. Tip: make two
vectors first."
x_vec <- c(strptime("20220924", format = "%Y%m%d"),
           strptime("20221224", format = "%Y%m%d"),
           strptime("20230430", format = "%Y%m%d"))
y_vec <- c(0, 6, 6)
plot(x_vec, y_vec, type = "l")


# ----- Practise 12 ----- #
"Import data from a comma delimited file: 'salaries.csv'"
read.csv("../datasets/SalariesDataset/Salaries.csv", sep = ",")


# ----- Practise 13 ----- #
"Import data from a tab delimited file: 'salaries.txt'"
read.csv("../datasets/SalariesDataset/Salaries.txt", sep = "\t")


# ----- Practise 14 ----- #
"Import data from an Excel workbook: 'salaries.xlsx'"
library(readxl)
read_excel("../datasets/SalariesDataset/Salaries.xlsx")


# ----- Practise 15 ----- #
"From the starwars data set:"
"Keep only the variables: name, height, and gender"
library(dplyr)
select(starwars, name)
select(starwars, height)
select(starwars, gender)


# ----- Practise 16 ----- #
"Keep the variables name and all variables between mass
and species inclusive"
select(starwars, name, mass : species)


# ----- Practise 17 ----- #
"Keep all variables except birth_year and gender"
select(starwars, !birth_year & !gender)
# select(starwars, -birth_year, -gender)


# ----- Practise 18 ----- #
"Select females"
filter(starwars, gender == "feminine")


# ----- Practise 19 ----- #
"Select females that are from Alderaan"
starwars %>%
    filter(gender == "feminine") %>% 
    filter(homeworld == "Alderaan")


# ----- Practise 20 ----- #
"Select individuals that are from Alderaan, Coruscant, or Endor"
starwars %>% 
    filter(homeworld == "Alderaan" |
           homeworld == "Coruscant" |
           homeworld == "Endor")


# ----- Practise 21 ----- #
"Convert height in centimeters to inches, and mass in
kilograms to pounds"
starwars %>%
    mutate(height = height / 2.54) %>%
    mutate(mass = mass * 2.2)


# ----- Practise 22 ----- #
"If height is greater than 180 then heightcat = 'tall',
otherwise heightcat = 'short'"
starwars %>%
    mutate(heightcat = ifelse(height > 180, "tall", "short"))


# ----- Practise 23 ----- #
"Convert any eye color that is not black, blue or brown, to other"
starwars2 <- starwars %>%
    mutate(eye_color = ifelse(eye_color != "black" &
                              eye_color != "blue" &
                              eye_color != "brown",
                              "other", eye_color))


# ----- Practise 24 ----- #
"Set heights greater than 200 or less than 75 to missing"
starwars %>%
    mutate(height = ifelse(height > 200 | height < 75, "missing", height))


# ----- Practise 25 ----- #
"Calculate mean height and mass"
means <- starwars %>%
                filter(!is.na(height) & !is.na(mass)) %>%
                summarise(mean(height), mean(mass))
means


# ----- Practise 26 ----- #
"Calculate mean height and weight by gender"
means <- starwars %>%
                filter(!is.na(height) & !is.na(mass)) %>%
                group_by(gender) %>%
                summarise(mean(height), mean(mass))
means


# ----- Practise 27 ----- #
"Calculate the mean height for women by species using the
pipe %>% operator"
means <- starwars %>%
                filter(!is.na(height) & gender == "feminine") %>%
                group_by(species) %>%
                summarise(mean(height))
means


# ----- Practise 28 ----- #
"Produce the following dataset and name it 'Wide_data' - see slides"
id <- c("01", "02", "03")
name <- c("Bill", "Bob", "Mary")
sex <- c("Male", "Male", "Female")
age <- c(22, 25, 18)
income <- c(55000, 75000, 90000)
wide_data <- data.frame(id, name, sex, age, income)
wide_data

"and convert it to a long dataset like: - see slides"
long_data <- wide_data %>%
    gather("variable", "value", -id, -name)


# ----- Practise 29 ----- #
"converting back again:"
long_data %>%
    spread("variable", "value")