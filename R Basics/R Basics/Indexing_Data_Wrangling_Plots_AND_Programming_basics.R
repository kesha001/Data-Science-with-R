library(dslabs)
data("murders")

murder_rate <- murders$total / murders$population * 100000

# Get indexing of elements with murder rate less that 0.71
index <- murder_rate <= 0.71
# Get names of these states
murders$state[index]

# We want to get states with murder rate <= 1 and to be on the West part of the country
safe <- murder_rate <= 1
west <- murders$region == "West"
murders$state[safe & west]


x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
# Get indices of true elements
which(x)   

index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state



# Basic Data Wrangling
library(dplyr)
murders
# Add murder rate to murders data table
murders <- mutate(murders, rate = total/population*100000)
murders
# Filter rows in data frame to exclude states with higher that 0.71 murder rate
filter(murders, rate <= 0.71)
# Selecting state region and rate from original data
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)
# Putting together fuctions with pipe
murders %>% 
  select(state, region, rate) %>% 
  filter(rate <= 0.71)

# Creating Data Frames
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"),
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 85, 85, 90))
grades

class(grades$names)

murders <- murders %>%
  mutate(rate = total / population * 100000)

# Basic plots
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

# Histogram
hist(murders$rate)
murders$state[which.max(murders$rate)]

# Boxplots
boxplot(rate ~ region, data = murders)



# Summarizing with dplyr
library(tidyverse)
library(dslabs)
data(murders)

murders <- murders %>% 
  mutate(rate = total / population * 10^5)

# Get summarizing statistics for rates
s <- murders %>% 
  filter(region == "West") %>% 
  summarize(
    minimum = min(rate),
    median = median(rate),
    maximum = max(rate),
  )
s
s$median

# Compute rate for all states
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population)*10^5)

us_murder_rate

# Get the same statistics with quantile function
murders %>% 
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))

# Create function to get this statistics in one row
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

# dplyr pull() fucntion to get values from dataframe
us_murder_rate %>% pull(rate)

us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate

# accessing dataframe with pipe
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate

# Grouping data
# the fucntion group_by creates special dataframe with witch dplyr functions will act differently
murders %>% 
  group_by(region) %>% 
  summarize(mean = mean(rate))

# arrange() for sorting entire data frame
murders %>% 
  arrange(region, desc(population)) %>% 
  head(10)

murders %>% 
  top_n(10, rate)


#
# data.table
# install.packages("data.table")
library(data.table)

# convert data frame to data table
murders <- setDT(murders)

# selecting in dplyr
select(murders, state, region)

# selecting with data table
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# memory efficient approach uses update by reference in data table
murders[, rate := total / population * 10^5]

# define multiple columns
murders[, ":="(rate = total / population * 100000, rank = rank(population))]

# y is referring to x and := changes by reference
x <- data.table(a = 1)
y <- x

x[, a := 2]
y


y[,a := 1]
x

# make full copy 
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y


# subsetting in dplyr
filter(murders, rate <= 0.7)

# subsetting in data.table
murders[rate <= 0.7]

# combining filter and select in data.table
murders[rate <= 0.7, .(state, rate)]

# combining filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state, rate)

data(heights)
heights <- setDT(heights)

# summarizing in dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))

s <- heights[, .(average = mean(height), standard_deviation = sd(height))]


# subsetting and then summarizing in dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]

# previously defined function
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}
# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]


# sorting in data.table

data(murders)
murders <- setDT(murders)
murders[, rate := total / population * 100000]

# order by population
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] 

# order by region and then murder rate
murders[order(region, rate)]


# Questions
library(dslabs)
data(heights)
options(digits = 3)
heights
# Question 1
# How many individuals in the dataset are above average height? and How many individuals in the dataset are above average height and are female?
length(which(heights$height > mean(heights$height) & heights$sex == "Female"))

# Question 3 
# What proportion of individuals in the dataset are female?
length(which(heights$sex == "Female")) / length(heights$sex)

# Question 4a
# Determine the minimum height in the heights dataset.
# Question 4b
# Use the match() function to determine the index of the first individual with the minimum height.
# Question 4c
# Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
which.max(heights$height)
heights[which.min(heights$height), ]
min(heights$height)
heights[match(min(heights$height), heights$height),]

# Question 5a
# Determine the maximum height.
max(heights$height)
# Question 5b
# Which integer values are between the maximum and minimum heights?
x <- 50:82
# Question 5c
# How many of the integers in x are NOT heights in the dataset?
sum(!(x %in% heights$height))


heights2 <- heights %>% 
  mutate(ht_cm = height*2.54)
# Question 6a
# What is the height in centimeters of the 18th individual (index 18)?
heights2$ht_cm[18]
# Question 6b
# What is the mean height in centimeters?
mean(heights2$ht_cm)

# Question 7a
# How many females are in the heights2 dataset?
females <- heights2 %>% 
  filter(sex == "Female")
length(females$sex)
nrow(females)

# Question 7b
# What is the mean height of the females in centimeters?
mean(females$ht_cm)

# Question 8
# The olive dataset in dslabs contains composition in percentage of eight fatty acids found in the lipid fraction of 572 Italian olive oils:
library(dslabs)
data(olive)
head(olive)
# Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. What relationship do you see?
ggplot(olive, aes(palmitic, palmitoleic)) +
  geom_point()

# Question 9
# Create a histogram of the percentage of eicosenoic acid in olive.
olive %>% 
  ggplot(aes(eicosenoic)) +
  geom_histogram()

# Question 10
# Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.
# Which region has the highest median palmitic acid percentage?
# Which region has the most variable palmitic acid percentage?
olive %>% 
  ggplot(aes(region, palmitic)) +
  geom_boxplot()
  

# Basic Conditionals
a <- 2

if (a != 0){
  print(1/a)
} else {
  print("NO")
}

library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population * 100000
ind <- which.min(murder_rate)

if (murder_rate[ind] < 0.5) {
  print(murders$state[ind])
} else {
  print("no")
}

a <- 0
ifelse(a > 0, 1/a, NA)

a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
result


data(na_example)
sum(is.na(na_example))

no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))

z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)


# Basic functions

avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
x <- 1:100
avg(x)
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

#
# FOR LOOOPS

S
