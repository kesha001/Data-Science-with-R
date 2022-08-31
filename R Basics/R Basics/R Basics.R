# Objects
a <- 1
b <- 1
c <- -1
# print object
print(a)
# show all objects in workspace
ls()
# solving the quadratic equation
solution_1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
solution_2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)


# Data Types
library(dslabs)
data("murders")
class(murders)
# structure of an object
str(murders)
# first observations in data
head(murders)
# get names of the columns of dataframe
names(murders)
# accessing values of variables in dataframe
murders$state
# save population variables into vector "pop"
pop <- murders$population
# length of vector (numerical)
length(pop)
class(pop)
# character vector
class(murders$state)
# logical type
x <- 2 == 3
class(x)
# factor data type, stores categorical values, more memory efficient that character type
class(murders$region)
levels(murders$region)


# Question 1: What are the two solutions to 2x^2 - x - 4 = 0
a <- 2
b <- -1
c <- -4

solution_1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
solution_2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)

# Question 2: Use R to compute log base 4 of 1024. 
log(1024, 4)


library(dslabs)
data(movielens)


# Question 3a: How many rows are in the dataset?
str(movielens)
# 100004

# Question 3b: How many different variables are in the dataset?
# 7 

# Question 3c: What is the variable type of title ?
class(movielens$title)

# Question 3d: What is the variable type of genres ?
class(movielens$genres)

# Question 4: Determine how many levels are in the factor genres in the movielens data frame.
nlevels(movielens$genres)


# Vectors
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")
# named vector creation
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)
# to name elements of vector with function name()
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
# subindexing vector
# with numbers
codes[2]
codes[c(1,3)]
codes[1:2]
# with names of elements
codes["canada"]
codes[c("egypt","italy")]

# vectors coercion
x <- c(1, "canada", 3)
x
class(x)
x <- as.character(1:4)
as.numeric(x)
# trying to convert b into number gives NA - missing value
as.numeric(c("1", "b", "4", 3))

# Sorting
data(murders)
sort(murders$total)

x <- c(31, 4, 15, 92, 65)
x
sort(x)
# get indices for elements for vector to be sorted
index <- order(x)
sorted <- x[index]
sorted

# sort murders data
index <- order(murders$total)
murders$state[index]
# maximum value of murder totals 
max(murders$total)
# get index of this maximum value of totals 
i_max <- which.max(murders$total)
murders$state[i_max]
#function min and which.min for minimum values
# rank is used to rank values (smallest is 1, second smallest is 2 etc.)
rank(x)


# Vector arithmetic
murders$state[which.max(murders$population)]
max(murders$population)

#get murder rate per state
murder_rate <- murders$total/murders$population*100000
# sort states by murder rate
murders$state[order(murder_rate, decreasing = TRUE)]

# Question 3: Mandi, Amy, Nicole, and Olivia all ran different distances in different time intervals. Their distances (in
# miles) and times (in minutes) are as follows:
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_hrs <- time/60

distance/time_hrs
