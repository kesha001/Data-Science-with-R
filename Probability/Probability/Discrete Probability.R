#
# Introduction to Discrete Probability
#


# Monte Carlo Simulations
#

beads <- rep( c("red", "blue"), times = c(2, 3))
beads

sample(beads, 1)
sample(beads, 3)
sample(beads, 5)

sample(beads, 3, replace = TRUE)
sample(beads, 5, replace = TRUE)
sample(beads, 6, replace = TRUE)

B <- 10000
events <- replicate(B, sample(beads, 1))
tab <- table(events)
tab

prop.table(tab)


3/(3+5+7) * (5+7)/(3+5+7)

#
# Combinations and Permutations
#

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))


suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)
deck

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5

# generates all possible 7 digit numbers without repeats
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]
all_phone_numbers


permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

# Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]


sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# Probability of a natural 21 in blackjack
# All aces
aces <- paste("Ace", suits)
# all faces
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

# order does not matter in combinations 1 and 2 the same as 2 and 1
hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)


# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# Monte Carlo simulation of natural 21 in blackjack
# code for one hand of blackjack
hand <- sample(deck, 2)
hand
# code for B=10,000 hands of 
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)



#
# The Birthday Problem

n <- 50
bdays <- sample(1:365, n, replace = TRUE)
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


#
# sapply

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

# Element-wise operation over vectors and sapply

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)


# Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

outcomes <- c(0, 1)
sample(outcomes, 6, replace = TRUE)


# 
# Assessment: Discrete Probability

library(gtools)
library(tidyverse)

# Question 1: Olympic running

# How many different ways can the 3 medals be distributed across 8 runners?
nrow(permutations(8, 3))

# How many different ways can the three medals be distributed among the 3 runners from Jamaica?
nrow(permutations(3, 3))

# What is the probability that all 3 medals are won by Jamaica?
3/8 * 2/8 * 1/8

# Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  sum(winners == "Jamaica") == 3
})
mean(results)

#
# Question 2: Restaurant management

# How many meal combinations are possible with the current menu?
6 * nrow(combinations(6, 2)) * 2

# How many combinations are possible if he expands his original special to 3 drink options?
6 * nrow(combinations(6, 2)) * 3

#  How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
6 * nrow(combinations(6, 3)) * 3

# What is the minimum number of entree options required in order to generate more than 365 combinations?
meal_comb <- function(entr_ch = 6){
  nrow(combinations(entr_ch, 1)) * nrow(combinations(6, 2)) * 3
}

meal_comb()

combos <- sapply(1:12, meal_comb)
data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

# What is the minimum number of side options required in order to generate more than 365 combinations?
meal_comb <- function(side_ch = 6){
  6 * nrow(combinations(side_ch, 2)) * 3
}

meal_comb()

combos <- sapply(2:12, meal_comb)
data.frame(sides = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$sides)




# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1

head(esoph)


# How many groups are in the study?
length(levels(esoph$agegp)) *
length(levels(esoph$tobgp)) *
length(levels(esoph$alcgp))

nrow(esoph)

# How many cases are there?
all_cases <- sum(esoph$ncases)

# How many controls are there?
all_controls <- sum(esoph$ncontrols)

# What is the probability that a subject in the highest alcohol consumption group is a cancer case?

x <- sum(esoph[esoph$alcgp == "120+", ]$ncases)/(sum(esoph[esoph$alcgp == "120+", ]$ncases)+sum(esoph[esoph$alcgp == "120+", ]$ncontrols))
signif(x,3)

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>% 
  filter(alcgp == "0-39g/day") %>% 
  summarise(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>% 
  mutate(p_case = ncases/(ncases + ncontrols))

signif(0.06987952, 3)

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
sum(esoph[esoph$tobgp != "0-9g/day", ]$ncases) / all_cases

tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

# Given that a person is a control, what is the probability that they smoke 10g or more a day?
tob_controls <- esoph %>% 
  filter(tobgp != "0-9g/day") %>% 
  pull(ncontrols) %>% 
  sum()

tob_controls/all_controls

# Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2

# For cases, what is the probability of being in the highest alcohol group?
in_h_gr_alc <- esoph %>% 
  filter(alcgp == "120+") %>% 
  summarise(ngroup = sum(ncases)) %>% 
  pull(ngroup)

in_h_gr_alc / (all_cases )


# For cases, what is the probability of being in the highest tobacco group?

in_h_gr_tob <- esoph %>% 
  filter(tobgp == "30+") %>% 
  summarise(ngroup = sum(ncases)) %>% 
  pull(ngroup)

in_h_gr_tob / (all_cases )


# For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?

in_h_gr_tob_alc <- esoph %>% 
  filter(tobgp == "30+" & alcgp == "120+") %>% 
  summarise(ngroup = sum(ncases)) %>% 
  pull(ngroup)

in_h_gr_tob_alc / (all_cases )


# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
in_h_gr_tob_or_alc <- esoph %>% 
  filter(tobgp == "30+" | alcgp == "120+") %>% 
  summarise(ngroup = sum(ncases)) %>% 
  pull(ngroup)

in_h_gr_tob_or_alc / (all_cases )


# For controls, what is the probability of being in the highest alcohol group?
high_alc_gp_ctrls <- esoph %>% 
  filter(alcgp == "120+") %>% 
  summarise(ngroup = sum(ncontrols)) %>% 
  pull(ngroup)

signif(high_alc_gp_ctrls / (all_controls ), 3)


# How many times more likely are cases than controls to be in the highest alcohol group?
signif((in_h_gr_alc / (all_cases )) / (high_alc_gp_ctrls / all_controls), 3)


# For controls, what is the probability of being in the highest tobacco group?
high_tob_gp_ctrls <- esoph %>% 
  filter(tobgp == "30+") %>% 
  summarise(ngroup = sum(ncontrols)) %>% 
  pull(ngroup)

signif(high_tob_gp_ctrls / all_controls, 3)


# For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
high_alc_tob_gp_ctrls <- esoph %>% 
  filter(tobgp == "30+" & alcgp == "120+") %>% 
  summarise(ngroup = sum(ncontrols)) %>% 
  pull(ngroup)

signif(high_alc_tob_gp_ctrls / all_controls, 3)

# For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
high_alc_or_tob_gp_ctrls <- esoph %>% 
  filter(tobgp == "30+" | alcgp == "120+") %>% 
  summarise(ngroup = sum(ncontrols)) %>% 
  pull(ngroup)

signif(high_alc_or_tob_gp_ctrls / all_controls, 3)

# How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?


signif((in_h_gr_tob_or_alc/all_cases) / (high_alc_or_tob_gp_ctrls/all_controls), 3)









