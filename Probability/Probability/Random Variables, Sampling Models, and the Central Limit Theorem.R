# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)


# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


#
# Sampling Models

# Monte Carlo simulation: Chance of casino losing money on roulette
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]



# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")



#
# Questions 1 and 2: SAT testing
#

# Question 1a
# What is the probability of guessing correctly for one question?
p_guess <- 1/5

# Question 1b
# What is the expected value of points for guessing on one question?

0.2*1 - 0.25*0.8

# Question 1c
# What is the expected score of guessing on all 44 questions?
ES <- 0*44 # expected score

# Question 1d
# What is the standard error of guessing on all 44 questions?
SE <- sqrt(44) * abs(-0.25-1)*sqrt(0.2*0.8) # standart error

# Question 1e
# Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1- pnorm(8, ES, SE)

# Question 1f
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  sum(sample(c(1, -0.25), size=44, replace=TRUE, prob=c(p, 1-p)))
})
mean(S>8)

# Question 2a
# What is the expected value of the score when guessing on this new test?
ES <- (1*(1/4) + 0*(3/4))*44

# Question 2b
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
set.seed(21, sample.kind = "Rounding")

prob_win <- 1/4
p <- seq(0.25, 0.95, 0.05)

ES <- (1*(p) + 0*(1-p))*44
SE <- sqrt(44) * abs(1-0) * sqrt(p*(1-p))

probs <- 1- pnorm(35, ES, SE)
min(p[which(probs > 0.8)])



#
# Question 3: Betting on Roulette
#

# Question 3a
# What is the expected value of the payout for one bet?
prob_win <- 5/38
win_pay <- 6
lose_pay <- -1
ev_1 <- prob_win * win_pay + (1 - prob_win) * lose_pay
ev_1

# Question 3b
# What is the standard error of the payout for one bet?
se_1 <- abs((win_pay - lose_pay)) * sqrt(prob_win * (1-prob_win))
se_1

# Question 3c
# What is the expected value of the average payout over 500 bets?
ev_avg <- ev_1

# Question 3d
# What is the standard error of the average payout over 500 bets?
se_avg <- se_1 / sqrt(500)

# Question 3e
# What is the expected value of the sum of 500 bets?
ev_sum_500 <- ev_avg * 500

# Question 3f
# What is the standard error of the sum of 500 bets?
se_sum_500 <- se_1 * sqrt(500)

# Question 3g
# Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, .
pnorm(0, ev_sum_500, se_sum_500)
