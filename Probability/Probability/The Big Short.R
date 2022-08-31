#
#The Big Short: Interest Rates Explained
#

# Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

# Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error



x = - loss_per_foreclosure*p/(1-p)
x

# n a $180,000 loan, this equals an interest rate of:
x/180000

# Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

#
# The Big Short

# Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

# Monte Carlo simulation with unknown default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million


#
# Assessment: The Big Short

library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

# Questions 1 and 2: Insurance rates, part 1

# Question 1a
# Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>% 
  filter(age==50 & sex=="Female") %>% 
  pull(prob)
p

# Question 1b
# What is the expected value of the company's net profit on one policy for a 50 year old female?
loss <- -150000
gain <- 1150
mu <- p*loss + (1-p)*gain
mu

# Question 1c
# Calculate the standard error of the profit on one policy for a 50 year old female.
sigma <- abs(loss - gain)*sqrt(p*(1-p))
sigma

# Question 1d
# What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
n <- 1000
exp_val <- n * mu
exp_val

# Question 1e
# What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
st_err <- sqrt(n) * sigma
st_err

# Question 1f
# Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0, exp_val, st_err)

# Question 2a
# Use death_prob to determine the probability of death within one year for a 50 year old male.
p_male <- death_prob %>% 
  filter(age==50 & sex=="Male") %>% 
  pull(prob)
p_male

# Question 2b
# What premium should be charged?
loss <- -150000
n <- 1000
gain_male <- (700000 - (n * loss * p_male))/ (n*(1-p_male))

# Question 2c
# Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
st_err_male <- sqrt(n) * abs(loss - gain_male) * sqrt(p_male*(1-p_male))

# Question 2d
# What is the probability of losing money on a series of 1,000 policies to 50 year old males?
exp_val_male <- 700000
pnorm(0, exp_val_male, st_err_male)


#
# Questions 3 and 4: insurance rates, part 2

# Question 3a
# What is the expected value of the company's profits over 1,000 policies?
n <- 1000
p <- 0.015
loss <- -150000
gain <- 1150
exp_val <- n * (p * loss + (1-p)*gain)

# Question 3b
# What is the standard error of the expected value of the company's profits over 1,000 policies?
st_err <- sqrt(n) * abs(loss-gain) * sqrt(p * (1-p))
st_err

# Question 3c
# What is the probability of the company losing money?
pnorm(0, exp_val, st_err)

# Question 3d
# What is the probability of losing more than $1 million?
pnorm(-10^6, exp_val, st_err)

# Question 3e
# What is the lowest death probability for which the chance of losing money exceeds 90%?
calc_prob <- function(p){
  exp_val <- n * (p * loss + (1-p)*gain)
  st_err <- sqrt(n) * abs(loss-gain) * sqrt(p * (1-p))
  pnorm(0, exp_val, st_err)
}
p <- seq(.01, .03, .001)
p[min(which(sapply(p, calc_prob) > 0.9))]


# Question 3f
# What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
p <- seq(.01, .03, .0025)
calc_prob_lose_1m <- function(p){
  exp_val <- n * (p * loss + (1-p)*gain)
  st_err <- sqrt(n) * abs(loss-gain) * sqrt(p * (1-p))
  pnorm(-10^6, exp_val, st_err)
}
p[min(which(sapply(p, calc_prob_lose_1m) > 0.9))]

# Question 4a
# What is the reported profit (or loss) in millions (that is, divided by 10^6)?
p_loss = .015
loss <- -150000
profit <- 1150
n <- 1000
set.seed(25, sample.kind = "Rounding")
sum(sample(c(loss, profit), size=n, replace=TRUE, prob=c(p_loss, 1-p_loss))) / 10^6

# Question 4b
# What is the observed probability of losing $1 million or more?
B <- 10000
set.seed(27, sample.kind = "Rounding")
S <- replicate(B, {
  sum(sample(c(loss, profit), size=n, replace=TRUE, prob=c(p_loss, 1-p_loss))) 
})
mean(S < -10^6)

#
# Questions 5 and 6: Insurance rates, part 3

# Question 5a
# Calculate the premium required for a 5% chance of losing money given n = 1000 loans, probability of death p = 0.015, and loss per claim l = -150000. Save this premium as x for use in further questions.
n <- 1000
p <- 0.015
l <- -150000
z <- qnorm(0.05) 
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# Question 5b
# What is the expected profit per policy at this rate?
exp_prft <- p*l + (1-p)*x
exp_prft

# Question 5c
# What is the expected profit over 1,000 policies?
exp_prft * n

# Question 5d
B <- 10000
set.seed(28, sample.kind = "Rounding")
S <- replicate(B, {
  sum(sample(c(l, x), size=n, replace=TRUE, prob=c(p, 1-p_loss))) 
})
mean(S < 0)

# Question 6a
# What is the expected value over 1,000 policies?
set.seed(29, sample.kind = "Rounding")
S <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, l), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(S)

# Question 6b
# What is the probability of losing money?
mean(S<0)

# Question 6c
# What is the probability of losing more than $1 million?
mean(S < -10^6)
















