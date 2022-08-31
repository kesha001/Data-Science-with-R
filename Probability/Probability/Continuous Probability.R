# Cumulative distribution function
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
F <- function(a) mean(x <= a)

1 - F(70)    # probability of male taller than 70 inches

#
# Theoretical Distribution

# We can estimate the probability that a male is taller than 70.5 inches using pnorm
1 - pnorm(70.5, mean(x), sd(x))


# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))


# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


#
# Monte Carlo Simulations

# : Generating normally distributed random numbers
# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)


# Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)


x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()



# Questions 1 and 2: ACT scores, part 1
set.seed(16, sample.kind = "Rounding")

B <- 10000
act_scores <- rnorm(B, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores <= 10)


x <- 1:36

y <- dnorm(x, 20.9, 5.7)

plot(x, y)


# Questions 3 and 4: ACT scores, part 2
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)

mean(z_scores > 2)

act_scores[z_scores >= 2]

qnorm(0.975, mean(act_scores), sd(act_scores))


f <- function(x){
  mean(act_scores <= x)
}

min(which(sapply(x, f)>=0.95))


qnorm(0.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)

sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])


theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles, sample_quantiles)
