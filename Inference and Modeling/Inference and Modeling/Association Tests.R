# Research funding rates example

# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))


# Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab


# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")


#
# Chi-Squared Tests
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))


# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value


# Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()


#
# Comprehensive Assessment: Brexit
#

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Question 1: Expected value and standard error of a poll
# What is the expected total number of voters in the sample choosing "Remain"?
N <- 1500
p*N

# What is the standard error of the total number of voters in the sample choosing "Remain"?
sqrt(1500)*sqrt(p*(1-p))

# What is the standard error of , x hat  proportion of "Remain" voters?
sqrt(p*(1-p)/N)

# What is the standard error of , the spread between the proportion of "Remain" voters and "Leave" voters?
2*sqrt(p*(1-p)/N)


# Question 2: Actual Brexit poll estimates
data(brexit_polls)
head(brexit_polls)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

mean(brexit_polls$spread)
sd(brexit_polls$spread)

mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

# Question 3: Confidence interval of a Brexit poll
brexit_polls[1,]

#lower bound
x_hat_brexit <- brexit_polls[1,]$x_hat
N <- brexit_polls[1,]$samplesize
se <- sqrt(x_hat_brexit*(1-x_hat_brexit)/N)
brexit_polls[1,]$x_hat - qnorm(0.975)*se

#higher bound
brexit_polls[1,]$x_hat + qnorm(0.975)*se

#
# Brexit poll analysis - Part 2

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Question 4: Confidence intervals for polls in June
june_polls <- brexit_polls %>% 
  filter(enddate >= "2016-06-01")

nrow(june_polls)

d_r <- (-0.038+1)/2

june_polls2 <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_d = 2*se_x_hat,
         lower = spread - qnorm(0.975)*se_d,
         upper = spread + qnorm(0.975)*se_d,
         hit = -0.038 >= lower & -0.038 <= upper)

mean(june_polls2$lower <= 0 & june_polls2$upper >= 0)
mean(june_polls2$lower > 0)
mean(june_polls2$hit)


june_polls2 %>% 
  group_by(pollster) %>% 
  summarise(prop = mean(hit)) %>% 
  arrange(prop)

june_polls2 %>% 
  group_by(pollster) %>% 
  summarise(prop = mean(hit)) %>% 
  arrange(prop)




june_polls %>% 
  ggplot(aes(poll_type, spread)) +
  geom_boxplot()

# Question 7: Combined spread across poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)


2*sqrt(combined_by_type[1,]$p_hat*(1-combined_by_type[1,]$p_hat)/combined_by_type[1,]$N)

2*combined_by_type[1,]$p_hat-1 - qnorm(0.975)*2*sqrt(combined_by_type[1,]$p_hat*(1-combined_by_type[1,]$p_hat)/combined_by_type[1,]$N)

2*combined_by_type[1,]$p_hat-1 + qnorm(0.975)*2*sqrt(combined_by_type[1,]$p_hat*(1-combined_by_type[1,]$p_hat)/combined_by_type[1,]$N)


combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            spread_lower = spread - qnorm(.975)*se_spread,
            spread_upper = spread + qnorm(.975)*se_spread)

combined_by_type$spread_upper - combined_by_type$spread_lower


# Brexit poll analysis - Part 3
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481


# Question 9: Chi-squared p-value
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit


brexit_hit <- brexit_hit %>%
  group_by(hit) %>% 
  summarise(Online = sum(poll_type=="Online"), Telephone = sum(poll_type=="Telephone")) %>%
  select(-hit) 


brexit_hit %>% 
  chisq.test()


(brexit_hit$Online[2] / sum(brexit_hit$Online))

(brexit_hit$Telephone[2] / sum(brexit_hit$Telephone))

# Question 10: Odds ratio of online and telephone poll hit rate

brexit_hit

(brexit_hit$Online[2] / sum(brexit_hit$Online)) / ((brexit_hit$Online[1] / sum(brexit_hit$Online)))

(brexit_hit$Telephone[2] / sum(brexit_hit$Telephone)) / (brexit_hit$Telephone[1] / sum(brexit_hit$Telephone))

((brexit_hit$Online[2] / sum(brexit_hit$Online)) / ((brexit_hit$Online[1] / sum(brexit_hit$Online)))) /
((brexit_hit$Telephone[2] / sum(brexit_hit$Telephone)) / (brexit_hit$Telephone[1] / sum(brexit_hit$Telephone)))


# Question 11: Plotting spread over time
d <- -0.038

brexit_polls %>% 
  ggplot(aes(enddate, spread, color=poll_type)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.4) +
  geom_hline(yintercept=d)



# Question 12: Plotting raw percentages over time
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

unique(brexit_long$vote)

brexit_long %>% 
  ggplot(aes(enddate, proportion, color=vote)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3)
