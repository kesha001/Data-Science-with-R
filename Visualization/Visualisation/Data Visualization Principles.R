#
# Show the Data

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)


#
# Slope Charts

library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 


#
# Case Study: Vaccines

# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")


# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")


# Line plot of measles rate by year and state

avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


# Titanic Survival Exercises

# installation

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
str(titanic)


# Question 2: Demographics of Titanic Passengers

titanic %>% 
  ggplot(aes(Age, fill = Sex)) +
  geom_density(aes(y = ..count.., alpha=0.2))


# Question 3: QQ-plot of Age Distribution

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))


titanic %>% 
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()


# Question 4: Survival by Sex

titanic %>% 
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()

titanic %>% 
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

# Question 5: Survival by Age

titanic %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(y = ..count.., alpha=0.2))


titanic %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(y = ..count.., alpha=0.2), position="stack")

titanic %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(alpha=0.2))


# Question 6: Survival by Fare

titanic %>% 
  filter(Fare != 0) %>% 
  ggplot(aes(y = Fare)) +
  geom_boxplot(aes(group = Survived)) +
  scale_y_continuous(trans = "log2")

# Question 7: Survival by Passenger Class

titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>% 
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())


# Question 8: Survival by Age, Sex and Passenger Class

titanic %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(y = ..count.., alpha=0.2)) +
  facet_grid(Sex ~ Pclass)




#
# Assessment Part 1: Properties of Stars
#

library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) 

# Question 1
str(stars)
mean(stars$magnitude)
sd(stars$magnitude)

# Question 2

stars %>% 
  ggplot(aes(magnitude)) +
  geom_density()

#Question 3

stars %>% 
  ggplot(aes(temp)) +
  geom_density()

# Question 4&5&6

stars %>% 
  ggplot(aes(log10(temp), magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse() 

str(stars)
# Question 7 & 8
stars %>% 
  ggplot(aes(log10(temp), magnitude, label = star, color = (star == "Sun"))) +
  geom_point() +
  scale_y_reverse() +
  geom_text()


# Question 9

stars %>% 
  ggplot(aes(log10(temp), magnitude, color = type)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_manual(values = c("#000000", "#AAAAAA", "#0022BB", "#22BB00", "#CCCCCC", "#CC00CC", "#CCCC00", "#00CCCC", "#CC0000", "#888888"))





#
# Assessment Part 2: Climate Change
#


library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)


str(temp_carbon)

# Question 1

temp_carbon %>% 
  .$year %>% 
  max()

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  pull(year) %>% 
  max()

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  max(year)

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  .$year %>% 
  max()

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  max(.$year)

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  select(year) %>% 
  max()


# Question 2

# What is the first year for which carbon emissions (carbon_emissions) data are available?
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  pull(year) %>% 
  min()

# What is the last year for which carbon emissions data are available?
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  pull(year) %>% 
  max()

#How many times larger were carbon emissions in the last year relative to the first year?
temp_carbon %>% 
  filter(year == 2014 | year == 1751) %>% 
  pull(carbon_emissions)

9855/3

# Question 3

# What is the first year for which global temperature anomaly (temp_anomaly) data are available?
min_year_temp <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>% 
  pull(year) %>% 
  min()

# What is the last year for which global temperature anomaly data are available?
max_year_temp <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>% 
  pull(year) %>% 
  max()

temp_carbon %>% 
  filter(year == min_year_temp) %>% 
  pull(temp_anomaly)

temp_carbon %>% 
  filter(year == max_year_temp) %>% 
  pull(temp_anomaly)

0.82 - (-0.11)


# Question 4

p <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>% 
  ggplot(aes(year, temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")

# Question 5&6

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

# Question 7

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  geom_line(aes(y = ocean_anomaly), color = "green") +
  geom_line(aes(y = land_anomaly), color = "brown")


str(greenhouse_gases)
# Question 8 & 9

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


str(temp_carbon)
# Question 10
temp_carbon %>% 
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

# Question 11
str(historic_co2)

co2_time <-  historic_co2 %>% 
  ggplot(aes(year, co2, color = source)) +
  geom_line()

historic_co2$year

# Question 12
co2_time + scale_x_continuous(limits = c( -800000,-775000))

co2_time + scale_x_continuous(limits = c(-375000,-330000))

co2_time + scale_x_continuous(limits = c(-140000,-120000))

co2_time + scale_x_continuous(limits = c(-3000,2018))

















