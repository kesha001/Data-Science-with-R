# ggplot2 is from tydiverse, ggplot2 works only with data tables

#
# Basics of ggplot2
#

# load murders dataset
library(dslabs)
library(tidyverse)
data(murders)

#
# Creating a New Plot

ggplot(data = murders)

murders %>% 
  ggplot()

class(ggplot(data = murders))

#
# Layers

murders %>% 
  ggplot() +
  geom_point(aes(x = population/10^6, y = total)) +
  geom_text(aes(population/10^6, total, label = abb))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))  


#
# Tinkering
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))  

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)  

args(ggplot)


# we can assign global mapping in ggplot
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) + 
  geom_text(nudge_x = 1)

# local mapping > global mapping
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))


#
# Scales, Labels, and Colors

# set log 10 scale

p + geom_point(size = 3) + 
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

p + geom_point(size = 3) + 
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() 

# add libels to axes and add a title

p + geom_point(size = 3) + 
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# redefine p for everything of the plot axcept geom point

p <- murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
p + geom_point(size = 3)

# make all points blue 
p + geom_point(size = 3, color = "blue")

# map color for each point depending on the region (automaticly adds a label)
p + geom_point(aes(color = region), size = 3)

# average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)
r

# plot line with average murder rate
p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))
p

# dashed line and grey color, and because abline should be behind points firstly layer with ablien
p <- p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

# capitalize the name of the label
p <- p + scale_color_discrete(name = "Region")
p

#
# Add-on Packages
library(ggthemes)

p + theme_economist()
p + theme_fivethirtyeight()

library(ggrepel) # adds labels and ensures they do not fall on each other

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# the final plot 
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


#
# Other examples

# histogram for male heghts
heights %>% 
  filter(sex == "Male") %>% 
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black") + # define binwidth for the histogram with blue filling and outside as blue
  xlab("Male heights in inches") + # add x label
  ggtitle("Histogram") # add title

p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

# density plot for male heights
p + geom_density()

# add a filling
p + geom_density(fill = "blue")


# QQ plot (for this sample argument is needed)
p <- heights %>% filter(sex == "Male") %>% 
  ggplot(aes(sample = height))

p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data (because by defauld mean is 0 and sd = 1)
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data(z) against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


































