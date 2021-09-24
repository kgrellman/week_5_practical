library(tidyverse)
library(magrittr)
# Question 0
# write a function that takes a vector and replaces it with the mean of that vector
x <- c(1, NA, 2, 3, 2, 2, NA)
replace_na_mean <- function(x) {
  mean(x, na.rm = TRUE)
}
replace_na_mean(x)
replace_na_mean(c(1, NA, 2, 3, 2, 2, NA))

# Question 1
# write a function that returns the sample space for the experiment consisting of sampling a 4 digit PIN
get_all_perms <- function(x) {
  pin <- expand.grid(0:9, 0:9, 0:9, 0:9)
  nrow(pin)
}
get_all_perms()

# Question 2
get_all_perms <- function(size) {
  replicate(size, 0:9, simplify = FALSE) %>%
  expand.grid()
}
get_all_perms(size = 4)
get_all_perms(size = 3)
get_all_perms(size = 1)

# Question 3
# probability that a fish will be diseased
fish <- c(0:340)
probs <- mapply(dbinom, fish, size=340, prob=0.43)
which.max(probs)

# Question 4
# plot mean=10, sd=0.5 and mean=10.2, sd=0.5
x_vals = seq(8, 12, 0.05)
probs_1 = mapply(dnorm, x_vals, mean = 10, sd = 0.5)
probs_2 = mapply(dnorm, x_vals, mean = 10.2, sd = 0.5)

ggplot() +
  geom_line(aes(x=x_vals, y=probs_1, color="blue"), size=1) +
  geom_line(aes(x=x_vals, y=probs_2, color="red"), size=1) +
  xlim(8, 12) + 
  theme(axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18)) + 
  scale_color_manual(labels = c("N(10, 0.5)", "N(10.2, 0.5)"), values = c("red", "blue"))

# draw 40 samples from first distribution
set.seed(42)
x_sample <- rnorm(40, mean=10, sd=0.5)
x_sample
# draw 40 samples from second distribution
y_sample <- rnorm(40, mean=10.2, sd=0.5)
y_sample
# do a t-test to compute x_sample and y_sample
t.test(x_sample, y_sample)
# p-value is >5, therefore the data sets are statistically different

# Question 5 - Challenge
x_vals_5 = seq(8, 12, 0.1)
probs_5 = mapply(dnorm, x_vals_5, mean = 10, sd = 0.5)

ggplot() +
  geom_line(aes(x=x_vals_5, y=probs_5), color="black", size=1) +
  xlim(8, 12)

probs_5
# for pdf, sum should equal 1
sum(probs_5)
# multiply by small value
sum(probs_5)*.1
