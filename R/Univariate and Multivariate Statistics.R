#
# Univariate and Multivariate Statistics
#   Material from Chapters 5 and 6 of Larose and Larose and external sources
# 2/2/2019 - Jeff Smith
#

library(tidyverse)

# Random N(225, 47)
df <- rnorm(3000, 225, 47)
summary(df)

# The churn dataset
churn <- read_csv("../data/churn.txt")
summary(churn)

# The arrests dataset
data("USArrests")
arrests <- as_tibble(USArrests)
summary(arrests)

# The baseball dataset
baseball <- read_csv("..\\data\\baseball.csv", col_names = TRUE)
summary(baseball)

#
# Parameter Estimates
#
# Consider the value of DayMins from churn.  
# Pick a random value:
churn[sample.int(3333,1), 8]
# Clearly this is governed by the distribution
hist(churn$DayMins)

# Suppose we're interested in the mean value
mean(churn$DayMins)
# the std dev
sd(churn$DayMins)
# and the quantile summary
quantile(churn$DayMins)
# and the proportion with >= 250 mins
sum(churn$DayMins >= 250)/nrow(churn)

# What if we sample some random values and compute the mean?
mean(churn[sample.int(3333, 25),8]$DayMins)
# ditto for the std dev.
sd(churn[sample.int(3333, 25),8]$DayMins)
# ditto for the quantile summary
quantile(churn[sample.int(3333, 25),8]$DayMins)
# ditto for the proportion
sum(churn[sample.int(3333, 25),8] >= 250)/25

#
# Switch to the N(225, 47) samples:
mean(rnorm(100, 225, 47))
#




# After Monte Carlo
# Let's estimate the distribution ...
n <- 100
test <- rep(0,1000)
for (i in 1:1000) {
  test[i] <- mean(rnorm(n, 225, 47))
}
hist(test)
mean(test)
sd(test)
47/sqrt(n)


# mean number of hits == 65
t.test(baseball$hits, mu = 65, alternative = "two.sided")

# mean number of hits less that 55
t.test(baseball$hits, mu = 55, alternative = "greater")

# mean number of day minutes == 180
t.test(churn$DayMins, mu = 180, alternative = "two.sided")

# mean number of day minutes > 180
t.test(churn$DayMins, mu = 180, alternative = "less")
