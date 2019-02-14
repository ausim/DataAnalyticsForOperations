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


hist(baseball$hits)
# mean number of hits == 65
t.test(baseball$hits, mu = 65, alternative = "two.sided")

# mean number of hits less that 55
t.test(baseball$hits, mu = 55, alternative = "greater")

# mean number of day minutes == 180
t.test(churn$DayMins, mu = 180, alternative = "two.sided")

# mean number of day minutes > 180
t.test(churn$DayMins, mu = 180, alternative = "less")


#
# H0: proportion of churners == .15
p <- sum(churn$Churn == 'True.')/nrow(churn)
zdata <- (p-.15)/sqrt(.15*.85/nrow(churn))
(pval <- 2*pnorm(zdata))


#
# Equality of means
#
# Split the churn dataset into a "training" set and a "test" set.
# From https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(churn)), size = 2529)
churn.train <- churn[train_ind, ]
churn.test  <- churn[-train_ind, ]
# Check the means and std. dev. of the split datasets
mean(churn.train$CustServCalls);mean(churn.test$CustServCalls)
sd(churn.train$CustServCalls); sd(churn.test$CustServCalls)
# Run the t-test
t.test(churn.train$CustServCalls,churn.test$CustServCalls , alternative = "two.sided", var.equal = FALSE)

# See how this works
# This gives us the right of t_data
# Note that we're using min(n1-1, n2-1) for DOF while the t.test uses the
# more complicated expression.
pt(.7134, 804, lower.tail=FALSE)
# since it's a 2-tail test, the p-value is:
2*pt(.7134, 804, lower.tail=FALSE)
# Go back and change the random number seed -- different sample

# Example from Section 6.3
pchisq(1.15, 2,lower.tail=FALSE)

# Example from Section 6.4
pchisq(1.04, 2,lower.tail=FALSE)


# ANOVA
a <- c(30, 40, 50, 60)
b <- c(25, 30, 50, 55)
c <- c(25, 30, 40, 45)
ab <- append(a, b)
datavalues <- append(ab, c)
datalabels <- factor(c(rep("a",length(a)),rep("b",length(b)),rep("c",length(c))))
anova.results <- aov(datavalues ~ datalabels)
summary(anova.results)

d <- c(43, 45, 45, 47)
e <- c(37, 40, 40, 43)
f <- c(34, 35, 35, 36)
de <- append(d, e)
datavalues <- append(de, f)
datalabels <- factor(c(rep("d",length(d)),rep("e",length(e)),rep("f",length(f))))
anova.results <- aov(datavalues ~ datalabels)
summary(anova.results)

