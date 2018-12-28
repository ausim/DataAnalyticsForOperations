#
# The R Zone from Chapter 2
# 12/10/2018 - Jeff Smith
#
library(ggplot2)

# similar to the built-in mtcars dataset, but more records
cars <-read.csv(file = "../data/cars.txt", stringsAsFactors = FALSE)
cars2 <-read.csv(file = "../data/cars2.txt", stringsAsFactors = FALSE)
cars3 <-read.csv(file = "../data/cars3.txt", stringsAsFactors = FALSE)


# histogram of vehicle weight
ggplot(data = cars2) +
  geom_histogram(mapping = aes(x = weightlbs), binwidth = 200, fill="blue", color="orange")

# Scatter plot of mpg vs. weight
ggplot(data=cars2) +
  geom_point(aes(x=weightlbs, y=mpg, color=cylinders))


# Data Transformations

# Min-max normalization
summary(cars$weightlbs)
mi <- min(cars$weightlbs)
ma <- max(cars$weightlbs)
minmax.weight <- (cars$weightlbs - mi)/(ma - mi)
cars$mmweight<- (cars$weightlbs - mi)/(ma - mi)
summary(cars$mmweight)
ggplot(data = cars) +
  geom_histogram(mapping = aes(x = mmweight), fill="blue", color="orange")

# Z-score standardization
m <- mean(cars$weightlbs)
s <- sd(cars$weightlbs)
cars$zweight <- (cars$weightlbs - m)/s
ggplot(data = cars) +
  geom_histogram(mapping = aes(x = zweight), fill="blue", color="orange")

# Decimal scaling
max(abs(cars$weightlbs))
# --- 4 digits
cars$dweight <- cars$weightlbs/(10^4)
ggplot(data = cars) +
  geom_histogram(mapping = aes(x = dweight), fill="blue", color="orange")


