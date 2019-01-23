#
# The R Zone from Chapter 2
# 12/10/2018 - Jeff Smith
#
library(tidyverse)

# similar to the built-in mtcars dataset, but more records
cars <-read_csv(file = "../data/cars.txt")
cars2 <-read_csv(file = "../data/cars2.txt")
# note that cars3.txt and cars3.csv have different NA values :-(
cars3 <-read_csv(file = "../data/cars3.txt")

# Missing Data
# missing mpg
cars3 %>% 
  filter(is.na(mpg))

# remove records with NaNs
# mask of "complete" records
complete.cases(cars3)
# use the mask to select records to store in cars4
cars4 <- cars3[complete.cases(cars3),]
cars4

# Handling missing data
# Missing weight
cars3 %>% 
  filter(is.na(weightlbs))

# missing mpg
cars3 %>% 
  filter(is.na(mpg))
# since we know the row numbers ...
cars3[c(15, 19, 23, 88, 104, 166),]

# replace with the mean - Vectorized since it's a single value
cars3$mpg[is.na(cars3$mpg)] <- mean(cars3$mpg, na.rm=TRUE)

# Distribution of the current values
ggplot(data = cars3) +
  geom_histogram(mapping = aes(x = mpg), fill="blue", color="orange")
# sample from the existing values using:
sample(na.omit(cars3$mpg), 1)

# replace with a sample of other values - iterate since it's a function
# (there is likely a better (vectorized) way to do this)
for (row in 1:nrow(cars3)) {
  if(is.na(cars3[row,"mpg"])) {
    cars3[row,"mpg"] <- sample(na.omit(cars3$mpg), 1)
  }
}

# Identifying misclassifications - Similar to Table 2.2
unique(cars2$brand)
cars2 %>% 
  group_by(brand) %>% 
  summarize(
    count = n()
  )


# Identifying outliers -- graphical methods
# histogram of vehicle weight
ggplot(data = cars2) +
  geom_histogram(mapping = aes(x = weightlbs), binwidth = 200, fill="blue", color="orange")

# Scatter plot of mpg vs. weight
ggplot(data=cars2) +
  geom_point(aes(x=weightlbs, y=mpg, color=cylinders))


# Data Transformations

# Min-max normalization
# vehicle weight
summary(cars$weightlbs)
mi <- min(cars$weightlbs)
ma <- max(cars$weightlbs)
cars$mmweight<- (cars$weightlbs - mi)/(ma - mi)
summary(cars$mmweight)
# Looka the two together
require('gridExtra')
plot1 <- ggplot(data = cars) +
  geom_histogram(mapping = aes(x = mmweight), fill="blue", color="orange")
plot2 <- ggplot(data = cars) +
  geom_histogram(mapping = aes(x = weightlbs), fill="blue", color="orange")
grid.arrange(plot1,plot2,ncol=2)

# Same thing for cubic inches
summary(cars$cubicinches)
mi <- min(cars$cubicinches)
ma <- max(cars$cubicinches)
cars$mmcubicinches<- (cars$cubicinches - mi)/(ma - mi)
summary(cars$mmcubicinches)


# Z-score standardization
m <- mean(cars$weightlbs)
s <- sd(cars$weightlbs)
cars$zweight <- (cars$weightlbs - m)/s
plot1 <- ggplot(data = cars) +
  geom_histogram(mapping = aes(x = zweight), fill="blue", color="orange")
plot2 <- ggplot(data = cars) +
  geom_histogram(mapping = aes(x = weightlbs), fill="blue", color="orange")
grid.arrange(plot1,plot2,ncol=2)


# Decimal scaling
max(abs(cars$weightlbs))
# --- 4 digits
cars$dweight <- cars$weightlbs/(10^4)
plot1 <- ggplot(data = cars) +
  geom_histogram(mapping = aes(x = dweight), fill="blue", color="orange")
plot2 <- ggplot(data = cars) +
  geom_histogram(mapping = aes(x = weightlbs), fill="blue", color="orange")
grid.arrange(plot1,plot2,ncol=2)


# skewness - 3*(mean - median)/stddev
sk <- 3*(mean(cars$zweight) - median(cars$zweight))/sd(cars$zweight)

ndata <- rnorm(5000)
hist(ndata)
3*(mean(ndata) - median(ndata))/sd(ndata)
