#
# KC Real Estate - PCA Regression
# Jeff Smith 
#
library(tidyverse)
library(GGally)

# Read in
kcre <- read_csv("..\\data\\kc_house_data.csv")

kcre <- kcre %>%
  select('price', 'sqft_living', 'sqft_basement','sqft_lot', 'bedrooms', 'bathrooms')

# Just for convenience in class - pick a random sample (the dataset is big)
kcre <- sample_n(kcre, 500)


# start with a simple linear regression
(r <- lm(price~sqft_living, kcre))
print(summary(r))
# have look ...
ggplot(data=kcre) +
  geom_point(aes(x=sqft_living, y=price)) +
  geom_abline(aes(slope=r$coefficients[2],intercept=r$coefficients[1]),color='red')

# regress on all 5 of the varilables
(r <- lm(price~sqft_living+sqft_lot+sqft_basement+sqft_lot+bedrooms+bathrooms, kcre))
print(summary(r))

# Check the pairs
ggpairs(select(kcre, c('sqft_living', 'sqft_basement', 'sqft_lot', 'bedrooms','bathrooms')))

# PCA
# dataframe of the predictors (no target)
x <- kcre %>%
  select('sqft_living', 'sqft_basement','sqft_lot', 'bedrooms', 'bathrooms')
prcomp.x <- prcomp(x, center = TRUE, scale = TRUE)
summary(prcomp.x)
# the rotations (the eigenvectors)
prcomp.x$rotation
screeplot(prcomp.x, type="lines",col=3)

# see the first two sets of loadings
prcomp.x$rotation[,1:2]

# create a new datframe with the transformed observations
newdf <- as_tibble(prcomp.x$x[,1:2])
newdf['price'] <- kcre['price']
# regress on both
(r <- lm(price~PC1+PC2, newdf))
print(summary(r))

# try just the first
(r <- lm(price~PC1, newdf))
print(summary(r))
ggplot(data=newdf) +
  geom_point(aes(x=PC1, y=price)) +
  geom_abline(aes(slope=r$coefficients[2],intercept=r$coefficients[1]),color='red')



