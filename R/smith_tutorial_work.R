#
# Work from "A Tutorial on Principal Components Analysis", by Lindsay J. Smith
#
# 1/4/2019 - Jeff Smith
#
library(tidyverse)

# Exercises from page 11
m = cbind(c(3, -4, -6), c(0, 1, 0), c(1, 2, -2))
m
eigenValues <- eigen(m)$values
eigenVectors <- eigen(m)$vectors
eigenValues
eigenVectors
# -------------------------

# PCA section
x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)
df <- as_tibble(data.frame("x"=x, "y" = y))
# Scatter plot of y vs x
ggplot(data=df) +
  geom_point(aes(x=x, y=y))

# Center the values - note that he doesn't scale, just centers.
dfCentered <- as_tibble(cbind(x=(x-mean(df$x)), y=(df$y-mean(df$y))))
ggplot(data=dfCentered) +
  geom_point(aes(x=x, y=y))

# check the variances of the centered data.
apply(dfCentered, 2, var)

# Covariance matrix
covdf = cov(dfCentered)
covdf

# Get the eigenvalues and eigenvectors
(eigenValues <- eigen(covdf)$values)
(eigenVectors <- eigen(covdf)$vectors)
# the first eigenvector is negative of what is shown in the Smith tutorial.  This shouldn't matter.
# See https://stats.stackexchange.com/questions/154716/pca-eigenvectors-of-opposite-sign-and-not-being-able-to-compute-eigenvectors-wi
# Per, https://uc-r.github.io/pca, by default, eigenvectors in R point in the negative direction
# Also, R sorts the eginvectors in decending order based on the eginvalues

# Plot the centered points and the lines defined by the
# eigenvectors (the principal components)
ggplot(data=dfCentered) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  geom_point(aes(x=x, y=y)) +
  geom_abline(intercept=0, slope=eigenVectors[2,1]/eigenVectors[1,1], color="red") +
  geom_abline(intercept=0, slope=eigenVectors[2,2]/eigenVectors[1,2], color="blue")

# proportion of variance explained
# Note that we would divide by the number of variables (2, in this case)
#   if we had scaled to variance to 1.
eigenValues / (covdf[1,1] + covdf[2,2])

# Transfomed data
# For some reason, he wants rows ...
FinalData <- as_tibble(t(eigenVectors) %*% t(data.matrix(dfCentered)))
# I'm going to do the tranaspose here
dfTransformed <- as_tibble(t(t(eigenVectors) %*% t(data.matrix(dfCentered))))
# plot the transformed data
ggplot(data=dfTransformed) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  geom_point(aes(x=V1, y=V2))
# Can also use Z*e to get the transfomed data - not sure when he doesn't
# do this -- seems easier to me ...
test <- as_tibble(data.matrix(dfCentered) %*% eigenVectors)
# plot the transformed data
ggplot(data=test) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  geom_point(aes(x=V1, y=V2))

# To get the centered data back:
test <- t(eigenVectors %*% t(dfTransformed))
# original values - uncenter
orig <- as_tibble(test + c(mean(df$x), mean(df$y)))

# Using the built-in PCA tools
df.pca <- prcomp(df, center = TRUE, scale. = FALSE)
summary(df.pca)
df.pca$rotation
# Back to the eigenValues
df.pca$sdev^2
# Transfomed data
df.pca$x
ggplot(data=as_tibble(df.pca$x)) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  geom_point(aes(x=PC1, y=PC2))

