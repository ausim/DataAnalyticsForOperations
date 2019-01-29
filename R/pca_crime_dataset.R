#
# Crime dataset PCA example from https://uc-r.github.io/pca.  The
# web site has a nice explanation of the process.
#
# 1/4/2019 - Jeff Smith
#
library(tidyverse)
library(gridExtra)

# the dataset that we want to use is built-in to R
data("USArrests")
head(USArrests, 10)

# check the mean and variance of each column (variable)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Let's check the pairs
pairs(USArrests)

# Normalize the variables
scaled_df <- apply(USArrests, 2, scale)
head(scaled_df)

# check the means and variances of the normalized data.
# should be 0, 1 (if scale() works as expected)
apply(scaled_df, 2, mean)
apply(scaled_df, 2, var)

# The manual way ...
arrests.cov <- cov(scaled_df)
arrests.eigen <- eigen(arrests.cov)
str(arrests.eigen)
arrests.eigen$values
arrests.eigen$vectors
# By default, eigenvectors in R point in the negative direction

# grab the first two sets of loadings
(phi <- arrests.eigen$vectors[,1:2])
# Flip the negatives on the PCs to make them more intuitive
phi <- -phi
row.names(phi) <- c("Murder", "Assult", "UrbanPop", "Rape")
colnames(phi) <- c("PC1", "PC2")
phi

# Calculate Principal Component scores (Transformed data)
PC1 <- as.matrix(scaled_df) %*% phi[,1]
PC2 <- as.matrix(scaled_df) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(State = row.names(USArrests), PC1, PC2)
head(PC)

# Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of USArrests Data")

# Proportion of variance explained
PVE <- arrests.eigen$values / sum(arrests.eigen$values)
round(PVE, 2)

# Scree plots
# PVE (aka scree) plot
PVEplot <- qplot(c(1:4), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Cumulative PVE plot
cumPVE <- qplot(c(1:4), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)

# The built-in way
pca_result <- prcomp(USArrests, scale = TRUE)
names(pca_result)

# means
pca_result$center
# standard deviations
pca_result$scale

# component loadings
pca_result$rotation
# switch the negatives
pca_result$rotation <- -pca_result$rotation
pca_result$rotation
# switch the component values also
pca_result$x <- - pca_result$x
head(pca_result$x)

biplot(pca_result, scale = 0)
# Use choices=3:4 if you want to see the other components)
biplot(pca_result, choices=3:4, scale = 0)

# Variance eplained
pca_result$sdev
(VE <- pca_result$sdev^2)
PVE <- VE / sum(VE)
round(PVE, 2)
