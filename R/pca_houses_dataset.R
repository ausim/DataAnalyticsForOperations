#
# Houses dataset from Larose & Larose Section 4.3 (p.96)
#
# Note that this anaysis uses the whole dataset -- the book
# partitions the data into training and test datasets.
#
library(tidyverse)

# read the dataset
df = read_csv("..\\data\\cadata.csv", col_names = TRUE)
summary(df)

# extract and then remove the target, median price (column 1)
med_price <- pull(df, "median_house_value")
df <- df[,-1]
# histogram of median price
hist(med_price)

# Replicates Figure 4.3 -- takes a while to run
pairs(df)

# PCA
# the easy way 
# from https://www.datacamp.com/community/tutorials/pca-analysis-r
prcomp.df <- prcomp(df, center = TRUE, scale = TRUE)
summary(prcomp.df)
# % of variance values match Table 4.3

# the rotations (the eignvectors)
prcomp.df$rotation
screeplot(prcomp.df, type="lines",col=3)
# matches the scree plot in Figure 4.4

# The manual way
# scale the dataset
# from https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
sdf <- scale(df)
# check the means/sdt dev.
colMeans(sdf)  # faster version of apply(scaled.dat, 2, mean)
apply(sdf, 2, sd)

# covariance
cov_sdf <- cov(sdf)

# eigenvalues and eigenvectors 
eigenValues <- eigen(cov_sdf)$values
eigenVectors <- eigen(cov_sdf)$vectors
eigenVectors
# will be negatives of the vectors from the prcomp function (but that doesn't matter)
eigenValues

# the principal components are different from the values that the book gets (Table 4.2).  
# The book used 'components' (loadings). # loading = Eigenvector * sqrt(Eigenvalue)
# Consider the first PC and the first column in Table 4.2
eigenVectors[,1] * sqrt(eigenValues[1])
# The book only uses the training part of the data, so the results won't be exact

# the transformed values;
prcomp.df$x
# or
sdf %*% eigenVectors

