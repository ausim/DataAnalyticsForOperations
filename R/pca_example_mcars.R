#
# PCA Examle based on https://www.datacamp.com/community/tutorials/pca-analysis-r
#
# 1/3/2019 - Jeff Smith
#

# the dataset we'll use
vals <- mtcars[,c(1:7,10,11)]

# quick visual check of the correlations
pairs(vals)

# Have a look at the covarance matrix
cov(vals)

# grab the numeric columns and use the PCA function prcomp()
mtcars.pca <- prcomp(vals, center = TRUE,scale. = TRUE)
summary(mtcars.pca)
# the eigenvalues are the square sof the std. dev's. 

# the eigenvectors
mtcars.pca$rotation

# instructions for installing the library at the web site above.
library(ggbiplot)
ggbiplot(mtcars.pca)

# add labels with car names
ggbiplot(mtcars.pca, labels=rownames(mtcars))

# caregorize by country of origin
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

# Principal components 3 and 4
ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4), labels=rownames(mtcars), groups=mtcars.country)


# New car --- Spacecar!
# Note that this car has a 60-cylinder engine, gets 1000 mpg has 500 HP, etc.  
spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)

mtcarsplus <- rbind(mtcars, spacecar)
mtcars.countryplus <- c(mtcars.country, "Jupiter")

mtcarsplus.pca <- prcomp(mtcarsplus[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

# the PCA has changed quite a bit -- as expected with a super car
ggbiplot(mtcarsplus.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"), groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample added")+
  theme_minimal()+
  theme(legend.position = "bottom")

# Compare the rotations (eginvectors)
mtcars.pca$rotation
mtcarsplus.pca$rotation

