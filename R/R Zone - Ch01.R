#
# The R Zone from Chapter 1 
#

# similar to the built-in mtcars dataset
cars <-read.csv(file = "../data/cars.txt", stringsAsFactors = FALSE)


# define a matrix
mat <- matrix(0.0, nrow=3, ncol=2)
mat

colnames(mat) <- c('Var 1', 'Var 2')
colnames(mat)


# Subset data and declar new variables
cars.rsub <- cars[1:50,]
cars.csub <- cars[,1:3]
cars.rcsub <- cars[c(1, 3, 5), c(2, 4)]
cars.vsub <- cars[which(cars$mpg>30),]


library(ggplot2)
# histogram
ggplot(data = cars) +
  geom_histogram(mapping = aes(x = mpg), binwidth = 3, fill="blue", color="orange")
  
# Scatter plot
ggplot(data=cars) +
  geom_point(aes(x=cubicinches, y=mpg,color=cylinders))


