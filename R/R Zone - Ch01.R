#
# The R Zone from Chapter 1 
# 12/10/2018 - Jeff Smith
# 
library(tidyverse)

# From the book (generic R)
# similar to the built-in mtcars dataset, but more records
cars <-read.csv(file = "../data/cars.txt", stringsAsFactors = FALSE)

# define a matrix
mat <- matrix(0.0, nrow=3, ncol=2)
mat
colnames(mat) <- c('Var 1', 'Var 2')
colnames(mat)
mat

# Subset data and declar new variables
cars.rsub <- cars[1:50,]
cars.csub <- cars[,1:3]
cars.rcsub <- cars[c(1, 3, 5), c(2, 4)]
cars.vsub <- cars[which(cars$mpg>30),]

# using the tidyverse
carst <- as_tibble(cars)

slice(carst, 1:50)

select(carst, c(1, 3))

carst %>% 
  select(c(1, 3))

carst %>% 
  slice(c(1,3,5)) %>%
  select(c(2,4))

filter(carst, mpg > 30)

carst %>% 
  filter(mpg > 30)


# Sample histogram
ggplot(data = cars) +
  geom_histogram(mapping = aes(x = mpg), binwidth = 3, fill="blue", color="orange")
  
# Sample scatter plot
ggplot(data=cars) +
  geom_point(aes(x=cubicinches, y=mpg,color=cylinders))


