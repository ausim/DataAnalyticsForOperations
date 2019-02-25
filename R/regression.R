#
# Regression
#
# 1/25/2019 - Jeff Smith
#

library(tidyverse)

#
# Cereals - From Larose and Larose
#
cereals <- read_csv("../data/cereals.csv", col_names = TRUE)
ggplot(data=cereals) +
  geom_point(aes(x=Sugars, y=Rating))
(r <- lm(Rating~Sugars, cereals))
print(summary(r))

# use lm to add the regression line
ggplot(data=cereals,aes(x=Sugars, y=Rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# manually add the line using the slope/intercept
ggplot(data=cereals) +
  geom_point(aes(x=Sugars, y=Rating)) +
  geom_abline(aes(slope=r$coefficients[2],intercept=r$coefficients[1]),color='red')


#
# Data from https://newonlinecourses.science.psu.edu/stat462/node/101/
#
# Poverty
poverty <- read_csv("../data/teen_birthrate_poverty.csv", col_names = TRUE)
ggplot(data=poverty, aes(x=PovPct, y=Brth15to17)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE)
(r <- lm(Brth15to17~PovPct,poverty))
print(summary(r))

#
# Lung function
lung_function <- read_csv("../data/lung_function.csv", col_names = TRUE)
ggplot(data=lung_function) +
  geom_point(aes(x=age, y=FEV))
(r <- lm(FEV~age,lung_function))
print(summary(r))


# Multiple regression
(r <- lm(Rating~Sugars, cereals))
print(summary(r))

(r <- lm(Rating~Fiber, cereals))
print(summary(r))

(r <- lm(Rating~Sugars+Fiber, cereals))
print(summary(r))
