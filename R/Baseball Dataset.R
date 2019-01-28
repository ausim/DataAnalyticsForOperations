#
# Baseball dataset from Larose & Larose
#
#
library(tidyverse)

# read the dataset
baseball <- read_csv("..\\data\\baseball.csv", col_names = TRUE)
summary(baseball)

ggplot(data = baseball) +
  geom_histogram(mapping = aes(x = age), bins=15)

ggplot(data = baseball) +
  geom_histogram(mapping = aes(x = games), bins=20)

filter(baseball, games <= 10)

ggplot(data = baseball) +
  geom_histogram(mapping = aes(x = bat_ave))

filter(baseball, bat_ave >= .5)

ggplot(data = baseball) +
  geom_histogram(mapping = aes(x = at_bats))

ggplot(data = filter(baseball, at_bats >= 100)) +
  geom_histogram(mapping = aes(x = bat_ave))

ggplot(data=baseball) +
  geom_point(aes(x=age, y=hits))

ggplot(data=baseball) +
  geom_point(aes(x=hits, y=doubles))

ggplot(data=baseball) +
  geom_point(aes(x=stolen_bases, y=caught_stealing))

ggplot(data=baseball) +
  geom_point(aes(x=hits, y=homeruns))

ggplot(data=baseball) +
  geom_point(aes(x=age, y=homeruns))

ggplot(data=baseball) +
  geom_point(aes(x=age, y=hits))

library(GGally)
ggpairs(select(baseball, c('age', 'games', 'bat_ave', 'hits', 'doubles', 'walks', 'RBIs')))

        