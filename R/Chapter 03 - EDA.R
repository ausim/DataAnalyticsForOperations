# 
# Chapter 03 - EDA.R -- Based on material from Chapter 3 of Larose and Larose, 2015 
#  Uses the tidyverse rather than plain R.
#
# 12/22/2018 - Jeff Smith
#
library(tidyverse)

churn <- read_csv("../data/churn.txt")

# Explore the data
summary(churn)
# Sample histogram -- account length 
ggplot(data = churn) +
  geom_histogram(mapping = aes(x = AccountLength))
# DayMins
ggplot(data = churn) +
  geom_histogram(mapping = aes(x = DayMins))
# DayCalls
ggplot(data = churn) +
  geom_histogram(mapping = aes(x = DayCalls))
# DayCharge
ggplot(data = churn) +
  geom_histogram(mapping = aes(x = DayCharge))

# Churners
# Churn bar chart
ggplot(data = churn) +
  geom_bar(mapping = aes(x=Churn)) + 
  coord_flip()
# Summaries with percentages
churn %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))
# With some other variable means
churn %>%
  group_by(Churn) %>%
  summarize(n = n(), AvgDayMins = mean(DayMins), AvgNightMins = mean(NightMins), AvgCustServ = mean(CustServCalls))

# International Plan
# International plan bar chart
ggplot(data = churn) +
  geom_bar(mapping = aes(x=IntPlan)) + 
  coord_flip()
# Summary
churn %>%
  group_by(IntPlan) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Churn with Int'l plan
ggplot(data = churn) +
  geom_bar(mapping = aes(x=Churn, fill=IntPlan)) +
  coord_flip()
# normalize
ggplot(data = churn) +
  geom_bar(mapping = aes(x=Churn, fill=IntPlan), position='fill') +
  coord_flip()
# flip the variables
ggplot(data = churn) +
  geom_bar(mapping = aes(x=IntPlan, fill=Churn)) +
  coord_flip()
# normalize
ggplot(data = churn) +
  geom_bar(mapping = aes(x=IntPlan, fill=Churn), position='fill') +
  coord_flip()

# Contingency tables
# group and summarize
churn %>% 
  group_by(IntPlan, Churn) %>% 
  summarize(n = n())
# Spread by IntPlan value  
churn %>% 
  group_by(IntPlan, Churn) %>% 
  summarize(n = n()) %>%
  spread(key = IntPlan, value = n)
# Same data, but spead by Churn
churn %>% 
  group_by(Churn, IntPlan) %>% 
  summarize(n = n()) %>%
  spread(key = Churn, value = n)


# Churn vs. Voice Mail Plan
ggplot(data = churn) +
  geom_bar(mapping = aes(x=VMailPlan, fill=Churn)) +
  coord_flip()
# normalized
ggplot(data = churn) +
  geom_bar(mapping = aes(x=VMailPlan, fill=Churn), position='fill') +
  coord_flip()
# contingency table
churn %>% 
  group_by(Churn, VMailPlan) %>% 
  summarise(n = n()) %>%
  spread(key = VMailPlan, value = n)

# Numerical Variables
# Customer Service Calls
ggplot(data = churn) +
  geom_bar(mapping = aes(x=CustServCalls, fill=Churn))
ggplot(data = churn) +
  geom_bar(mapping = aes(x=CustServCalls, fill=Churn), position='fill')

# Day Minutes
ggplot(data = churn) +
  geom_histogram(mapping = aes(x=DayMins, fill=Churn))
ggplot(data = churn) +
  geom_histogram(mapping = aes(x=DayMins, fill=Churn), position='fill')


#
# Scatter plot of Evening minutes vs. Day minutes
#  Seems like a clear transition line
ggplot(data=churn) +
  geom_point(aes(x=EveMins, y=DayMins,color=Churn))

# Guess a line:
# y = 400 - .6x
ggplot(data=churn) +
  geom_point(aes(x=EveMins, y=DayMins,color=Churn)) +
  geom_abline(intercept=385, slope=-0.6)

# Add a flag variable to indicate the bad side of the line
churn$Talkers <- 0
index <- churn$DayMins > 385 - .6*churn$EveMins
churn$Talkers[index] <- 1
# Create two datasets
talkers <- filter(churn, Talkers == 1)
nontalkers <- filter(churn, Talkers == 0)

# Compare the talkers and nontalkers
# scatter
ggplot(data=talkers) +
  geom_point(aes(x=EveMins, y=DayMins,color=Churn))
ggplot(data=nontalkers) +
  geom_point(aes(x=EveMins, y=DayMins,color=Churn))
# Churn bar chart for talkers and then nontalkers
ggplot(data = talkers) +
  geom_bar(mapping = aes(x=Churn)) + 
  coord_flip()
ggplot(data = nontalkers) +
  geom_bar(mapping = aes(x=Churn)) + 
  coord_flip()
# Summaries with percentages
talkers %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))
nontalkers %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))


#
# Cust service calls vs Day Mins
#   Seems like a couple of clumps of churners (upper left, lower right)
ggplot(data=churn) +
  geom_point(aes(x=DayMins, y=CustServCalls,color=Churn))

# Filter out the upper left clump
clump1 <- churn %>%
  filter(CustServCalls>4, DayMins<200)

ggplot(data=clump1) +
  geom_point(aes(x=EveMins, y=DayMins,color=Churn))

ggplot(data=clump1) +
  geom_point(aes(x=DayMins, y=CustServCalls,color=Churn))

# Partition the dataset
churners <- filter(churn, Churn == 'True.')
notchurners <- filter(churn, Churn == 'False.')

# Correlation among predictors
pairs(~churn$DayMins+churn$DayCalls+churn$DayCharge)
# Will discuss the details of this method soon.
fit <- lm(churn$DayCharge~churn$DayMins)
summary(fit)

pairs(~churn$NightMins+churn$NightCalls+churn$NightCharge)

# an add-in library for ggplot2
library(GGally)
ggpairs(select(churn, c('NightMins', 'NightCharge', 'DayMins', 'DayCharge')))

ggpairs(select(churn, c('Churn', 'NightMins', 'DayMins')))

# Big plots -- look at Zoomed version
ggpairs(data= select(churn, c('NightMins','NightCharge', 'DayMins','DayCharge', 'IntlMins', 'IntPlan', 'Churn')))

# Add color based on Churn
ggpairs(data= select(churn, c('NightMins','NightCharge', 'DayMins','DayCharge', 'IntlMins', 'IntPlan', 'Churn')),
          mapping=ggplot2::aes(colour = Churn))


