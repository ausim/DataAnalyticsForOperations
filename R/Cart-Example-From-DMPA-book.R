#
# CART Example - From the DMPA Book (Chapter 11)
#
# 03/21/2019 - Jeff Smith
#
# Classification Tree with rpart
library(rpart)
library(rpart.plot)

# Create the dataframe
savings <- c('Medium', 'Low',   'High', 'Medium',    "Low", "High", "Low", "Medium")
assets  <- c(  'High', 'Low', 'Medium', 'Medium', 'Medium', 'High', 'Low', 'Medium')
income  <- c(     75,    50,       25,       50,      100,     25,    25,       75 )
crisk   <- c(  'Good', 'Bad',    'Bad',   'Good',   'Good', 'Good', 'Bad',   'Good')
df <- data.frame(savings, assets, income, crisk)

# grow tree 
# with cp at it's default, there is no splitting (only the root node), so 
# set it to 0 to maximize the tree depth.
fit <- rpart(crisk ~ assets+savings+income,
             method="class", data=df, cp=0,minsplit=1)

# A better plot from rpart.plot
prp(fit, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# Numerical results
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree - from the DMPA book
plot(fit, uniform=TRUE, 
     main="Classification Tree for Credit Risk")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Using the default rpart() parameters
fit <- rpart(crisk ~ assets+savings+income,
             method="class", data=df)
prp(fit, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
summary(fit)




