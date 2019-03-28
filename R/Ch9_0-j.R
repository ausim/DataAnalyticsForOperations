#
# Ch9_0-j.R - Modified version of Ch9_9.R from the DMBA book (https://www.dataminingbook.com/book/r-edition)
#
# 2019-03-24 - Jeff Smith
#
# Libraries

library(rpart)
library(rpart.plot)
library(caret)

#### Figure 9.7=
mower.df <- read.csv("../data/RidingMowers.csv")

# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    maxdepth = 2, method = "class")
## plot tree
# use prp() to plot the tree. You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  

# Full tree
class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    cp=0, method = "class", minsplit=2)
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  


# Banking dataset - Acceptance of a personal loan
#### Figure 9.9

# Read the data and remove extraneous columns
bank.df <- read.csv("../data/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# partition into training and validation datasets
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# Default classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# Deeper classification tree
#### Figure 9.10
# cp is the complexity parameter - "any split that does not decrease 
# the overall lack of fit by a factor of cp is not attemplted."
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  


#### Table 9.3

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
# Default tree
# Confusion matrix for training data
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Personal.Loan))

# Confusion matrix for validation dataset
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))


# deeper tree
# Training dataset
deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(deeper.ct.point.pred.train, as.factor(train.df$Personal.Loan))

# Validation dataset
deeper.ct.point.pred.valid <- predict(deeper.ct,valid.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))



#### Table 9.4

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
# use printcp() to print the table. 
printcp(cv.ct)
# show the tree
prp(cv.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(cv.ct$frame$var == "<leaf>", 'gray', 'white'))  



#### Figure 9.12

# prune by lower cp using the cp value for the min xerror from above
pruned.ct <- prune(cv.ct, 
    cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10,  
  box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))  



#### Figure 9.13
# best pruned
set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)  
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a 
#regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" 
# stands for "cross") is a scaled version of overall average of the 
# 5 out-of-sample errors across the 5 folds.
pruned.ct <- prune(cv.ct, cp = 0.0068729)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 
# Result is the same tree that rpart() geneates by default.

# Default classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)


# ---------------------------

####Figure 9.15

library(randomForest)
## random forest
rf <- randomForest(as.factor(Personal.Loan) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$Personal.Loan))


#### Table 9.5

library(adabag)

train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)

set.seed(1)
boost <- boosting(Personal.Loan ~ ., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$Personal.Loan))
