#
# Ch7-j.R - Modified version of Ch7.R from the DMBA book (https://www.dataminingbook.com/book/r-edition)
#
# 2019-03-18 - Jeff Smith
#
# Libraries
library(caret)
library(FNN)
library (e1071)

#
# Read the data, partition into training and test, create the new instance (test), and
# plot the training data 
#
#### Figure 7.1
mower.df <- read.csv("..//data//RidingMowers.csv")
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])  
valid.index <- setdiff(row.names(mower.df), train.index)  
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]
## new household
new.df <- data.frame(Income = 60, Lot_Size = 20)

## scatter plot
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

#
# Normalize the data
#
#### Table 7.2
# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
# use preProcess() from the caret package to normalize Income and Lot_Size.  Note
# that it's only the training dataset that is used to compute the normalizing 
# constants (mean, std)
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
# to see the computed means
norm.values$mean
# to see the computed std deviations
norm.values$std
# use the prdict function to apply the centering/scaling to the training, validation, 
# and full datasets along with the new observation
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2])
new.norm.df <- predict(norm.values, new.df)

#
# Compute KNN
#
# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 3)
# look at the returned object
nn
# use the returned object to pick the row numbers for the neighbors
row.names(train.df)[attr(nn, "nn.index")]
# look at the neighbors' targets
train.df[row.names(train.df)[attr(nn, "nn.index")],]
# Majority voting would yield "owner" for the test (as shown in the nn object)


#
# Choosing k - Use the Holdout Method and evaluate the accuracy of the classifier
# with different values of k
#
# First show how the knn() function will take predict for multiple values
knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                cl = train.norm.df[, 3], k = 2)
# show the results
knn.pred
# compare to the actual values
valid.norm.df[, 3]
# Finally, the confusion matrix
(cm <- confusionMatrix(knn.pred, valid.norm.df[, 3]))
# look at the cm object in the object view)
# the accuract
cm$overall[1]
# in one step
confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1]

# Now iterate through k = 1, 2, 3, ..., 14 and store the accuracy for each
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                  cl = train.norm.df[, 3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1] 
}
accuracy.df
# 8 appears to be the best value.  

# Now that we have the "best" k, use the full dataset as training
# to make our prediction
# Table 7.4
knn.pred.new <- knn(mower.norm.df[, 1:2], new.norm.df, 
                    cl = mower.norm.df[, 3], k = 8)
knn.pred.new
mower.df[row.names(mower.df)[attr(knn.pred.new, "nn.index")],]
# Note that the vote is 4-4 -- the winner was chosen randomly.
