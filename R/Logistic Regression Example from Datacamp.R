#
# Example from https://www.datacamp.com/community/tutorials/logistic-regression-R
#
# 3/30/2019 - Jeff Smith
#
library(ISLR)
library(corrplot)
library(caret)


# Stock market dataset
# Store the dataframe so that we can see the whole thing
df <- Smarket
# Explore
names(Smarket)
head(Smarket)
summary(Smarket)

# Visualization of the data
# set the graphic parameters to show 8 histograms in a row
par(mfrow=c(1,8))
for(i in 1:8) {
  hist(Smarket[,i], main=names(Smarket)[i])
}

par(mfrow=c(1,8))
for(i in 1:8) {
  boxplot(Smarket[,i], main=names(Smarket)[i])
}

par(mfrow=c(1,1))
correlations <- cor(Smarket[,1:8])
corrplot(correlations, method="circle")

pairs(Smarket, col=Smarket$Direction)

x <- Smarket[,1:8]
y <- Smarket[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


# Logistics Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)

# Inference on parameters
# G
with(glm.fit, null.deviance - deviance)
# dof
with(glm.fit, df.null - df.residual)
# p-value
with(glm.fit, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))


glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]


glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

attach(Smarket)
table(glm.pred,Direction)

mean(glm.pred == Direction)


train = Year<2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, 
               family = binomial, 
               subset = train)

glm.probs <- predict(glm.fit, 
                     newdata = Smarket[!train,], 
                     type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)


# Fit a smaller model
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)

summary(glm.fit)




