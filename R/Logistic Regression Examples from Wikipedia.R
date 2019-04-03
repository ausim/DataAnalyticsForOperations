#
# Logistic regression example from https://en.wikipedia.org/wiki/Logistic_regression
#
# 3/30/2019 - Jeff Smith
#

# Initial Example - Disease ~ Age
# Create the dataframe
students <- data.frame(
  hours = c(0.5, 0.75, 1.0, 1.25, 1.50, 1.75, 1.75, 2, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 4, 4.25, 4.5, 4.75, 5, 5.5),
  pass  = c(  0,    0,   0,    0,    0,   0,     1, 0,    1,   0,    1,   0,    1,   0, 1,    1,   1,    1, 1,   1)
)
# Logistic regression
lrl <- glm(pass~hours, data=students, family=binomial)
# Plot the observations and the models
plot(students$hours, students$pass,
     xlab="Hours", ylab="Pass",
     main="Pass vs. Hours",
     xlim=c(0, 6), pch=16)
curve(predict(lrl, data.frame(hours=x), type="resp"),
      add=TRUE, lwd=2)

summary(lrl)

# Inference on parameters
with(lrl, null.deviance - deviance)
with(lrl, df.null - df.residual)
with(lrl, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))

# Make predictions
newd <- with(students, data.frame(hours=c(2.5)))
# log odds (probabilities on logit scale)
predict.glm(lrl, newdata=newd)
# probabilities
predict.glm(lrl, newdata=newd, type="resp")


# Odds Ratios
round(exp(coef(lrl)[2]),3)
