#
# Example from https://www.theanalysisfactor.com/r-tutorial-glm1/ and
# https://www.theanalysisfactor.com/r-glm-model-fit/
#
# 3/30/2019 - Jeff Smith
#

# Check out the (built-in) dataset
df <- mtcars

# vs - V-engine vs Straight engine (1/0)
model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
summary(model)

# Inference on parameters
# G
with(model, null.deviance - deviance)
# degrees of freedom 
with(model, df.null - df.residual)
# p-value
with(model, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))

# Prediction
newdata = data.frame(wt = 2.1, disp = 180)
predict(model, newdata, type="response")
