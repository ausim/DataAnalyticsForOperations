#
# Logistic regression example from DMPA Book
#
# 3/30/2019 - Jeff Smith
#

# Initial Example - Disease ~ Age
# Create the dataframe
patients <- data.frame(
  age = c(25, 29, 30, 31, 32, 41, 41, 42, 44, 49, 50, 59, 60, 62, 68, 72, 79, 80, 81, 84),
  disease = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1)
)
# Linear regression - for comparison
lml <- lm(disease~age, data=patients)
# Logistics regression
lrl <- glm(disease~age, data=patients, family=binomial)
# Plot the observations and the models
plot(patients$age, patients$disease,
     xlab="Age", ylab="Disease",
     main="Disease vs. Age",
     xlim=c(20, 90), pch=16)
abline(lml, lty=3)
curve(predict(lrl, data.frame(age=x), type="resp"),
      add=TRUE, lwd=2)
legend("topleft",legend=c("LS","Log."),
       lty=c(3,1), cex=.9)

summary(lml)
summary(lrl)


# Make predictions
newd <- with(patients, data.frame(age=c(50, 72)))
# log odds (logit units)
predict.glm(lrl, newdata=newd)
# probabilities
predict.glm(lrl, newdata=newd, type="resp")


# Inference on parameters
# G
with(lrl, null.deviance - deviance)
# dof
with(lrl, df.null - df.residual)
# p-value
with(lrl, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))

# Odds Ratios
round(exp(coef(lrl)),3)


#
# Dichotomous example - Churn dataset
# read data file
churn <- read.csv(file="../data/churn.txt", stringsAsFactors = TRUE)
# show churn/vmail table
table(churn$Churn, churn$VMailPlan)
# create binary variable
churn$VMP.ind <- ifelse(churn$VMailPlan=='yes', 1, 0)
# regress
lr2 <- glm(Churn~VMP.ind,data=churn,family=binomial)
summary(lr2)


# Inference on parameters
# G
with(lr2, null.deviance - deviance)
# dof
with(lr2, df.null - df.residual)
# p-value
with(lr2, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))

# odds ratio
round(exp(coef(lr2)),3)
# Make predictions
newd <- with(churn, data.frame(VMP.ind=c(0,1)))
predict.glm(lr2,newdata=newd)
predict.glm(lr2,newdata=newd, type="resp")


#
# Polychotomous Example
#
churn <- read.csv(file="../data/churn.txt", stringsAsFactors = TRUE)
churn$CSC <- factor(churn$CustServCalls)
levels(churn$CSC)
# Set 0, 1 to be low
levels(churn$CSC)[1:2] <- "Low"
levels(churn$CSC)
# Set 2, 3 to be medium
levels(churn$CSC)[2:3] <- "Medium"
levels(churn$CSC)
# Set 4-9 to be high
levels(churn$CSC)[3:8] <- "High"
# set the two indicator variables (low is the baseline)
churn$CSC_Med <- ifelse(churn$CSC == "Medium", 1, 0)
churn$CSC_Hi <- ifelse(churn$CSC == "High", 1, 0)
table(churn$Churn, churn$CSC)
lr3 <- glm(Churn ~ CSC_Med + CSC_Hi, data = churn, family=binomial)
summary(lr3)

# odds ratio
round(exp(coef(lr3)),3)

# G
with(lr3, null.deviance - deviance)
# dof
with(lr3, df.null - df.residual)
# p-value
with(lr3, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))
# CSC_Med does not appear to be significant (p-value = .75)

#
# Combine low and medium to use 2 levels 
# Back to a diachatomous model, but with the new CSC_Hi factor.
churn <- read.csv(file="../data/churn.txt", stringsAsFactors = TRUE)
churn$CSC <- factor(churn$CustServCalls)
levels(churn$CSC)
# Set 0, 1, 2, 3 to be low
levels(churn$CSC)[1:4] <- "Low"
levels(churn$CSC)
# set 4-9 to be high
levels(churn$CSC)[2:7] <- "High"
levels(churn$CSC)
# Create the indicator
churn$CSC_Hi <- ifelse(churn$CSC == "High", 1, 0)
table(churn$Churn, churn$CSC)
lr4 <- glm(Churn ~ CSC_Hi, data = churn, family=binomial)
summary(lr4)

# odds ratio
round(exp(coef(lr4)),3)

# G
with(lr4, null.deviance - deviance)
# dof
with(lr4, df.null - df.residual)
# p-value
with(lr4, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))


#
# Continuous Example
#
lr5 <- glm(Churn ~ DayMins, data = churn, family=binomial)
summary(lr5)

# odds ratio
round(exp(coef(lr5)),3)

# G
with(lr5, null.deviance - deviance)
# dof
with(lr5, df.null - df.residual)
# p-value
with(lr5, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))

# 
# Multiple Logistic Regression
#
churn$IntlPind <- ifelse(churn$IntPlan =="yes",1,0)
churn$VMPind <- ifelse(churn$VMailPlan == "yes", 1,0)
lr6 <- glm(Churn ~
             IntlPind +
             VMPind +
             DayMins +
             EveMins +
             NightMins +
             IntlMins,
           data=churn, family=binomial)
summary(lr6)

# odds ratio
round(exp(coef(lr6)),3)

# G
with(lr6, null.deviance - deviance)
# dof
with(lr6, df.null - df.residual)
# p-value
with(lr6, pchisq(null.deviance-deviance, 
                 df.null-df.residual,
                 lower.tail=FALSE))

# Make predictions
newd <- with(churn, data.frame(
  IntlPind = c(0), 
  VMPind=c(0),
  DayMins=c(225),
  EveMins=c(100),
  NightMins=c(55),
  IntlMins=c(27)))
predict.glm(lr6,newdata=newd)
predict.glm(lr6,newdata=newd, type="resp")

