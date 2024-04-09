# Title: wcgs
# Content: practical example of a logistic regression


# Introduction
# We will use the Faraway library
library(faraway)

# The dataset to analyze is wcgs (Western Collaborative Group Study)
data(wcgs)

# Remember we are supposed to know what we are working with
# have a look at
?wcgs

# You can also use the same structure to obtain information about functions
?lm()
?glm()

# Let's see the first observations
head(wcgs)

# The goal is to predict if individuals are going to suffer a heart disease
# In order to account for the probability we first transform the chd variable into a binary
wcgs$chd <- ifelse(wcgs$chd=="yes",1,0)

# Let's see the effect of this transformation
head(wcgs)

# LOGISTIC REGRESSION #
# Model syntax #
# We are now ready to fit our model
mod1 <- glm(chd~age+height+weight+sdp+dbp+chol+behave+cigs+dibep,family=binomial(),data=wcgs)

# Notice the syntax: 
# The first part is our model, the dependent variable and the independent ones
# The second part is the distribution of the observations (Bernouilli for an individual observation and Binomial for the set of all combinations)
# The third part is simply the data

# However the order of family and data is not really important
mod2 <- glm(chd~age+height+weight+sdp+dbp+chol+behave+cigs+dibep,data=wcgs,family=binomial())

# mod2 is identical to mod1
mod1
mod2

# 12 observations are deleted, can we do anything about it?
# Let's check how many there are in total
nrow(wcgs)
# The amount of data is a tiny fraction of the total
12/nrow(wcgs)
# Therefore We can clean the data by eliminating those 12 observations
wcgs <- wcgs[complete.cases(wcgs),]

# Let's fit a new model 
mod3 <- glm(chd~age+height+weight+sdp+dbp+chol+behave+cigs+dibep,data=wcgs,family=binomial())
summary(mod3) 

# Notice that there are singularities that do not allow to fit dibepB, other than that no observations have been deleted now

# STEPWISE VARIABLE SELECTION #
# Wald test #
# We can use the p value of the Wald test to do variable selection
# Check the last column for the pvalue
summary(mod3)
# Using this technique we may do stepwise variable selection

# Confidence interval #
# we can also infer the same information from the confidence interval
confint(mod3)
# height variable, among others, is negligible 

# NESTED MODEL SELECTION #
# Likelihood ratio test #
# Comparison with null model #

# Let us first check the Deviance (remember, related to loglikelihood)
# The deviance for the null model (small omega )
mod3$null.deviance
# The deviance for the model (Capital omega)
mod3$deviance

# If we want to carry out a likelihood ratio test, the statistic is the difference of deviances
lratiostat <- mod3$null.deviance-mod3$deviance


# The statistic follows a chi-squared distribution 
# The degrees of freedom are given by the difference of the degrees of freedom in both models
# The degrees of freedom for the null model are
mod3$df.null
# The degrees of freedom for the model are
mod3$df.residual
# Therefore
dofstat <- mod3$df.null- mod3$df.residual

# The pvalue of the test is given by 
pchisq(lratiostat,df=dofstat,lower.tail = FALSE)

# The pvalue is smaller than 0.05 (usually we use this for the significance)
# Therefore we reject the null hypothesis and we should discard the null model

# Comparison of two nested models #
# We may use the same technique to compare any two nested model
# Let us first define a new model mod31 that does not contain the behave variable
mod31<-glm(chd~age+height+weight+sdp+dbp+chol+cigs,family=binomial(),data=wcgs)

# Now we repeat the same procedure
lratiostat <- mod31$deviance-mod3$deviance
dofstat <- mod31$df.residual-mod3$df.residual
pchisq(lratiostat,df=dofstat,lower.tail = FALSE)
# Given that the p value is small, the null hypothesis is rejected
# Therefore we should select mod3 over mod31

# Exercise
# Is the cholesterol significant if we include it in categories?

# Create a new qualitative variable "cholqual" out of chol with 3 possible values 
# cholqual=good if chol < 200 
# cholqual=regular if 200 < chol < 220
# cholqual=bad if 220 < chol

# Substitute chol with cholqual in the models
# mod32  model with all the variables in mod3 except chol
# mod33  model with all variables in mod3 except chol and add cholqual


# Solution
# I am using the discretize() function but you can do it manually
# install.packages("arules")
library(arules)

# create the new variable
wcgs$cholqual <-discretize(wcgs$chol,method="fixed",breaks=c(min(wcgs$chol),200,220,max(wcgs$chol)),labels=c("good","regular","bad"))
# make sure that everything works
head(wcgs)
# create the new models
mod32 <- glm(chd~age+height+weight+sdp+dbp+behave+cigs+dibep,data=wcgs,family=binomial())
mod33 <- glm(chd~age+height+weight+sdp+dbp+cholqual+behave+cigs+dibep,data=wcgs,family=binomial())

# likelihood ratio tests
lratiostat <- mod32$deviance-mod33$deviance
dofstat <- mod32$df.residual- mod33$df.residual
pchisq(lratiostat,df=dofstat,lower.tail = FALSE) 


# COEFFICIENT INTERPRETATION #
# let's have a look at the coefficients in mod3
mod3$coefficients
# we can obtain the exponentials simply by
exp(mod3$coefficients)
# for continuous covariates:
# an increase of 1 year in age implies multiplies the odds with 1.06
# for qualitative covariates the comparison is done with respect to the ref level:
# behaviour B3 increases the odds by 0.54 compared with behaviour A1
# Therefore it decreases the odds by 0.46

# HOSMER-LEMESHOW TEST
# install the package first
# install.packages("ResourceSelection")
library(ResourceSelection)

# Let's obtain the pvalue 
hoslem.test(wcgs$chd,mod3$fitted.values)
# the first term contains our response variable (what we want to predict)
# the second term contains our model

# Given that the pvalue is large we cannot reject the null hypothesis
# Therefore our model fits the data adequately

# CLASSIFICATION TABLES #
# The table is produced with two variables: data of the response and predictions
# The response is:
response <- wcgs$chd
# In order to compute our predictions we need to fix the cut point
# Let's select cp=0.5
cp<-0.5
# Let us now use the cut point to obtain our predictions
prediction <- ifelse(mod3$fitted.values < cp,0,1) 

# We can now create the table
library(caret)
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab

# Have a look at the table and compute the accuracy, sensitivity and specificity

ac <- (tab$table[1]+tab$table[4])/nrow(wcgs)
sn <- tab$table[4]/(tab$table[3]+tab$table[4])
sp <- tab$table[1]/(tab$table[1]+tab$table[2])
c(ac,sn,sp)  

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificity
library(pROC)
par(pty="s")
Roc <- roc(response,mod3$fitted.values,plot=TRUE,legacy.axes=TRUE,print.auc=TRUE)

# OPTIMAL CUT POINT
# The optimal cut point can be obtained from the point the sensitivity and the specificity curves cross
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="Cut points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.7,y=0.05,labels = "Sensitivity")
text(x=0.7,y=0.95,labels = "Specificity")
abline(v=0.084,col="gray",lty=2)

# Let us now use the value of the new cut point to make predictions
cp<-0.084
prediction <- ifelse(mod3$fitted.values < cp,0,1) 
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab  

# Notice that the accuracy has decreased to 0.7 but the sensitivity and specificity are both above 0.7


# Let's use the new cut point to make predictions
new <- data.frame(age=31, height=1.86, weight=80, sdp=120, dbp=80, chol=200,behave="A2", cigs=4,dibep="A")
# The predicted probability for the new individual is
pred <- predict(mod3, newdata = new,type="response")
# WIll the new individual suffer a heart disease?
pred > cp

pred