# Title: Gala
# Content: This is the practical example of linear regressions with the Faraway library 
# In this example we go trough all the techniques studied in the theory sessions

# Introduction
# we will use the faraway library
library(faraway)

# we will work with the Gala dataset
# Gala dataset: number of species of plants in the Galapago islands
data(gala)

# lets get a glimpse of the dataset
head(gala)

# describe each var:
# species: number of species of plants
# endemics: species exclusive from the island
# area: area of the island
# elevation: highest elevation 
# nearest: distance to nearest island
# scruz: distance to santa cruz island
# adjacent: area of the adjacent island

# the dataset covers 30 observations (islands) and 7 variables
dim(gala)
# therefore m=30 and n=7-1 (at the most)

# ------------------------------------------------------------------------------  

# Model (typing)
# let us define our model
# the number of species is the dependent variable y
# the rest of variables are independent. 
# we will not include the endemics variable in our model

# therefore the independent variables are
x <- model.matrix(~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)

# and the dependent variable is
y <- gala$Species

# now we apply the formula for the estimation of beta
# t is the transpose
# %*% is the matrix multiplication
# and solve yields the inverse
solve(t(x)%*%x)%*%t(x)%*%y

# ------------------------------------------------------------------------------

# Model (library)
# let us now see how to use the Faraway library 
# use the lm() command for linear models
mod1 <- lm(Species~Area+Elevation+Nearest+Scruz+Adjacent, data=gala)

# use residuals() for the residuals (difference between data and prediction)
residuals(mod1)

# use fitted() for the prediction of the model
fitted(mod1)

# use deviance() for the residual sum of squares
deviance(mod1)

# use df.residual() for the degrees of freedom
df.residual(mod1)
# remember this should be m-(n+1)
30-(5+1)

# and use coef() for the estimation of the parameters
coef(mod1)

# the model contains more information that we may extract
names(mod1)

# we may be interested in the command summary() for a summary of the results
summary(mod1)

# the summary provides access to even more information
mod1s <- summary(mod1)
names(mod1s)

# for instance we can obtain R^2 from the summary
mod1s$r.squared
# which can also be computed with the corresponding formula
1-deviance(mod1)/sum((y-mean(y))^2)

# ------------------------------------------------------------------------------

# Inference  
# Variances and standard deviations of the errors and parameters given their models
# We can use the summary it to compute sigma (standard deviation of errors)
mod1s$sigma
# which is also available via the residual sum of squares and the degrees of freedom
sqrt(deviance(mod1)/df.residual(mod1)) 

# we can also compute the standard deviation of the parameters with the summary
mod1s$coef[,2] 
# which are also available with the corresponding formula
sqrt(diag(solve(t(x)%*%x)*mod1s$sigma^2))

# Confidence intervals
# Confidence interval for the parameters
# we can obtain the boundaries with confint for a value of alpha=0.05
confint(mod1)

# we may also compute them from the formula
mod1s$coefficients[,1]+mod1s$coefficients[,2]*qt(0.025,df.residual(mod1))
mod1s$coefficients[,1]-mod1s$coefficients[,2]*qt(0.025,df.residual(mod1))

# Confidence interval for the predictions
# Prediction in an island with x0=(0.08,93,6,12,0.34)

# we define the new data
x0 <- data.frame(Area=0.08, Elevation=93, Nearest=6.0, Scruz=12, Adjacent=0.34)

# Prediction of a single observation
predict(mod1,x0,interval="prediction")
# Prediction of an ensemble of observations 
predict(mod1,x0,interval="confidence")


# we can also do these calculations directly
# let us first introduce the values
x0 <- c(1,0.08,93,6.0,12.0,0.34)
# let us now obtain our prediction
y0 <- sum(x0*coef(mod1))
y0

# The prediction of a single observation would be:
# boundaries for alpha=0.05
y0+qt(0.025,df.residual(mod1))*mod1s$sigma*sqrt(1+t(x0)%*%solve(t(x)%*%x)%*%x0)
y0-qt(0.025,df.residual(mod1))*mod1s$sigma*sqrt(1+t(x0)%*%solve(t(x)%*%x)%*%x0)

# Prediction for an ensemble of observations would be:
# boundaries for alpha=0.05
y0+qt(0.025,df.residual(mod1))*mod1s$sigma*sqrt(t(x0)%*%solve(t(x)%*%x)%*%x0)
y0-qt(0.025,df.residual(mod1))*mod1s$sigma*sqrt(t(x0)%*%solve(t(x)%*%x)%*%x0)

# ------------------------------------------------------------------------------

# Model Selection

# Test of all predictors (F test) 
# Check the F-statistic and the p-value
summary(mod1)
# we are looking at the last line
# we get a tiny pvalue
# therefore the null hypothesis is rejected
# and therefore there is at least a parameter different from 0

# We can compute the pvalue step by step
# TSS: Total sum of squares (sum of squared errors for the null model)
TSS <- sum((gala$Species-mean(gala$Species))^2)
# RSS: Residual sum of squares (sum of squared errors for the general model)
RSS <- deviance(mod1)

# F statistic
Fstat <- (TSS-RSS)*df.residual(mod1)/(RSS*(dim(x)[2]-1))
Fstat

# Now we have to compute P(F>Fstat) to obtain the pvalue
pf(Fstat,dim(x)[2]-1,df.residual(mod1),lower.tail = FALSE)

# The fact that the null hypothesis is rejected means that the model with 5 variables is better than the model given by the average


# Test of a single predictor (T test)
# Check the p value for each predictor
# We are looking at the last column
summary(mod1)
# high pvalues mean that the null hypothesis is not rejected
# therefore the parameter is 0
# therefore the independent variable is not relevant for the model

# We can also derive this from the expressions in the theory
Tstat <- mod1s$coefficients[,1]/mod1s$coefficients[,2]
# this computes the estimator divided by the standard error, i.e., the statistic
Tstat

# the pvalue corresponds to P(|t|>Tstat)
2*pt(abs(Tstat),mod1$df.residual,lower.tail = FALSE)

# Comparison between 2 models with Ftest
# let us create a new model that does not contain the area
mod2 <- lm(Species~Elevation+Nearest+Scruz+Adjacent,data=gala)
# and let's compare it with mod1, the one with the area
Fstat <- (deviance(mod2)-deviance(mod1))*df.residual(mod1)/deviance(mod1)
Fstat
# let us now compute the pvalue
pf(Fstat,1,df.residual(mod1),lower.tail = FALSE)
# The p value is large, the null hypothesis cannot be rejected
# Therefore there is no evidence to add more parameters
# Therefore it's better not to include the area in our model

# -----------------------------------------------------------------------------    

# Model diagnosis    
# General info
# model diagnosis can be carried out visually by plotting the model
plot(mod1)
# the command produces 4 plots: 
# residuals/predictions, 
# qqplot of the standarized residuals,
# sqrt(standarized residuals)/predictions
# standarized residuals/leverage

# standarized residuals are the residuals divided by the standard deviation

# Heteroscedasticity and Linearity
plot(mod1,1)
# in the first plot there is a linear pattern for the residuals
# a good way to address this is transform the dependent variable
# we will transform to the square root:
# This is our new model
mod11 <- lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent, data=gala)

# let us see the effect in the residuals
plot(fitted(mod11),residuals(mod11),xlab="Fitted values",ylab="Residuals",main="Square root of Species response")
# the residuals are now evenly distributed

# let us plot the first plot for the new model
plot(mod11,1)
# the linear pattern is no longer present
# Therefore the model is homoscedastic

# Normality
plot(mod11,2)
# Most points are close to the line, and the plot is symmetric.
# The distribution is short tailed, but the approximation is valid.

# Unusual observations
plot(mod11,4)
# Isabella is a clear influential point
plot(mod11,5)
# points in the right have high leverage (fernandina and isabella)
# points far from the centre line have large residuals (isabella)
# points beyond the dashed line have high cook's distance (isabella)
# we could consider excluding isabella

# This is our new model without Isabella
mod12 <- lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent,data=gala[-which(rownames(gala)=="Isabela"),])
# let us compare the coefficients of both models
coefcomp <- rbind(mod11$coefficients,mod12$coefficients)
rownames(coefcomp) <- c("Isabela","No Isabela")
coefcomp
# notice that the area is positive for the number of species without isabella, and negative with isabella

# given that Isabella was an influential point, its removal is expected to increase R^2
summary(mod11)$r.squared
summary(mod12)$r.squared
# Indeed


# -----------------------------------------------------------------------------    

# Model construction
# Let us first select alpha=0.15 as the significance

# Backward elimination
# Notice the star system for denoting the pvalue

# Step 1
summary(mod12)
# The distance to the nearest island has the highest pvalue higher than the critical alpha
# Let us create a new model 20 without "Nearest"
mod20 <- lm(sqrt(Species)~Area+Elevation+Scruz+Adjacent,data=gala[-which(rownames(gala)=="Isabela"),])

# Step 2
summary(mod20)
# Notice that the value of R squared is almost the same in the new model even if it has one variable less
# The next highest pvalue is given by the "Scruz" variable
# Let us create a new model 21 without "Scruz"
mod21 <- lm(sqrt(Species)~Area+Elevation+Adjacent,data=gala[-which(rownames(gala)=="Isabela"),])

# Step 3
summary(mod21)
# No more variables have a pvalue larger than 0.15, therefore we consider that the variables that remain should be in the model

# Diagnosis
# If we perform a diagnosis we will realize that the Fernandina island is an influential point.
plot(mod21,4)
plot(mod21,5)

# Let us do a model without Fernandina
mod22 <- lm(sqrt(Species)~Area+Elevation+Adjacent,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
summary(mod22)

# We start a new backward elimination run
# Step 1  
summary(mod22)
# It turns out that the variable Adjacent is no longer relevant
mod23 <- lm(sqrt(Species)~Area+Elevation,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])

# Step 2
summary(mod23)
# No more variables can be removed

# Diagnosis
plot(mod23,4)
# There are no influential points

# The final result is a model in which the number of species can be predicted only based on the area and elevation
summary(mod23)
# R^2 yields a value of 0.8059 even after removing 3 variables.

# Forward selection #
# Step 1
# Let us start with all possible models that only include a single variable
mod30 <- lm(sqrt(Species)~Area,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod31 <- lm(sqrt(Species)~Elevation,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod32 <- lm(sqrt(Species)~Nearest,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod33 <- lm(sqrt(Species)~Scruz,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod34 <- lm(sqrt(Species)~Adjacent,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])


# let us now compare the pvalues
# this prints the pvalue of the second coefficient (the first one is the intercept)
c(summary(mod30)$coefficients[2,4],
  summary(mod31)$coefficients[2,4],
  summary(mod32)$coefficients[2,4],
  summary(mod33)$coefficients[2,4],
  summary(mod34)$coefficients[2,4])
# The elevation has the smallest pvalue.

# Step 2
# Let us now create a new family of models including the elevation and one variable more
mod35 <- lm(sqrt(Species)~Elevation+Area,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod36 <- lm(sqrt(Species)~Elevation+Nearest,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod37 <- lm(sqrt(Species)~Elevation+Scruz,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod38 <- lm(sqrt(Species)~Elevation+Adjacent,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])

# let us now compare the pvalues
# this prints the pvalue of the third coefficient
c(summary(mod35)$coefficients[3,4],
  summary(mod36)$coefficients[3,4],
  summary(mod37)$coefficients[3,4],
  summary(mod38)$coefficients[3,4])
# The area has the smallest pvalue and should be included in the model

# Step 3
# Let us create a new family of models with the elevation,area and one of the remaining variables
mod39 <- lm(sqrt(Species)~Elevation+Area+Nearest,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod310 <- lm(sqrt(Species)~Elevation+Area+Scruz,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])
mod311 <- lm(sqrt(Species)~Elevation+Area+Adjacent,data=gala[-which(rownames(gala)=="Isabela"|rownames(gala)=="Fernandina"),])

# let us now compare the pvalues
# this prints the pvalue of the fourth coefficient
c(summary(mod39)$coefficients[4,4],
  summary(mod310)$coefficients[4,4],
  summary(mod311)$coefficients[4,4])

# None of the new variables has a pvalue smaller than 0.2. We shouldn't include any of them in the model.

# Therefore, our best model, according to the forward selection contains Elevation and Area. 
# The result is consistent with the result in the backward elimination.

# AIC and BIC #
# Let us analyze if our model selection is compatible with AIC and BIC
# in order to do that we will compute AIC and BIC for each of the models we have fitted

# AIC  
AIC(mod12,mod20,mod21,mod22,mod23)
# Model 23 yields the minimal AIC, therefore the result is consistent with the backward elimination

# BIC  
BIC(mod12,mod20,mod21,mod22,mod23)
# Model 23 also yields the minimal BIC, and thus the result is consistent with the backward elimination and the AIC

# Adjusted R^2#
c(summary(mod12)$adj.r.squared,
  summary(mod20)$adj.r.squared,
  summary(mod21)$adj.r.squared,
  summary(mod22)$adj.r.squared,
  summary(mod23)$adj.r.squared
)
# Model 23 also has the highest value of adjusted R^2 out of the ones we have tested
# Our results are consistent