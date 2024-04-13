#install.packages("pacman")
pacman::p_load(
  tidyverse,
  dplyr,
  ResourceSelection, # for Hosmer-Lemeshow test
  caret, # for confusionMatrix
  pROC # for roc
)

#Firts Model: Outcome
# Load the data
d3 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")

# We delete outliers (explained in Report 1)
d3<- d3 %>% filter(BloodPressure > 0,BMI > 0, Glucose > 0, Insulin > 0, SkinThickness > 0)

# Let's see the first observations
head(d3)
#Count the number of observations of Outcome
table(d3$Outcome)
#Outcome: 0 = No diabetes, 1 = Diabetes


# LOGISTIC REGRESSION
# First model
mod1 <- glm(Outcome ~ Pregnancies+Glucose+BloodPressure+SkinThickness+
              Insulin+BMI+Age+DiabetesPedigreeFunction, data = d3, family = binomial())
mod1 #No observation is deleted
#Performing Wald Tests
summary(mod1)
#BloodPressure, SkinThickness and Insulin are not significant, because p-value > 0.05
#They are negligible

# Confidence intervals
confint(mod1)

# Model selection
mod2 <- glm(Outcome ~ Glucose+BMI+DiabetesPedigreeFunction+Age, data = d3, family = binomial())

# Likelihood ratio test to compare mod1 and mod2
lambda <- mod2$deviance - mod1$deviance
dof <- mod2$df.residual - mod1$df.residual
pchisq(lambda, df = dof, lower.tail = FALSE)
# Given that the p value is big, the we cannot reject the null hypothesis
# Therefore we should select mod2 over mod1

#Now another test is going to be conducted to study the inclusion of Age
summary(mod2)
#Regarding Wald Test, Age is signicant in the model
#Let's see the results of the Likelihood Ratio Test, comparing mod2 and mod3
#First we build mod3 not including age
mod3 <- glm(Outcome ~ Glucose+BMI+DiabetesPedigreeFunction, data = d3, family = binomial())
lambda <- mod3$deviance - mod2$deviance
dof <- mod3$df.residual - mod2$df.residual
pchisq ( lambda ,df= dof , lower.tail = FALSE )
# Given that the p value is small, we can reject the null hypothesis
# Therefore we should select mod2 over mod3

#In conclusion we should include Age in the model

# COEFFICIENT INTERPRETRATION
# A look to the coefficients
mod2$coefficients

# HOSMER-LEMESHOW TEST
hoslem.test(d3$Outcome,mod2$fitted.values)
# Given that the pvalue is large we cannot reject the null hypothesis
# Therefore our model fits the data adequately

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificity
response <- d3$Outcome
par(pty="s")
Roc <- roc(response,mod2$fitted.values,plot=TRUE,legacy.axes=TRUE,print.auc=TRUE)
optimal_cut <- coords(Roc, "best", best.method = "closest.topleft")


# OPTIMAL CUT POINT
# The optimal cut point can be obtained from the point the sensitivity and the specificity curves cross
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="Cut points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.7,y=0.05,labels = "Sensitivity")
text(x=0.7,y=0.85,labels = "Specificity")
abline(v=0.315,col="red",lty=2)
cp = 0.315

# Let us now use the value of the new cut point to make predictions
prediction <- ifelse(mod2$fitted.values < cp,0,1) 
# We can now create the confusion matrix to study accuracy, sensitivity and specificity
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab  
#Accuracy = 0.7857
#Sensitivity = 0.7462
#Specificity = 0.8053

#Deletion of the variables used before in order not make mistakes
rm(list=ls())


#------------------------------------------------------------------------------------------------------------
#Second Model: PhysActivity

# Load the data
d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")

# Let's see the first observations
head(d1)

#First Model
mod1 <- glm(PhysActivity ~ Diabetes_012+Stroke+HvyAlcoholConsump+Fruits+Veggies
            +HighBP+HighChol, data = d1, family = binomial())

mod1 #No observation is deleted

#Performing Wald Tests
summary(mod1)
#All variables of the model are significants

# Confidence intervals
confint(mod1)


# COEFFICIENT INTERPRETRATION
# A look to the coefficients
mod1$coefficients

# HOSMER-LEMESHOW TEST
hoslem.test(d1$PhysActivity,mod1$fitted.values)
# Given that the pvalue is large we cannot reject the null hypothesis
# Therefore our model fits the data adequately

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificityç
response <- d1$PhysActivity
par(pty="s")
Roc <- roc(response,mod1$fitted.values,plot=TRUE,legacy.axes=TRUE,print.auc=TRUE)

# OPTIMAL CUT POINT
# The optimal cut point can be obtained from the point the sensitivity and the specificity curves cross
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="Cut points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.4,y=0.95,labels = "Sensitivity")
text(x=0.4,y=0.09,labels = "Specificity")
abline(v=0.784,col="red",lty=2)
cp<-0.784


# Let us now use the value of the new cut point to make predictions
prediction <- ifelse(mod1$fitted.values < cp,0,1) 
# We can now create the confusion matrix to study accuracy, sensitivity and specificity
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab  
#Accuracy = 0.6225          
#Sensitivity = 0.6308                   
#Specificity = 0.5966          

#Deletion of the variables used before in order not make mistakes
rm(list=ls())

#------------------------------------------------------------------------------------------------------------
#Third Model: Smoker

# Load the data
d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")

# Let's see the first observations
head(d1)

#First Model
mod1 <- glm(Smoker ~ Diabetes_012+Stroke+HvyAlcoholConsump+Fruits+Veggies
            +HighBP+HighChol, data = d1, family = binomial())

mod1 #No observation is deleted

#Performing Wald Tests.
summary(mod1)
#All variables of the model are significants

# Confidence intervals
confint(mod1)


# COEFFICIENT INTERPRETRATION
# A look to the coefficients
mod1$coefficients

# HOSMER-LEMESHOW TEST
hoslem.test(d1$Smoker,mod1$fitted.values)
# Given that the pvalue is large we cannot reject the null hypothesis
# Therefore our model fits the data adequately

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificity
response <- d1$Smoker
par(pty="s")
Roc <- roc(response,mod1$fitted.values,plot=TRUE,legacy.axes=TRUE,print.auc=TRUE)


# OPTIMAL CUT POINT
# The optimal cut point can be obtained from the point the sensitivity and the specificity curves cross
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="Cut points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.7,y=0.13,labels = "Sensitivity")
text(x=0.7,y=0.9,labels = "Specificity")
abline(v=0.412,col="red",lty=2)
cp = 0.412

# Let us now use the value of the new cut point to make predictions
prediction <- ifelse(mod1$fitted.values < cp,0,1) 
# We can now create the confusion matrix to study accuracy, sensitivity and specificity
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab  
#Accuracy = 0.5698          
#Sensitivity = 0.5798
#Specificity = 0.5619
