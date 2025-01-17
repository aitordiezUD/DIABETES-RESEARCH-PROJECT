# Check if the following packages are installed, if not install them
#install.packages("pacman")
pacman::p_load(
  tidyverse,
  dplyr,
  ResourceSelection, # for Hosmer-Lemeshow test
  caret, # for confusionMatrix
  pROC # for roc
)

#=================================================
#LINEAR REGRESSIONS
#=================================================

# Clear the workspace
rm(list = ls())

# Read the dataset
d3 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")

# Turn the dataset into a dataframe
df3 <- as.data.frame(d3)

# Filter to avoid 0 values
df3 <- df3 %>% filter(Glucose != 0, BloodPressure != 0, SkinThickness != 0, Insulin != 0, BMI != 0, DiabetesPedigreeFunction != 0, Age != 0)

# Model selection ----------------------------------------------------------------------------------------------------------------------------

# Model Construction (Glucose Prediction)

mod1 <- lm(Glucose ~ BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = df3)
mod2 <- lm(Glucose ~ BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction, data = df3)

# Lets compute the FStat
Fstat <- (deviance(mod2)-deviance(mod1))*df.residual(mod1)/deviance(mod1)

# Now compute the p-value
pf(Fstat,1,df.residual(mod1),lower.tail = FALSE)

# The p_value is 6.467824e-06 so we reject the null hypothesis and we can say that the variable Age is significant
mod3 <- lm(Glucose ~ BloodPressure + SkinThickness + Insulin + BMI + Age, data = df3)

# Lets compute the FStat
Fstat2 <- (deviance(mod3)-deviance(mod1))*df.residual(mod1)/deviance(mod1)

# Now compute the p-value
pf(Fstat2,1,df.residual(mod1),lower.tail = FALSE)

# The p_value is 0.25 so we can say the variable DiabetesPedigreeFunction is not significant
# The best model for the moment is mod3
# Lets build a simpler model and check if it is better than mod3
mod4 <- lm(Glucose ~ BloodPressure + Insulin + BMI + Age, data = df3)

Fstat3 <- (deviance(mod4)-deviance(mod3))*df.residual(mod3)/deviance(mod3)

# Now compute the p-value
pf(Fstat3,1,df.residual(mod3),lower.tail = FALSE)

# The p_value is 0.58 so we can say the variable SkinThickness is not significant
# The best model for the moment is mod4

mod5 <- lm(Glucose ~ BloodPressure + Insulin + Age, data = df3)

# Lets compute the FStat
Fstat4 <- (deviance(mod5)-deviance(mod4))*df.residual(mod4)/deviance(mod4)

# Now compute the p-value
pf(Fstat4,1,df.residual(mod4),lower.tail = FALSE)

# The p-value is 0.2 so BMI is not significant

#----------------------------------------------------------------------------------------------------------------------------

# Model diagnosis
# Heteroscedasticity and linearity
plot(mod5,1)


#Lets check for the plots of mod2 and mod4
plot(mod5,1)
plot(mod2, 1)

# They do not change a lot so we can say that the model is linear

# Lets try with a transformation of the dependent variable
mod6 <- lm(log(Glucose) ~ BloodPressure + Insulin + Age, data = df3)

# Now plot to compare
plot(mod5,1)
plot(mod6,1)

# There is not a big change so there is no need to transform the dependent variable

# Now lets study the normality
plot(mod5,2)
# Most plots are close to the line so we can say that the residuals are normally distributed

# Unsual observations
plot(mod5,4)
plot(mod5,5)
# Points far from the line have large residuals (302, 226)
# Point in the right have high leverage (5)
# Lets remove the observations where glucose is 302 and 206
mod7 <- lm(Glucose ~ BloodPressure + Insulin + Age, data = df3 %>% filter(Glucose != 5))

coefcomp <- rbind(mod5$coefficients, mod7$coefficients)
coefcomp

# Evaluating scores ----------------------------------------------------------------------------
# AIC
AIC(mod7,mod6,mod5,mod4,mod3,mod2,mod1)
# BIC
BIC(mod7,mod6,mod5,mod4,mod3,mod2,mod1)

# Model 4 has the lowest AIC and BIC so we are right with the model selection
 
c(summary(mod1)$adj.r.squared,
  summary(mod2)$adj.r.squared,
  summary(mod3)$adj.r.squared,
  summary(mod4)$adj.r.squared,
  summary(mod5)$adj.r.squared,
  summary(mod6)$adj.r.squared,
  summary(mod7)$adj.r.squared
)

# Model 4 has also the highest adjusted R squared so we are right with the model selection


# Model selection (BMI Prediction) ----------------------------------------------------------------------------------
# Model Construction

mod11 <- lm(BMI ~ Glucose + BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction + Age, data = df3)
mod12 <- lm(BMI ~ Glucose + BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction, data = df3)

# Lets compute the FStat
Fstat11 <- (deviance(mod12)-deviance(mod11))*df.residual(mod11)/deviance(mod11)

# Now compute the p-value
pf(Fstat11,1,df.residual(mod1),lower.tail = FALSE)

# The p_value is less than 0.05 so we reject the null hypothesis and we can say that the variable Age is significant
mod13 <- lm(BMI ~ Glucose + BloodPressure + SkinThickness + Insulin + Age, data = df3)

# Lets compute the FStat
Fstat12 <- (deviance(mod13)-deviance(mod11))*df.residual(mod11)/deviance(mod11)

# Now compute the p-value
pf(Fstat12,1,df.residual(mod1),lower.tail = FALSE)

# The p_value is 0.12 so we can say the variable DiabetesPedigreeFunction is not significant

mod14 <- lm(BMI ~ BloodPressure + SkinThickness + Insulin + Age, data = df3)

# Lets compute the FStat
Fstat13 <- (deviance(mod14)-deviance(mod13))*df.residual(mod13)/deviance(mod13)

# Now compute the p-value
pf(Fstat13,1,df.residual(mod1),lower.tail = FALSE)

# The p_value is 0.53 so Glucose is not significant

mod15 <- lm(BMI ~ SkinThickness + Insulin + Age, data = df3)

# Lets compute the FStat
Fstat14 <- (deviance(mod15)-deviance(mod14))*df.residual(mod14)/deviance(mod14)

# Now compute the p-value
pf(Fstat14,1,df.residual(mod1),lower.tail = FALSE)

# The p_value is less than 0.05 so blood pressure is significant

# Model diagnosis ----------------------------------------------------
# Hetereoscedasticity and linearity
plot(mod15,1)

# Lets check for the models 15 and 12
plot(mod15,1)
plot(mod12,1)

# There is not a big change so the model is linear

# Lets try with a transformation of the dependent variable
mod16 <- lm(sqrt(BMI) ~ SkinThickness + Insulin + Age, data = df3)
plot(mod16,1)
plot(mod15,1)
# There is no need of a change of variable

# Normality
plot(mod15,2)
# Most points are close to the line so we can say that the residuals are normally distributed

# Unsual observations
plot(mod15,4)
plot(mod15,5)
# Points far from the line have large residuals (87)
# Cook's distances are not high enough to be considering harmful for the model

#-------------------------------------------------------------------------

# Evaluating scores
# AIC
AIC(mod17,mod15,mod14,mod13,mod12,mod11)
# BIC
BIC(mod17,mod15,mod14,mod13,mod12,mod11)
# In both AIC and BIC the lowest scores are from mod15 so we are right with the model selection

# Model selection --------------------------------------------------------
# Model construction (BloodPressure Prediction)
mod21 <- lm(BloodPressure ~ Glucose + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = df3)
mod22 <- lm(BloodPressure ~ Glucose + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction, data = df3)

# Lets compute the FStat
Fstat21 <- (deviance(mod22)-deviance(mod21))*df.residual(mod21)/deviance(mod21)

# Now compute the p-value
pf(Fstat21,1,df.residual(mod21),lower.tail = FALSE)

# The p_value is less than 0.05 so we reject the null hypothesis and we can say that the variable Age is significant

mod23 <- lm(BloodPressure ~ Glucose + SkinThickness + Insulin + BMI + Age, data = df3)

# Lets compute the FStat
Fstat22 <- (deviance(mod23)-deviance(mod21))*df.residual(mod21)/deviance(mod21)

# Now compute the p-value
pf(Fstat22,1,df.residual(mod21),lower.tail = FALSE)

# The p-value is 0.05508768 so the variable DiabetesPedigreeFunction is not significant

mod24 <- lm(BloodPressure ~ Glucose + SkinThickness + Insulin + Age, data = df3)

# Lets compute the FStat
Fstat23 <- (deviance(mod24)-deviance(mod23))*df.residual(mod23)/deviance(mod23)

# Now compute the p-value
pf(Fstat23,1,df.residual(mod23),lower.tail = FALSE)

# The p-value is 5.55109e-06 so the variable BMI is significant

mod25 <- lm(BloodPressure ~ Glucose + SkinThickness + BMI + Age, data = df3)

# Lets compute the FStat
Fstat24 <- (deviance(mod25)-deviance(mod23))*df.residual(mod23)/deviance(mod23)

# Now compute the p-value
pf(Fstat24,1,df.residual(mod23),lower.tail = FALSE)

# The p-value is 0.128 so the variable Insulin is not significant

mod26 <- lm(BloodPressure ~ Glucose + BMI + Age, data = df3)

# Lets compute the FStat
Fstat25 <- (deviance(mod26)-deviance(mod25))*df.residual(mod25)/deviance(mod25)

# Now compute the p-value
pf(Fstat25,1,df.residual(mod25),lower.tail = FALSE)

# The pvalue is 0.88 so the variable SkinThickness is not significant

# Model diagnosis ----------------------------------------------------
# Hetereoscedasticity and linearity
plot(mod26,1)

# Lets check for the models 26 and 23
plot(mod26,1)
plot(mod23,1)

# There is not a big change so the model is linear

# Lets try with a transformation of the dependent variable
mod27 <- lm(sqrt(BloodPressure) ~ Glucose + BMI + Age, data = df3)

plot(mod27,1)
plot(mod26,1)

# There is no need of a change of variable

# Normality
plot(mod26,2)
# Most points are close to the line so we can say that the residuals are normally distributed

# Unsual observations
plot(mod26,4)
plot(mod26,5)
# Points far from the line have large residuals (348, 58)
# Points in the right have high leverage (87)

# Remove the unsual observations from the model
mod28 <- lm(BloodPressure ~ Glucose + BMI + Age, data = df3 %>% filter(BloodPressure != 87))

# Lets compare the model coefficients
coeffcomp <- rbind(mod26$coefficients, mod28$coefficients)
coeffcomp

# The coefficients are not very different so we can say that the model is not affected by the unsual observations

# Evaluating scores
# AIC
AIC(mod28,mod26,mod25,mod24,mod23,mod22,mod21)
# BIC
BIC(mod28,mod26,mod25,mod24,mod23,mod22,mod21)

# Adjusted R squared
c(summary(mod21)$adj.r.squared,
  summary(mod22)$adj.r.squared,
  summary(mod23)$adj.r.squared,
  summary(mod24)$adj.r.squared,
  summary(mod25)$adj.r.squared,
  summary(mod26)$adj.r.squared,
  summary(mod28)$adj.r.squared
)



# Model 28 has the best score in BIC and the Second best in AIC and in R squared
# So we can say the model selection is accurate

#=================================================
#LOGISTIC REGRESSIONS
#=================================================

#Deletion of the variables used before in order not make mistakes
rm(list=ls())

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