  pacman::p_load(
    tidyverse,
    dplyr
  )
  
  d3 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")
  
  print(nrow(d3))
  
  d3<- d3 %>% filter(BloodPressure > 0,BMI > 0, Glucose > 0, Insulin > 0, SkinThickness > 0)
  
  print(nrow(d3))
  
  #Outcome: 0 = No diabetes, 1 = Diabetes
  #d3$Outcome <- as.factor(d3$Outcome)
  
  table(d3$Outcome)
  
  # Let's see the first observations
  head(d3)
  
  # LOGISTIC REGRESSION
  # First model
  mod1 <- glm(Outcome ~ Pregnancies+Glucose+BloodPressure+SkinThickness+
                Insulin+BMI+Age+DiabetesPedigreeFunction, data = d3, family = binomial())
  mod1 #Any observation is deleted
  summary(mod1)
  #BloodPressure, SkinThickness and Insulin are not significant, because p-value > 0.05
  #They are negligible
  confint(mod1)
  
  # Model selection
  mod2 <- glm(Outcome ~ Glucose+BMI+DiabetesPedigreeFunction+Age, data = d3, family = binomial())
  
  # Likelihood ratio test
  lambda <- mod2$deviance - mod1$deviance
  dof <- mod2$df.residual - mod1$df.residual
  
  p_value <- pchisq(lambda, df = dof, lower.tail = FALSE)
  p_value
  # Given that the p value is big, the we cannot reject the null hypothesis
  # Therefore we should select mod2 over mod1
  
  #Now another test is going to be conducted to study the inclusion of Age
  summary(mod2)
  #Regarding Wald Test, Age is signicant in the model
  #Let's see the results of the Likelihood Ratio Test
  mod3 <- glm(Outcome ~ Glucose+BMI+DiabetesPedigreeFunction, data = d3, family = binomial())
  
  # The statistic can be obtained with
  lambda <- mod3$deviance - mod2$deviance
  # The difference in degrees of freedom
  dof <- mod3$df.residual - mod2$df.residual
  # We compute the pvalue
  pchisq ( lambda ,df= dof , lower.tail = FALSE )
  # Given that the p value is small, we can reject the null hypothesis
  # Therefore we should select mod2 over mod3
  
  #In conclusion we should include Age in the model
  
  # COEFFICIENT INTERPRETRATION
  # A look to the coefficients
  mod2$coefficients
  # we can obtain the exponentials simply by
  exp(mod2$coefficients)
  
  # HOSMER-LEMESHOW TEST
  # install the package first
  # install.packages("ResourceSelection")
  library(ResourceSelection)

# Let's obtain the pvalue 
hoslem.test(d3$Outcome,mod2$fitted.values)
# the first term contains our response variable (what we want to predict)
# the second term contains our model

# Given that the pvalue is large we cannot reject the null hypothesis
# Therefore our model fits the data adequately

# CLASSIFICATION TABLE
# The table is produced with two variables: data of the response and predictions
# The response is:
response <- d3$Outcome
# In order to compute our predictions we need to fix the cut point
# Let's select cp=0.5
cp<-0.5
# Let us now use the cut point to obtain our predictions
prediction <- ifelse(mod2$fitted.values < cp,0,1) 
# if the fitted value is less than the cut point then we predict 0, otherwise 1

# We can now create the table
library(caret)
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificity
library(pROC)
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
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab  
#Accuracy = 0.7857
#Sensitivity = 0.7462
#Specificity = 0.8053



