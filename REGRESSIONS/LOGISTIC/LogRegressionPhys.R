pacman::p_load(
  tidyverse,
  dplyr
)

d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")

print(nrow(d1))



mod1 <- glm(PhysActivity ~ Diabetes_012+Stroke+HvyAlcoholConsump+Fruits+Veggies
            +HighBP+HighChol, data = d1, family = binomial())

mod1 #No observation is deleted

#Let's checkout the Wald Tests.
summary(mod1)
#All variables of the model are significants

# Confidence intervals
#confint(mod1)


# COEFFICIENT INTERPRETRATION
# A look to the coefficients
mod1$coefficients
# we can obtain the exponentials simply by
exp(mod1$coefficients)

# HOSMER-LEMESHOW TEST
# install the package first
# install.packages("ResourceSelection")
library(ResourceSelection)

# Let's obtain the pvalue 
hoslem.test(d1$PhysActivity,mod1$fitted.values)
# the first term contains our response variable (what we want to predict)
# the second term contains our model

# Given that the pvalue is large we cannot reject the null hypothesis
# Therefore our model fits the data adequately

# CLASSIFICATION TABLE
# The table is produced with two variables: data of the response and predictions
# The response is:
response <- d1$PhysActivity
# In order to compute our predictions we need to fix the cut point
# Let's select cp=0.5
cp<-0.5
# Let us now use the cut point to obtain our predictions
prediction <- ifelse(mod1$fitted.values < cp,0,1) 
# if the fitted value is less than the cut point then we predict 0, otherwise 1

# We can now create the table
library(caret)
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab
          

# ROC
# The ROC curve is plot of the sensitivity as a function of 1-specificity
library(pROC)
par(pty="s")
Roc <- roc(response,mod1$fitted.values,plot=TRUE,legacy.axes=TRUE,print.auc=TRUE)

# OPTIMAL CUT POINT
# The optimal cut point can be obtained from the point the sensitivity and the specificity curves cross
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="Cut points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.4,y=0.95,labels = "Sensitivity")
text(x=0.4,y=0.09,labels = "Specificity")
abline(v=0.784,col="red",lty=2)


# Let us now use the value of the new cut point to make predictions
cp<-0.784
prediction <- ifelse(mod1$fitted.values < cp,0,1) 
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
tab  
#Accuracy = 0.6225          
#Sensitivity = 0.6308                   
#Specificity = 0.5966          

