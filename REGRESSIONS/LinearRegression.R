# Check if the following packages are installed, if not install them
pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics 
  corrplot,     # study correlation
  tidyr,        # Reshape dataframes, helpful to plot them
  skimr,
  DescTools,    # calculation of the mode
  faraway
)

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

#----------------------------------------------------------------------------------------------------------------------------

# Model diagnosis
# Heteroscedasticity and linearity
plot(mod4,1)


#Lets check for the plots of mod2 and mod4
plot(mod4,1)
plot(mod2, 1)

# They do not change a lot so we can say that the model is linear

# Lets try with a transformation of the dependent variable
mod5 <- lm(log(Glucose) ~ BloodPressure + Insulin + BMI + Age, data = df3)

# Now plot to compare
plot(mod4,1)
plot(mod5,1)

# There is not a big change so there is no need to transform the dependent variable

# Now lets study the normality
plot(mod4,2)
# Most plots are close to the line so we can say that the residuals are normally distributed

# Unsual observations
plot(mod4,4)
plot(mod4,5)
# Points far from the line have large residuals (302, 226)
# Point in the right have high leverage (5)

# Evaluating scores ----------------------------------------------------------------------------
# AIC
AIC(mod4,mod3,mod2,mod1)
# BIC
BIC(mod4,mod3,mod2,mod1)

# Model 4 has the lowest AIC and BIC so we are right with the model selection
 
c(summary(mod1)$adj.r.squared,
  summary(mod2)$adj.r.squared,
  summary(mod3)$adj.r.squared,
  summary(mod4)$adj.r.squared
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

#-------------------------------------------------------------------------

# Evaluating scores
# AIC
AIC(mod15,mod14,mod13,mod12,mod11)
# BIC
BIC(mod15,mod14,mod13,mod12,mod11)
# In both AIC and BIC the lowest scores are from mod15 so we are right with the model selection

