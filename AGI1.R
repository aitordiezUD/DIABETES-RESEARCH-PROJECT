#Check if the following packages are installed, if not install them
pacman::p_load(
  pastecs,      # for data summaries
  tidyverse,    # data management + ggplot2 graphics 
  flextable,    # for pretty tables
  corrplot,     # study correlation
  tidyr,        # Reshape dataframes, helpful to plot them
  skimr,        # summary statistics
  kableExtra,   # for pretty tables
  DescTools,    # calculation of the mode
  viridis,      # color palettes
  hrbrthemes    # ggplot2 themes
)



############ DATASET 1 ############

# Reading the first dataset
d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")

#Convert all variables to factors
d1 <- d1 %>% mutate_at(c("Diabetes_012", "HighBP", "HighChol", "Smoker", "Stroke", "PhysActivity", "HvyAlcoholConsump", "AnyHealthcare",
                         "GenHlth","MentHlth","PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income","Fruits","Veggies"),
                       as.factor)

#Selection of categorical variables
factor_vars <- d1 %>% select_if(is.factor)

#Creation of the summary
summaryDf_factors<- skim(factor_vars)

#Function to calculate the mode and adding it to the summary
summaryDf_factors <- cbind(summaryDf_factors,"Mode" = apply(d1 %>% select_if(is.factor), 2, function(x) {
  c(Mode = Mode(x))
}))

#Deletion of some columns that aren't relevant
summaryDf_factors <- summaryDf_factors[,-c(1,3,4,5)]

#Selecting the first 8 factor variables
summaryDf_factors_1 <- summaryDf_factors %>% slice(1:9)

#Selecting the last 8 factor variables
summaryDf_factors_2 <- summaryDf_factors %>% slice(10:18)

#Combining the two factor dataframes
summaryDf_factors <- cbind(summaryDf_factors_1,summaryDf_factors_2)

#Adding the variable names to the summary
colnames(summaryDf_factors) <- c("Variable1","N_unique1","Top counts1","Mode1","Variable2","N_unique2","Top counts2","Mode2")

#Parsing the dataFrame to a flextable in order to make it prettier
ftSummary_factors <- flextable(summaryDf_factors)
ftSummary_factors <- fontsize(ftSummary_factors, size = 10)


#Saving the table in a docx in order to copy and paste in the report
save_as_docx(ftSummary_factors, path = "DESCRIPTIVE ANALYSIS/DATASET 1/summary_d1.docx")

# Creating a data frame with only the binary variables of the dataset
dBinary <- d1%>% select(-c(Diabetes_012,BMI, MentHlth, PhysHlth, GenHlth,
                           Age, Education, Income))

# Turn binary columns into factors so that they are not interpreted as numeric
dBinary <- lapply(dBinary, factor)

# Compute the proportion of each of the variables
props <- sapply(dBinary, function(x) prop.table(table(x)))

#Creating a pretty visualization of most relevant proportions
props <- props[,c(1,2,4,14,8,9)]

rownames(props) <- NULL

round(props,3) %>%
  kable("html") %>%
  kable_styling(full_width = F)

# Compute the chi-square test to see the relation between two variables
# Create the contingency tables.
cont_table <- table(dBinary$Smoker, dBinary$HighChol)
chisq_res <- chisq.test(cont_table)

#Between Sex and High BP

cont_table2 <- table(dBinary$Sex, dBinary$HighBP)
chisq_res2 <- chisq.test(cont_table2)

# Compute the Anova test to see the relation between a qualitative and a quantitative variable.
# Between BMI and Diabetes_012
res <- aov(d1$BMI ~ d1$Diabetes_012)

# Fit a model
model <- lm(BMI ~ as.factor(Diabetes_012), data = d1)

# Get the result of the test
anova_result <- anova(model)




############ DATASET 2 ############
#Deletion of the variables used before in order not make mistakes
rm(list=ls())

#Reading the second dataset
d2 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 2/diabetes_d2.csv")

#Selection of the most relevant variables for the analysis
d2 <- d2[, c("age", "race", "gender", "time_in_hospital", 
             "num_lab_procedures", "A1Cresult", "diabetesMed")]

#Convert all variables to factors
d2 <- d2 %>% mutate_at(c(1,2,3,6,7),as.factor)

#Selection of categorical variables
factor_vars <- d2 %>% select_if(is.factor)

#Creation of the summary
summaryDf_factors<- skim(factor_vars)

#Function to calculate the mode and adding it to the summary
summaryDf_factors <- cbind(summaryDf_factors,"Mode" = apply(d2 %>% select_if(is.factor), 2, function(x) {
  c(Mode = Mode(x))
}))

#Deletion of some columns that aren't relevant
summaryDf_factors <- summaryDf_factors[,-c(3,4,5)]

#Parsing the dataFrame to a flextable in order to make it prettier
ftSummary_factors <- flextable(summaryDf_factors)

#Summary of numerical values
dfSummary_num = d2 %>% select_if(is.numeric) %>% stat.desc()

# Deletion of some irrelevant rows
dfSummary_num <- dfSummary_num[-c(7,10,11,16,18),]

#Round numeric columns to 2 decimal numbers
dfSummary_num <- dfSummary_num %>% mutate(across(where(is.numeric), round, digits = 2))

#adding a column with the row names. Necessary for the flextable
statRow <- data.frame("Stat"=rownames(dfSummary_num))
dfSummary_num <- cbind(statRow,dfSummary_num)

# Get the quartiles and IQR for each column (excluding the summary rows)
quartiles <- cbind("Stat" = c("Q1","Q2","Q3","IQR"),apply(d2[,c(4,5)], 2, function(x) {
  q1 <- quantile(x,0.25)
  q2 <- quantile(x,0.5)
  q3 <- quantile(x,0.75)
  iqr <- IQR(x)
  c(Q1 = q1, Q2 = q2, Q3=q3 , IQR = iqr)
}))

# Add the quartiles and IQR as new rows to the dataframe
dfSummary_num <- rbind(dfSummary_num, quartiles)

#Deletion of rownames because they are already in the first column
rownames(dfSummary_num) <- NULL

#Convert data frame to a flextable for a pretty representation
ftSummary_num <- flextable(dfSummary_num)

#Saving flextable as a docx in order to copy and paste the table in the report
save_as_docx("Numerical" = ftSummary_num, "Categorical" = ftSummary_factors,path = "DESCRIPTIVE ANALYSIS/DATASET 2/summary_d2.docx")

#Plotting the age distribution
ggplot(d2, aes(x = age)) +
  geom_bar(stat = "count", fill = "steelblue") +  # Count occurrences of each species
  labs(title = "Distribution of patients' age", x = "Age", y = "Count") +
  theme_minimal()

#Plotting distribution of Hospital Stay Length
ggplot(d2, aes(x = time_in_hospital)) + 
  geom_histogram(stat="count",fill = "steelblue") + 
  labs(title = "Distribution of Hospital Stay Length", x = "Days in Hospital", y = "Number of Patients") +
  theme_minimal()

#Prop table between Race and Gender
round(prop.table(table(d2$race, d2$gender)),3) %>%
  kable("html") %>%
  kable_styling(full_width = F)

#Plotting A1C Results
ggplot(d2[d2$A1Cresult!="None",], aes(x = A1Cresult)) + 
  geom_histogram(stat="count", fill = "steelblue") + 
  labs(title = "Distribution of A1C Results", x = "A1C (%)", y = "Number of Patients") +
  theme_minimal()

#Plot the relation between the age and if they have been medicated
ggplot(d2, aes(x = age, fill = diabetesMed)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Age Range and Medication Status",
       x = "Age",
       y = "Patient count") +
  theme_minimal()

#Plot the relation between the age and A1C result
ggplot(d2[d2$A1Cresult != "None",], aes(x = age, fill = A1Cresult)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Age Range and A1C Results",
       x = "Age",
       y = "A1C Results") +
  theme_minimal()

#Grouping the number of lab procedures
d2$group_procedures <- ifelse(d2$num_lab_procedures < 31,"<31",
                              ifelse(d2$num_lab_procedures < 44,"31-44",
                                     ifelse(d2$num_lab_procedures < 57,"44-57","57-132")))
d2$group_procedures <- as.factor(d2$group_procedures)

#Prop table between A1C result and group of procedures
round(prop.table(table(d2[d2$A1Cresult!="None","A1Cresult"],d2[d2$A1Cresult!="None","group_procedures"])),3)
#Median of time spent by age
average_time <- aggregate(time_in_hospital ~ age, d2, mean)

#Plot of the median of time spent by age
ggplot(average_time, aes(x = age, y = time_in_hospital)) +
  geom_col(fill = "steelblue") +
  labs(title = "Time spent in hospital by age",
       x = "Age",
       y = "Average time spent") +
  theme_minimal()






############ DATASET 3 ############
#Deletion of the variables used before in order not make mistakes
rm(list=ls())


#Reading the third dataset
d3 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")

#discretize Outcome variable
d3$Outcome <- ifelse(d3$Outcome == 0,"No diabetes","Diabetes")

#Outcome as a factor
d3$Outcome <- as.factor(d3$Outcome)

#Selection of categorical variables
factor_vars <- d3 %>% select_if(is.factor)

#Creation of the summary
summaryDf_factors<- skim(factor_vars)

#Function to calculate the mode and adding it to the summary
summaryDf_factors <- cbind(summaryDf_factors,"Mode" = apply(d3 %>% select_if(is.factor), 2, function(x) {
  c(Mode = Mode(x))
}))

#Deletion of some columns that aren't relevant
summaryDf_factors <- summaryDf_factors[,-c(3,4,5)]

#Parsing the dataFrame to a flextable in order to make it prettier
ftSummary_factors <- flextable(summaryDf_factors)

# Creating an extend summary of the numeric values of the data frame
dfSummary_num = d3 %>% select_if(is.numeric) %>% stat.desc(norm=TRUE)

# Deletion of some irrelevant rows
dfSummary_num <- dfSummary_num[-c(7,10,11,16,18),]

#Round numeric columns to 2 decimal numbers
dfSummary_num <- dfSummary_num %>% mutate(across(where(is.numeric), round, digits = 2))

#adding a column with the row names. Necessary for the flextable
statRow <- data.frame("Stat"=rownames(dfSummary_num))
dfSummary_num <- cbind(statRow,dfSummary_num)

# Get the quartiles and IQR for each column (excluding the summary rows)
quartiles <- cbind("Stat" = c("Q1","Q2","Q3","IQR"),apply(d3[,1:8], 2, function(x) {
  q1 <- quantile(x,0.25)
  q2 <- quantile(x,0.5)
  q3 <- quantile(x,0.75)
  iqr <- IQR(x)
  c(Q1 = q1, Q2 = q2, Q3=q3 , IQR = iqr)
}))

# Add the quartiles and IQR as new rows to the dataframe
dfSummary_num <- rbind(dfSummary_num, quartiles)

#Deletion of rownames because they are already in the first column
rownames(dfSummary_num) <- NULL

#Convert data frame to a flextable for a pretty representation
ftSummary_num <- flextable(dfSummary_num)

#Saving flextable as a docx in order to copy and paste the table in the report
save_as_docx("Numerical" = ftSummary_num, "Categorical" = ftSummary_factors,path = "DESCRIPTIVE ANALYSIS/DATASET 3/summary_d3.docx")

# Reshape d3 to long format in order to plot it
data_long <- gather(d3[,c("BloodPressure","BMI","Glucose","Insulin")], key = "Measurement", value = "Value")

# Create boxplot using ggplot2
ggplot(data_long, aes(x = Measurement, y = Value, fill=Measurement)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="#636363", size=0.7, alpha=0.5) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplots of BloodPressure, BMI, Glucose and Insulin") +
  xlab("")

# Define a function to plot histogram and density for each numeric variable and save plots
plot_and_save_hist_density <- function(data, folder) {
  # Select numeric columns
  data_numeric <- data %>% select_if(is.numeric)
  
  # Loop through each numeric column
  for (col in names(data_numeric)) {
    # Create histogram with density
    hist(data_numeric[[col]], freq = FALSE, main = paste("Density function of", col))
    dx <- density(data_numeric[[col]])
    lines(dx, lwd = 2, col = "red")
    
    # Save the plot with column name as filename
    dev.copy(png, filename = paste0(folder, "/", col, ".png"))
    dev.off()
  }
}

plot_and_save_hist_density(d3, "DESCRIPTIVE ANALYSIS/DATASET 3/density_functions")

#Study of the correlation coefficients between variables
cor <- cor(d3 %>% select_if(is.numeric))

#Plotting the results of the correlation analysis
corrplot(cor, method = "color", tl.cex = 0.7, tl.col = "black")



