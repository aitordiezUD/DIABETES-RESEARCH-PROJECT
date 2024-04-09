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
