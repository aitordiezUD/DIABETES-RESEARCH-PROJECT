#Check if the following packages are installed, if not install them
pacman::p_load(
  pastecs,      # for data summaries
  tidyverse,    # data management + ggplot2 graphics 
  flextable,    # converting tables to pretty images
  corrplot,     # study correlation
  tidyr,         # Reshape dataframes, helpful to plot them
  skimr,
  kableExtra
)

#Reading the second dataset
d2 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 2/diabetes_d2.csv")

#Selection of the most relevant variables for the analysis
d2 <- d2[, c("age", "race", "gender", "time_in_hospital", 
             "num_lab_procedures", "A1Cresult", "diabetesMed")]

#Convert all variables to factors
d2 <- d2 %>% mutate_at(c(1,2,3,6,7),as.factor)

#Plotting the age distribution
ggplot(d2, aes(x = age)) +
  geom_bar(stat = "count", fill = "steelblue") +  # Count occurrences of each species
  labs(title = "Distribution of patients' age", x = "Age", y = "Count")

#Plotting distribution of Hospital Stay Length
ggplot(d2, aes(x = time_in_hospital)) + 
   geom_histogram(stat="count",fill = "steelblue") + 
   labs(title = "Distribution of Hospital Stay Length", x = "Days in Hospital", y = "Number of Patients")

#Prop table between Race and Gender
round(prop.table(table(d2$race, d2$gender)),3) %>%
       kable("html") %>%
       kable_styling(full_width = F)

#Plotting A1C Results
ggplot(d2[d2$A1Cresult!="None",], aes(x = A1Cresult)) + 
  geom_histogram(stat="count", fill = "steelblue") + 
  labs(title = "Distribution of A1C Results", x = "A1C (%)", y = "Number of Patients")

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