#Check if the following packages are installed, if not install them
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  corrplot,      # study correlation
  vcd,
  kableExtra,
  DescTools     # calculation of the mode
)

# Reading the first dataset
d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")


head(d1)

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

rownames(props) <- c("0", "1")

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
