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
  corrplot      # study correlation
)

#Reading the third dataset
d3 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")

#An overview of the data frame and a summary of every column
dfSummary <- as.data.frame(skim(d3))

#Changing the name of the columns to make the more Readable
colnames(dfSummary) <- c("Type", "Variable", "Missing Rows", "Completion Rate",
                         "mean", "sd", "min", "Q1", "Q2", "Q3", "max", "hist")

#Delete some columns to make de visualization prettier and mor significant
dfSummary <- dfSummary %>% select(-c("Type","Missing Rows", "Completion Rate"))

#Round numeric columns to 4 decimal numbers
dfSummary <- dfSummary %>% mutate(across(where(is.numeric), round, digits = 4))

#Converting dfSummary to a flextable to make it prettier
ftSummary <- flextable(dfSummary)

#Saving flextable as a docx in order to copy and paste the table in the report
save_as_docx(  "Summary" =ftSummary,path = "DESCRIPTIVE ANALYSIS/DATASET 3/summary_d3.docx")

#Study of the correlation coefficients between variables
cor <- cor(d3 %>% select_if(is.numeric))

#Plotting the results of the correlation analysis
corrplot(cor, method = "color", tl.cex = 0.7, tl.col = "black")