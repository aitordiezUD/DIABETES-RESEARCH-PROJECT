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
  kableExtra
)

# Reading the first dataset
d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")

head(d1)

# Creating a data frame with only the binary variables of the dataset
dBinary <- d1%>% select(-c(Diabetes_012,BMI, MentHlth, PhysHlth, GenHlth,
Age, Education, Income))

dBinarySummary <- data.frame()

# Turn binary columns into factors so that they are not interpreted as numeric
dBinary <- lapply(dBinary, factor)

# Create a function to compute the mode of a variable

Mode <- function(x){

  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]

}

# Compute the mode of each of the variables

modes <- sapply(d1%>%select(-c(BMI, CholCheck, Fruits, Veggies, NoDocbcCost, HeartDiseaseorAttack)), Mode)


# Create a data frame having all the modes

dfModes <- as.data.frame(modes)

# Create a data frame with the mode of the first 8 variables

dfModes1 <- cbind(varNames1 = c("Diabetes_012", "HighBP", "HighChol", "Smoker", "Stroke", "PhysActivity", "HvyAlcoholConsump", "AnyHealthcare"),Mode1 = dfModes[1:8, "modes"])

# Create a data frame with the mode of the second 8 variables

dfModes2 <- cbind(varNames2 = c("GenHlth","MentHlth","PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income"),Mode2 = dfModes[9:16, "modes"])

# Merge both of the data frames

dfModes <- cbind(dfModes1, dfModes2)

# Convert dfModes into a data frame again (until this point it is a matrix)

dfModes <- as.data.frame(dfModes)

# Create a flex table

ftModes <- flextable(dfModes)

#Saving flextable as a docx in order to copy and paste the table in the report

save_as_docx(  "Summary" =ftModes,path = "DESCRIPTIVE ANALYSIS/DATASET 1/summary_d1.docx")



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
