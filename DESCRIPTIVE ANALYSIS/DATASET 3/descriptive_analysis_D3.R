#Check if the following packages are installed, if not install them
pacman::p_load(
  pastecs,      # for data summaries
  tidyverse,    # data management + ggplot2 graphics 
  flextable,    # converting tables to pretty images
  corrplot,     # study correlation
  viridis,
  hrbrthemes,
  tidyr         # Reshape dataframes, helpful to plot them
)

#Reading the third dataset
d3 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")

head(d3)

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

