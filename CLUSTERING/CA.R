######################################################################
#Correspondence Analysis

d1 <- read.csv("DESCRIPTIVE ANALYSIS/DATASET 1/diabetes_d1.csv")

#Create both contingency and probability tables
#Firstly, the contingency table
ctable <- table(d1$Diabetes_012, d1$Income)
rownames(ctable) <- c("None", "Prediabetes", "Diabetes")
#Note: the exact values of categories 2,3,4,6,7 in Income variable aren't specified in the dataset home page
colnames(ctable) <- c("<10.000$", "2", "3", "4", "<35.000$", "6", "7", ">75.000$")
#Nice visualization of the contingency table
ctable %>%
  kbl(caption = "Contingency Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Balloon plot for a nice visualization of the contingency table
balloonplot(ctable, main ="", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

#Secondly, we create the probability table
prob_table <- prop.table(ctable)
#Round the table to 2 decimals
prob_table <- round(prob_table, 3)
#Nice visualization of the probability table
prob_table %>%
  kbl(caption = "Probability Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Adding the "Sum" row and columns to the tables
#Only used this for visualization
prob_table_wmargins <- addmargins(prob_table, 1:2)
prob_table_wmargins <- round(prob_table_wmargins, 3)
#Nice visualization of the probability table with margins
prob_table_wmargins %>%
  kbl(caption = "Probability Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Chi-square test and Independent Model
#From Chi-square test we can extract the Independent Model
xtest <- chisq.test(ctable)
total_observations <- ctable_wmargins["Sum", "Sum"]
independent_model = xtest$expected
independent_model <- round(independent_model, 2)
independent_model %>%
  kbl(caption = "Independence Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#We compare the results
comp_it_ct = ctable-independent_model
comp_it_ct <- round(comp_it_ct, 2)
comp_it_ct  %>%
  kbl(caption = "Real Values - Expected Values") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Compute the correspondence analysis
ca.res <- CA(ctable)
summary(ca.res)

#Percentage of variance by each component
ca.res$eig

#Screeplot to determine the percentage of explained variance
fviz_screeplot(ca.res, addlabels = TRUE) +
  geom_hline(yintercept=100/2, linetype=2, color="red")

#Biplot to represent similarity
fviz_ca_biplot(ca.res, repel = TRUE)

#Correlation plots
corrplot(ca.res$row$coord,is.corr=FALSE)
corrplot(ca.res$col$coord,is.corr=FALSE)

#Projection of categories
fviz_ca(ca.res)

#Plotting the cos2 value
fviz_cos2(ca.res, choice = "col", axes = 1:2, fill = "lightblue", color = "orange")
fviz_cos2(ca.res, choice = "row", axes = 1:2, fill = "lightblue", color = "orange")

