pacman::p_load(
  tidyverse,
  dplyr,
  FactoMineR,
  factoextra,
  stats,
  plotly,
  gplots,
  corrplot,
  kableExtra
)

#Read the dataset
d3 = read.csv("DESCRIPTIVE ANALYSIS/DATASET 3/diabetes_d3.csv")
#Filtering the dataset, eliminating outliers
d3 = d3 %>% filter(BloodPressure > 0,BMI > 0, Glucose > 0, Insulin > 0, SkinThickness > 0)
#Converting the Outcome variable to a factor:
d3 = d3 %>% mutate(Outcome = ifelse(Outcome == 1, "Diabetic", "Non-Diabetic"))
d3$Outcome = as.factor(d3$Outcome)

#PCA 3D (3D representation of the PCA, using Plotly)
X = d3 %>% select(-Outcome)
pca3 <- PCA(X,scale.unit=TRUE,ncp = 3)
vars = pca3$var$coord
vars = data.frame(vars)
vars$Dim.2 <- -vars$Dim.2
vars$Dim.3 <- -vars$Dim.3

comps = pca3$ind$coord
comps = data.frame(comps)
comps$Dim.2 <- -comps$Dim.2
comps$Dim.3 <- -comps$Dim.3
comps = cbind(comps,data.frame(Outcome = d3$Outcome))
total_explained_variance = sum(pca3$eig[,"percentage of variance"][1:3])
tit = paste0("Total Explained Variance = ",round(total_explained_variance,2),"%")
knitr::kable(comps)#3D Plot creation with plotly

#Creation of the 3D plot with the individuals
fig <- plot_ly(comps, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, color = ~Outcome, colors = c('#636EFA','#EF553B') ) %>%
  add_markers(size = 12)
# Adding the variables to the plot
for (i in 1:nrow(vars)) {
  fig <- add_trace(fig, x = c(0, vars$Dim.1[i]), y = c(0, vars$Dim.2[i]), 
                   z = c(0, vars$Dim.3[i]), name = rownames(vars)[i],
                   type = 'scatter3d', mode = 'lines', color = I("black"))
}
# Adding the label to the variables
labels = list()
for (i in 1:nrow(vars)) {
  labels[[i]] = list(
    showarrow = F,
    x = vars$Dim.1[i],
    y = vars$Dim.2[i],
    z = vars$Dim.3[i],
    text = rownames(vars)[i],
    xanchor = "left",
    xshift = 5
  )
}
#Adding the labels to the plot
fig = fig %>% layout(
  title = tit,
  legend = list(title=list(text='<b>Outcome of Diabetes Test</b>')),
  scene = list(
    xaxis = list(title = paste0('Comp. 1 (',round(pca3$eig[,"percentage of variance"][1],2),"%)")),
    yaxis = list(title = paste0('Comp. 2 (',round(pca3$eig[,"percentage of variance"][2],2),"%)")),
    zaxis = list(title = paste0('Comp. 3 (',round(pca3$eig[,"percentage of variance"][3],2),"%)")),
    annotations = labels
  )
)

#Visualize the plot in RStudio
fig

#Saving the plotç
htmlwidgets::saveWidget(as_widget(fig), "plot.html", selfcontained = TRUE)


#####################################################################
#PCA (Code used for the Report interpretation)
pca3 <- PCA(d3 %>% select(-Outcome),scale.unit=TRUE)

#Scree plot
fviz_eig(pca3,addlabels = TRUE, ylim = c(0, 50),
         barfill = "steelblue",barcolor = "black",
         linecolor = "black",title ="% of explained variance by each component") 

#Plot for variables
fviz_pca_var(pca3, repel = TRUE) +
  theme_minimal()

#Plot for individuals
fviz_pca_ind(pca3, label="none", habillage=d3$Outcome,
             addEllipses=TRUE, ellipse.level=0.95) + 
  scale_color_brewer(palette="Set1") +
  theme_minimal()

#BIPLOT: Individuals and Variables
fviz_pca_biplot(pca3, label="var",col.var = "black", repel=TRUE, habillage=d3$Outcome) +
  labs(title = "PCA - Individuals and Variables") + 
  scale_color_brewer(palette="Set1") +
  theme_minimal()


#BIPLOT: Individuals and Variables with ellipses
fviz_pca_biplot(pca3, label="var",col.var = "black", repel=TRUE, habillage=d3$Outcome,
                addEllipses=TRUE, ellipse.level=0.8) +
  labs(title = "PCA - Individuals and Variables") + 
  scale_color_brewer(palette="Set1") +
  theme_minimal()

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


#Correlation plots
corrplot(ca.res$row$coord,is.corr=FALSE)
corrplot(ca.res$col$coord,is.corr=FALSE)

#Biplot of categories
fviz_ca(ca.res)


