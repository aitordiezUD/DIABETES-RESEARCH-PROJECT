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

#####################################################################
#K-Means Clustering
# K-means #   
q <- 3
km.out <- kmeans(pca3$ind$coord[,1:2], q, nstart = 20)
fviz_cluster(km.out, data = pca3$ind$coord[,1:2])
fviz_cluster(km.out, data = pca3$ind$coord[,1:2], geom = "point", ellipse = TRUE, ellipse.type = "convex",
             ellipse.level = 0.95, main = paste("K-Means Clustering Results with K = ", q, sep = " ")) +
  theme_minimal()

q <- 2
km.out2 <- kmeans(pca3$ind$coord[,1:2], q, nstart = 20)
fviz_cluster(km.out2, data = pca3$ind$coord[,1:2])
fviz_cluster(km.out2, data = pca3$ind$coord[,1:2], geom = "point", ellipse = TRUE, ellipse.type = "convex",
             ellipse.level = 0.95, main = paste("K-Means Clustering Results with K = ", q, sep = " ")) +
  theme_minimal()


##################################################################
#Hierarchical Clustering
# Hierarchical clustering #
#We first have to compute   a Distance Matrix, where we can choose among different metrics, e.g., euclidean/manhattan:
distance_matrix <- dist(pca3$ind$coord[,1:2], method = "euclidean")
distance_matrix
#Then, we apply the hclust function choosing the method (e.g., complete/single)
hc <-  hclust(distance_matrix, method = "complete")
hc
#We can plot the dendrogram
plot(hc)
#Two clusters:
fviz_dend(hc, k = 3, rect = TRUE, cex = 0.5, show_labels = FALSE, k_colors = c("#470500", "#c4d417", "#1fa4cc"))
#Three clusters:
fviz_dend(hc, k = 2, rect = TRUE, cex = 0.5, show_labels = FALSE, k_colors = c("#470500", "#1fa4cc"))
#To determine the cluster levels for each observation associated with a given cut of the dendrogram, we can use the \texttt{cutree()} function :
cutree(hc, 3)








