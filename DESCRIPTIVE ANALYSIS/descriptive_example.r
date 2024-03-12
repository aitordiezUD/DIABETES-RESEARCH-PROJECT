# Title: Descriptive
# Content: This is an introductory self-guided lesson with R
# Reference: https://statsandr.com/blog/descriptive-statistics-in-r/


# General information
# We will use a dataset including in R to showcase main descriptive statistics techniques

# Load the iris dataset in the variable d0
d0 <- iris

# Print the structure (number of observations and variables)
str(d0)
# How many variables and observations are there? What type of variables?

# Print the first 6 observations
head(d0)

# Max and Min

# Obtain the minimum sepal length
min(d0$Sepal.Length)

# Obtain the maximum sepal length
max(d0$Sepal.Length)

# Both can be obtained with the range command
extremes <- range(d0$Sepal.Length)

# Print the output
extremes

# Print the minimum
extremes[1]

# Print the maximum
extremes[2]

# Define a function to compute the actual range
rng <- function(x){
    f0 <- max(x)-min(x)
    return(f0)
} 

# Apply the rng function to the sepal length
rng(d0$Sepal.Length)

# Mean

# Compute with mean function
mean(d0$Sepal.Length)

# Quartiles

# Compute with the cuantile function setting 
# 0.25, 0.5 and 0.75 correspond to the first, second=median and third quartiles
quantile(d0$Sepal.Length,0.25)
quantile(d0$Sepal.Length,0.5)
quantile(d0$Sepal.Length,0.75)

# Test any percentile
# 0.4 for 40% and 0.98 for 98%
quantile(d0$Sepal.Length,0.4)
quantile(d0$Sepal.Length,0.98)

# Interquartile range with IQR function or computing the difference manually
IQR(d0$Sepal.Length)
quantile(d0$Sepal.Length,0.75)-quantile(d0$Sepal.Length,0.25)

# Standard deviation and variance #
# For the following calculations we assume that we have a sample and not a population

# sd for standard deviation
sd(d0$Sepal.Length)

# var for variance
var(d0$Sepal.Length)

# apply the function to multiple columns with lapply
# standard deviation
lapply(d0[,1:4],sd)
# variance
lapply(d0[,1:4],var)

# in both cases we select all observations of d0 and the fourth numerical variables

# Summary I

# the summary, function summary, provides the minimum, maximum, quartiles and mean for all variables
summary(d0)

# the summary can be computed for each species
by(d0,d0$Species,summary)

# ---------------------------------------------------#

# Packages

# sometimes we want to use external packages and use objects created by others

# how can we do that?  

# first we install the packages
# if the package is not installed simply uncomment the following line
# install.packages("pastecs")
# In general Rstudio will detect that we need to install a package

# we declare the package
library("pastecs")

# -------------------------------------------------------#  

# Summary II  

# we use the function stat.desc which provides a summary of descriptive statistics
stat.desc(d0)

# the output provides: number of variables, number of null, number of na, etc
# Always check the documentation. 
# The docs for this particular function are in https://www.rdocumentation.org/packages/pastecs/versions/1.3.21/topics/stat.desc

# We can obtain even more measurements with
stat.desc(d0,norm=TRUE)

# Coefficient of variation

# Some measurements in the example above can be obtained manually.
# For instance, the coefficient of variation is simply sd/mean
sd(d0$Sepal.Length)/mean(d0$Sepal.Length)

# Mode

# Let us create a procedure to compute the mode:

# we first obtain the occurrences of each value
tab <- table(d0$Sepal.Length)
# see the ouput
tab

# we then sort the output to obtain the value with the highest occurrence first
sort(tab,decreasing=TRUE)

# the same code may be applied to qualitative variables
sort(table(d0$Species),decreasing=TRUE)

# As previously mentioned, there are multiple paths to achieve the same results:
summary(d0$Species)

# Correlations

# We can compute the correlation between two variables with the cor function
cor(d0$Sepal.Length,d0$Sepal.Width)
# The result is the Pearson correlation

# Contingency Table

# We will now discretize a variable, transforming a quantitative into a qualitative
# we first use create a new variable from the values in Sepal.Length
# we will define the values "small" and "big" for lengths that are smaller or larger than the median

d0$Sepal.Size <- ifelse(d0$Sepal.Length < median(d0$Sepal.Length),
    "small","big"
)

# let us visualize the variable
table(d0$Sepal.Size)

# we now create a contingency table between our two qualitative variables
table(d0$Species,d0$Sepal.Size)

# a similar output is produced with xtabs
xtabs(~d0$Species + d0$Sepal.Size)

# let us now compute the total proportions
prop.table(table(d0$Species,d0$Sepal.Size))

# proportions can be computed by row (1) or by column (2)
prop.table(table(d0$Species,d0$Sepal.Size),1)
prop.table(table(d0$Species,d0$Sepal.Size),2)
# remember conditional probabilities here

# we can round the results to 2 digits for a better readability
round(prop.table(table(d0$Species,d0$Sepal.Size),1),2)
round(prop.table(table(d0$Species,d0$Sepal.Size),2),2)

# Mosaic Plot 
# A mosaic plot allows us to visualize contingency tables
mosaicplot(table(d0$Species,d0$Sepal.Size),
    color = TRUE,
    xlab= "Species",
    ylab= "Sepal size"
    )

# a mosaic plot can also be produced with the package vcd and grid
library(vcd)
library(grid)
mosaic(~ Species + Sepal.Size,
        data=d0,
        direction=c("v","h")
        )


# Bar Plot
# Bar plots for visualizing qualitative vars
barplot(table(d0$Sepal.Size))

# one can also represent the relative frequencies
barplot(prop.table(table(d0$Sepal.Size)))

# barplots can also be produced with the ggplot2 package
library(ggplot2)
ggplot(d0)+
    aes(Sepal.Size)+
    geom_bar()

# Histograms
# For quantitative vars grouped by values (discretized)

hist(d0$Sepal.Length)
# use breaks if you want to customize the number of bins

# histograms are also produced with ggplot2
ggplot(d0)+
    aes(x=Sepal.Length)+
    geom_histogram()

# the number of bins is 30 by default, change it with bins
ggplot(d0)+
  aes(x=Sepal.Length)+
  geom_histogram(bins=10)

# Boxplots
# Boxplot function provides q1,q2,q3 and the minimum and maximum values (without outliers)
boxplot(d0$Sepal.Length)

# we can combine the information of different variables
boxplot(d0$Sepal.Length ~ d0$Species)
# the dot in virginica is an outlier

# boxplots in ggplto2
ggplot(d0)+
  aes(x=Species,y=Sepal.Length)+
  geom_boxplot()

# Dot plot

# Similar to boxplots but with more groups
library(lattice)
dotplot(d0$Sepal.Length ~ d0$Species)
# it's very easy to see the outlier in this plot

# in ggplot2 we have
ggplot(d0)+
  aes(x=Species,y=Sepal.Length)+
  geom_dotplot(binaxis="y",stackdir="center")
# the dot plot in ggplot2 provides more information than the boxplot or lattice, but it' only practical with few observations

# Scatter plot
# In scatter plots we represent two variables simultaneously
# Scatter plots can provide the first hint of a relation between variables
plot(d0$Sepal.Length,d0$Petal.Length)

# in ggplot2
ggplot(d0)+
  aes(x=Sepal.Length,y=Petal.Length)+
  geom_point()

# we can divide the observation in species
ggplot(d0)+
  aes(x=Sepal.Length,y=Petal.Length,colour=Species)+
  geom_point()+
  scale_color_hue()

# Line plot
# Plot of values by index
plot(d0$Sepal.Length,type="l")
# the index is not important here, let's sort the lengths
plot(sort(d0$Sepal.Length),type="l")

# Qq plot
# We will learn about these in a few days in more detail
# A qq plot allows to check the normality assumption
qqnorm(d0$Sepal.Length)
qqline(d0$Sepal.Length)

# It's not obvious to say anything from the distance to the line.
# We will be more precise with bounds.

# the same plot can be done with the ggpubr library
library(ggpubr)
ggqqplot(d0$Sepal.Length)
# again some points are out of the region

# Density plot
# Density function
plot(density(d0$Sepal.Length))

# and in ggplot2
ggplot(d0) +
  aes(x = Sepal.Length) +
  geom_density()

