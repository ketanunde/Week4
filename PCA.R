
# Before using this code, load the "data_brexit_referendum_adjusted.csv"
# file from Blackboard. Read through "brexit example.R" for full details
# on how this file was generated

data_file <- read.csv("data_brexit_referendum_adjusted.csv")

# Principal Component Analysis (PCA) wrks best with numerical data
# so I'm checking that all data is now numeric first

data_numeric_variables <- sapply(data_file, is.numeric)
data_numeric_variables

# In the earlier lecture I created a "Proportion" variable using
# the variables "NVotes" and "Leave". Both these variables are correlated
# with "Proportion" and will produce false relationships in PCA.
data_numeric_variables["NVotes"] <- FALSE
data_numeric_variables["Leave"] <- FALSE

# Now I'll remove all non-numeric data and the other 2 variables
data_file_adjusted <- data_file[, data_numeric_variables]

# Passing this numeric data (23 variables) into the prcomp() function
# and setting two arguments, center and scale, to be TRUE. 
# Then we can have a peek at the PCA object with summary().
pca <- prcomp(data_file_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

# We obtain 23 principal components, which are called PC1-23. Each of these 
# explains a percentage of the total variation in the dataset. 
# That is to say: PC3 explains 44% of the total variance, which means that 
# nearly half of the information in the dataset (23 variables) can be 
# encapsulated by just that one Principal Component. 
# PC2 explains 22% of the variance. So, by knowing the position of 
# a sample in relation to just PC1 and PC2, you can get a very 
# accurate view on where it stands in relation to other samples, 
# as just PC1 and PC2 can explain 66% of the variance.

# We can look at the "cumulative proportion" line to see this
# value across all variables eg PC1-3 = 75% of data

# This plot shows the variamces in squared standard deviations
# from the summary() results.
# We can see how each subsequent principal component captures a lower
# amount of total variance.
plot(pca, type = "l", main = "Principal Components' Variances")


# Let's call str() to have a look at your PCA object.
# The center point ($center), 
# scaling ($scale), 
# standard deviation(sdev) of each principal component
# The relationship (correlation or anticorrelation, etc) between the 
# initial variables and the principal components ($rotation)
# The values of each sample in terms of the principal components ($x)

str(pca)

# For the finding the location of lbraries
.libPaths()

# install.packages("devtools")
library(devtools)
# if error about "backports" then
# Install.packages("backports")
# install_github("vqv/ggbiplot")

library(ggbiplot)



# Eigenvalues
install.packages("factoextra")
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

fviz_eig(pca, addlabels = TRUE, ylim =c(0, 50))

pca_for_variable <- get_pca_var(pca)
pca_for_variable


library("corrplot")
corrplot(pca_for_variable$cos2, is.corr = FALSE)

fviz_pca_var(pca, col.var = "Black")

# Quality of the representation of the data
head(pca_for_variable$cos2, 10)

# Total cos2 of the variables on dim1 and dim2
fviz_cos2(pca, choice = "var", axis = 1:2)

# biplot
fviz_pca_var(pca, col.var = "cos2", 
             gredient.cols = c("red", "blue", "green"), 
             repel = TRUE )

head(pca_for_variable$contrib, 20)
fviz_pca_var(pca, col.var = "contrib",
             gredient.cols = c("red", "blue", "green"))

# PC1
fviz_pca_contrib(pca, choice = "var", axes = 1, top = 20)

# PC2
fviz_pca_contrib(pca, choice = "var", axes = 2, top = 20)

# Contribution of 1 - 3
fviz_pca_contrib(pca, choice = "var", axes = 1:3, top = 20)

# Color output by groups
fviz_pca_ind(pca, axes = c(1, 2), 
             col.var = "contrib", goem.ind = "point",
             col.ind = data_file$Vote, palette = c("Red", "Green"),
             addEllipses = TRUE, legend.title = "Vote")


# plot of individual variables
fviz_pca_biplot(pca, col.ind = data_file$Vote, 
                palette = "jco", addEllipses = TRUE,
                label = "var", col.var = "black", 
                repel = TRUE, legend.title = "Vote")


# Tidy charts output
biplot <- fviz_pca_biplot(pca, col.ind = data_file$Vote,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE)
library("ggpubr")
ggpar(biplot,
      title = "Principle Component Analysis", 
      subtitle = "Brexit Dataset", 
      caption = "Source: BBC",
      xlab = "PC 1", ylab = "PC 2",
      legend.title = "Vote", 
      legend.position = "top",
      ggtheme = theme_dark(), 
      palette = "jco")

biplot <- fviz_pca_ind(pca, geom = "point", col.ind = data_file$Vote)

ggpar(biplot,
      title = "Principle Component Analysis", 
      subtitle = "Brexit Dataset", 
      caption = "Source: BBC",
      xlab = "PC 1", 
      ylab = "PC 2",
      legend.title = "Vote", 
      legend.position = "top",
      ggtheme = theme_dark(), 
      palette = "jco")


# Plot PC 3 and PC 4
# Tidy charts output
biplot <- fviz_pca_biplot(pca, axes = c(3, 4),
                          col.ind = data_file$Vote,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE)
library("ggpubr")
ggpar(biplot,
      title = "Principle Component Analysis", 
      subtitle = "Brexit Dataset", 
      caption = "Source: BBC",
      xlab = "PC 1", ylab = "PC 2",
      legend.title = "Vote", 
      legend.position = "top",
      ggtheme = theme_light(), 
      palette = "jco")

# Summary







