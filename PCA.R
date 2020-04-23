
data_file <- read.csv("data_brexit_referendum_adjusted_data.csv")

# PCA works best with the numeric data
# check variable for numeric property

data_numeric_variable <- sapply(data_file, is.numeric)
data_numeric_variable

# Proportion is calculated from NVotes and Leaves vars.
# This will create a false relationship between proportion and them

data_numeric_variable["NVotes"] <- FALSE
data_numeric_variable["Leave"] <- FALSE
data_numeric_variable

# Remove Non-Numeric vars
data_numeric_adjusted <- data_file[, data_numeric_variable]
data_numeric_adjusted

pca <- prcomp(data_numeric_adjusted, center = TRUE, scale. = TRUE)
pca
summary(pca)
pca$rotation[1:nrow(pca$rotation), 1:4]

par(mfrom = c(1, 1))
plot(pca, type = "l", main = "Principal Component Variences")

str(pca)

# For the finding the location of lbraries
.libPaths()

install.packages("devtools")
library(devtools)
# if error about "backports" then
# Install.packages("backports")
install_github("vqv/ggbiplot")

library(ggbiplot)

biplot <- ggbiplot(pca, obs.scale = 1, var.scale = 1, varname.size = 5)
print(biplot)

install.packages("rcpp")

