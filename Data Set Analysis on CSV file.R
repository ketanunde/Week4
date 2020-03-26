# Getting to Working Directory
getwd()
# For Selecting the Director use setwd()
#Loading CSV file 
my_data <- read.csv("Diabetes.csv")
# It shows the Types of my_data its dataframe
class(my_data)
str(my_data)

dim(my_data)
head(my_data, 2)
names(my_data)

# is.na.data.frame(my_data)

# before doing anything we have to see the incomplete data
my_data[!complete.cases(my_data),]


# Checking the missing data and replace them to NA
my_data$Address[my_data$Address == ""] <- NA
my_data$Daibetes.type[my_data$Daibetes.type == ""] <- NA
my_data$Status[my_data$Status == ""] <- NA

str(my_data)
# Converting the data from factor to character or back to factor

my_data$Daibetes.type <- as.character(my_data$Daibetes.type)
my_data$Daibetes.type <- as.factor(my_data$Daibetes.type)
my_data$Address <- as.character(my_data$Address)
my_data$Status <- as.character(my_data$Status)
my_data$Status <- as.factor(my_data$Status)
str(my_data)


#Analysing Missing Information
#Refresh understanding of NA's in the data
my_na <- my_data[!complete.cases(my_data),]
nrow(my_na)

# Graphics option
install.packages("mice")
library(mice)
md.pattern(my_data)

#Use of VIM package for the missing values
install.packages("VIM")
library(VIM)

missing_value <- aggr(my_data, prop = FALSE, numbers = TRUE)
summary(missing_value)


# If we decided to delete some of the records which is not that important
# Only data which is having staus missing 

my_na <- my_data[!complete.cases(my_data$Daibetes.type, my_data$Status),]
my_na
nrow(my_na)
nrow(my_data)

# 15 caes having the missing information
# Reverse the logic to conatin relevnt data
full_data <- my_data[complete.cases(my_data$Daibetes.type, my_data$Status),]
nrow(full_data)


# 4 configure type of an unorderesd factor
my_data$Status <- factor(my_data$Status,order=TRUE, levels = c("Poor", "Improved", "Excellent"))
str(my_data)


# 5 Configure the name to the attribute
col_name <- c("Patient Name", "NI Address", "Type", "Age", "Health Status")
colnames(my_data) <- col_name
head(my_data, 10)


# 6
patient_names <- my_data[1:1]
head(patient_names, 10)
head(my_data$`Patient Name`, 10)

