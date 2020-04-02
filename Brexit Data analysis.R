getwd()
file_data <- read.csv("data_brexit_referendum.csv")
# The data type of the file
class(file_data)

head(file_data)
str(file_data)
 
#For Counting the number of records in which having the -1 
sum(file_data$Leave[file_data$Leave == -1])

#It will replace the -1 with NA
file_data$Leave[file_data$Leave == -1] <- NA
# Verifying the records
sum(file_data$Leave[file_data$Leave == -1])

#Viewing the records with NA
na_records <- file_data[!complete.cases(file_data),]
na_records

#Count the number of rows having the NA records
nrow(na_records)

# Check the Missing Values
library(VIM)
missing_values <- aggr(file_data, prop = FALSE, numbers = TRUE)


# percentage of leave voters
file_data$percent_leave <- file_data$Leave / file_data$NVotes
file_data$percent_leave


# Look at the voters percentage where decide who are ready to leave and 
# remain and store the result in Vote Column
file_data$vote[file_data$percent_leave > 0.5] <- "Want to Leave"
file_data$vote
file_data$vote[file_data$percent_leave <= 0.5] <- "Wnat to Remain"
file_data$vote

# First we need to change the factor to character data type
str(file_data)
file_data$RegionName <- as.character(file_data$RegionName)

# Shortened the Nale of Regions
attach(file_data)
file_data$RegionName[file_data$RegionName == "London"] <- "L"
file_data$RegionName[file_data$RegionName == "North West"] <- "NW"
file_data$RegionName[file_data$RegionName == "North East"] <- "NE"
file_data$RegionName[file_data$RegionName == "South West"] <- "SW"
file_data$RegionName[file_data$RegionName == "South East"] <- "SE"
file_data$RegionName[file_data$RegionName == "East Midlands"] <- "EM"
file_data$RegionName[file_data$RegionName == "West Midlands"] <- "WM"
file_data$RegionName[file_data$RegionName == "East of England"] <- "EE"
file_data$RegionName[file_data$RegionName == "Yorkshire and The Humber"] <- "Y"
detach(file_data)

# Deleting the Column from dataset
file_data$shortened_name <- NULL 

# We can see the summary of the data
summary(file_data)

# Check if each value in it is numeric
is.numeric(file_data$percent_leave)
is.numeric(file_data$RegionName)

# use sapply() function to check whether each variable is numeric or not
numeric_variable_list <- sapply(file_data, is.numeric)
numeric_variable_list


# We can create this logic to use to create subset of data
numerical_data <- file_data[numeric_variable_list]
colnames(numerical_data)

# Removing ID Field because to not of any use 
numeric_variable_list["ID"] <- FALSE
numerical_data <- file_data[numeric_variable_list]
colnames(numerical_data)


# lapply() function returns the named list
# each list member has corrosponding name
lapply(numerical_data, summary)

# cbind is column bind for presented the output in columnar structure
?cbind()
cbind(lapply(numerical_data, summary))

# Need to resolve the issue with the NA before continuing
# We could drop the NA from this data but that will not resolve the issue
# instead we will drop them from original dataframe
file_data <- file_data[complete.cases(file_data),]
numerical_data <- file_data[numeric_variable_list]
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

# maximum numbers of percentage_value
numerical_summary["Max.", "percent_leave"]
numerical_summary["Min.", "percent_leave"]

numerical_summary["Max.", "percent_leave"] - numerical_summary["Min.", "percent_leave"]

display_variables <- c("NoQuals", "percent_leave", "AdultMeanAge", "L4Quals_plus", "RegionName")
file_data[which.max(file_data$percent_leave), display_variables]

file_data[which.min(file_data$percent_leave), display_variables]

table(file_data$RegionName)

prop.table(table(file_data$RegionName))
barplot(height = prop.table(table(file_data$RegionName)), 
        main = "Votes Proportion by region", 
        ylab = "Frequency", 
        xlab = "Region", 
        col = "White")
