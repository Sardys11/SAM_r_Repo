

# Module 7 - R Object: S3 vs. S4 assignment
# Student: Sardys Avile-Martinez

# This code has the journey to this adventure, but for cleaner code of S3 and S4. Please see "Module_7_S3andS4.R

###NOTES FROM THE PROFESSOR:
# Download any type of data (from the web or use datasets package) or create your own set. 
# 
# Then, on the second step, determine if generic function as discussed in this module can be assigned to your data set, and if not, why? (Example, here is list of data set in R)
# data("mtcars")
# head (mtcars, 6)
# list(mtcars, 6)
# 
# In third and last step, explore if S3 and S4 can be assigned to your data set.


# Imported required libraries. And reading the data.

library(tidyverse)
library(ggplot2)
# library(lubridate)
# library(GGally)
# library(ggpubr)

# STEP 1 LOAD DATA
# NOTE: The source is from: Kaggle.com
#https://www.kaggle.com/code/patnus/california-wildfire-dataset-eda?select=California+Wildfire+Damage.csv

path = "Data/California Wildfire Damage.csv"
df<-read.csv(path,stringsAsFactors = FALSE)

# Check data structure
str(df)

# Verify multiple data types
sapply(df, class)

# Convert date column (assuming format YYYY-MM-DD)
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$Location<-as.factor(df$Location)
df$Cause<-as.factor(df$Cause)
head(df)

df<-df %>% mutate (month=month(Date))
head(df)
colnames(df)

names(df) <- gsub("\\.+", "_", names(df))  # Convert dots to underscores
print(names(df))  # Check if new names match expectations

# STEP 2: Determine if a Generic Function Can be used
# Yes, the dataset contains multiple types (numeric, categorical), making it suitable


# STEP 2.1: Implementing a Generic Function
# Define the generic function
summarize_fire_data <- function(x) {
  UseMethod("summarize_fire_data")
}

### SEVERAL METHODS for different data types:
# Method for data frames
summarize_fire_data.data.frame <- function(x) {
  cat("\n Summary of Wildfire Dataset 🔥\n\n") # emojis in R a courtesy of  package cli and emo. Also see link: https://unicode.org/emoji/charts/full-emoji-list.html
  cat("📊 General Summary:\n")
  print(summary(x))
  
  cat("\n🏠 Total Homes Destroyed:", sum(x$Homes_Destroyed, na.rm = TRUE), "\n")
  cat("\n💰 Total Estimated Loss: $", sum(x$Estimated_Financial_Loss_Million_, na.rm = TRUE), "Million\n")
  
  cat("\n🔥 Top 3 Most Common Causes:\n")
  print(sort(table(x$Cause), decreasing = TRUE)[1:3])
}

# Method for numeric data
summarize_fire_data.numeric <- function(x) {
  cat("\nNumerical Summary:\n")
  cat("Mean:", mean(x, na.rm = TRUE), "\n")
  cat("Median:", median(x, na.rm = TRUE), "\n")
  cat("Min:", min(x, na.rm = TRUE), "\n")
  cat("Max:", max(x, na.rm = TRUE), "\n")
}

# Method for factors 
summarize_fire_data.factor <- function(x) {
  cat("\nCategorical Summary:\n")
  print(sort(table(x), decreasing = TRUE))
}

# Method for character data 
summarize_fire_data.character <- function(x) {
  cat("\nCategorical Summary:\n")
  print(sort(table(x), decreasing = TRUE))
}

summarize_fire_data(df$Cause)  #  using categorical factor data... Later realized that I need to convert to char for S3 sample...But for the moment this is fine.
summarize_fire_data(df$Fatalities)  # numeric data
summarize_fire_data(df)  # whole data set, similar to summary(df), however it includes specific custom Total information.



##### STEP 3 ############### now getting complicated with S3 vs S4
######### Let's see... Let's start with S3, which is simpler and more flexible using the actual dataset I have loaded

is.list(df)  # TRUE means we can use S3
isS4(df) # FALSE because it's not an S4 object yet

### Constructor

wildfire_s3 <- function(df, row_index) {
  row <- df[row_index, ]  # Select the row dynamically, 
  
  obj <- list(
    Incident_ID = as.character(row$Incident_ID),
    Location = as.character(row$Location),  # Convert to character because the index was using the underlying numeric values 
    Area_Burned = row$Area_Burned_Acres_,  
    Homes_Destroyed = row$Homes_Destroyed,
    Cause = as.character(row$Cause)  # Convert to character
  )
  
  class(obj) <- "wildfire_s3"  # Assign S3 class
  return(obj)
}

#NOTE: unfortunately R was converting factor values to their underlying numeric levels instead of the actual text. So have to find a way to make sure the values were converted to string

# Create an S3 object from the 2nd row of the dataset
fire1 <- wildfire_s3(df, 1)  # I have used any number, just to make sure it works

# Test the object
print(fire1)
class(fire1)

######BONUS:   # this so super bonus!!!!! You can decided how many rows to bring using loop. wooohooo
wildfires_list <- lapply(1:5, function(i) wildfire_s3(df, i))  # Create S3 objects for first 5 rows

wildfires_list

# Test one of the objects
print(wildfires_list[[1]])

########### Summary Method

# Define the S3 summary method with emojis :)
summary.wildfire_s3 <- function(x) {
  cat("\n🔥 Wildfire Incident Summary 🔥\n")
  cat("📍 Location:", x$Location, "\n")
  cat("🔥 Area Burned:", x$Area_Burned, "Acres\n")
  cat("🏠 Homes Destroyed:", x$Homes_Destroyed, "\n")
  cat("🕵️ Cause:", x$Cause, "\n")
}

# Lets create an object for row number  10
fire10 <- wildfire_s3(df, 10)
# Run the S3 summary method
summary(fire10)



##### STEP 3.1 ############### now getting complicated with  S4
######### Let's see


# Define the S4 Class for Wildfire
setClass("WildfireS4",
         slots = list(
           Incident_ID = "character",
           Location = "character",
           Area_Burned = "numeric",
           Homes_Destroyed = "numeric",
           Cause = "character"
         )
)

# Constructor Function for WildfireS4
wildfire_s4 <- function(df, row_index) {
  row <- df[row_index, ]  # Select the row dynamically
  
  # Create an S4 object
  new("WildfireS4",
      Incident_ID = as.character(row$Incident_ID),
      Location = as.character(row$Location),
      Area_Burned = row$Area_Burned_Acres_,
      Homes_Destroyed = row$Homes_Destroyed,
      Cause = as.character(row$Cause)
  )
}

# Create an S4 wildfire object from row 10
fire10_s4 <- wildfire_s4(df, 10)

# Test the object
fire10_s4

# Define an S4 Summary Method with emojis
setMethod("show", "WildfireS4",
          function(object) {
            cat("\n🔥 Wildfire Incident Summary (S4) 🔥\n")
            cat("📍 Location:", object@Location, "\n")
            cat("🔥 Area Burned:", object@Area_Burned, "Acres\n")
            cat("🏠 Homes Destroyed:", object@Homes_Destroyed, "\n")
            cat("🕵️ Cause:", object@Cause, "\n")
          }
)

# Test the S4 method
show(fire10_s4)




############ checking the objects
#install.packages("pryr")  # If not installed
library(pryr)

otype(df)  

fireS3 <- wildfire_s3(df, 3)
otype(fireS3)  # Should return "S3"

fireS4 <- wildfire_s4(df, 3)
otype(fireS4)  # Should return "S4"

#####Another way to check
# Check if an object is S3
is.object(fire10) & !isS4(fire10)  # TRUE if S3

# Check if an object is S4
isS4(fire10_s4)  # TRUE if S4

class(fire10)  # Shows S3 class
attributes(fire10)  # Shows additional attributes

class(fire10_s4)  # Shows S4 class
slotNames(fire10_s4)  # Lists slots in an S4 object

# checking the Class and Methods

showClass ("WildfireS4")

showMethods("show")

getMethod("show", "WildfireS4")


####################  The End 
