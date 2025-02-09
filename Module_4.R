# Module 4
# Student: Sardys Avile-Martinez
# Assignment # 4
### Professor notes:
# Your assignment:
#   The following data was collected by the local hospital. This data set contains 5 variables based on observation of 8 patients. In addition to the measurements of the patients checking in to the hospital that night, this data provides the patients' histories regarding the frequency of their visits to the hospital in the last 12 months.
# This data displays the measurement of blood pressure, first assessment by general doctor (bad=1, good =0) titled "first," the second assessment by external doctor (called "second"), and the last row provides the head of the emergency unit's decision regarding immediate care for the patient based on the values 0 or 1 (low = 0, high =1).
# The names of your variables are as follows: "Freq","bloodp","first”, " second”, ”finaldecision”
# The rows 
# 1.    "0.6","103","bad","low","low”
# 2.     "0.3","87","bad","low","high”
# 3.     "0.4","32","bad","high","low”
# 4.      "0.4","42","bad","high","high"
# 5.     "0.2","59","good","low","low”
# 6.      "0.6","109","good","low","high”
# 7.     "0.3","78","good","high","low”
# 8.      "0.4","205","good","high","high”
# 9.      "0.9","135",”NA","high","high"
# 10.    "0.2","176",”bad","high","high”
# Here is Clarification hint:
#Frequency <- c(0.6,0.3,0.4,......
# BP <- c(103,87,32,42,.....
#	First <- c(1,1,1,.....
#   Second <- c(0,0,1,1,...
#   FinalDecision <- c(0,1,0,1,...
#   Your first assignment: Create a side-by-side boxplot (boxplot(x, ...)) and and histogram ((hist(x, ...)). 
#                                                                                                                                                             
#Discuss the outcome of your results regarding patients BPs & MD’s Ratings.
################################################################################

# My Step-by-Step Approach
# To cover this module, I am mostly using functions to clean the data and then to visuaize it



 library(ggplot2)

# Step 1: Define Raw Data with Text Values
patient_data_raw <- data.frame(
  Freq = c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2),
  bloodp = c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176),
  First = c("bad", "bad", "bad", "bad", "good", "good", "good", "good", NA, "bad"),
  Second = c("low", "low", "high", "high", "low", "low", "high", "high", "high", "high"),
  FinalDecision = c("low", "high", "low", "high", "low", "high", "low", "high", "high", "high"),
  stringsAsFactors = FALSE # Keep text as character
)

# Step 2: Function to Convert Categorical Variables
convert_categorical <- function(df) {
  # Convert 'First' column: "bad" -> 1, "good" -> 0
  df$First <- ifelse(df$First == "bad", 1, ifelse(df$First == "good", 0, NA))
  
  # Convert 'Second' column: "low" -> 0, "high" -> 1
  df$Second <- ifelse(df$Second == "low", 0, 1)
  
  # Convert 'FinalDecision' column: "low" -> 0, "high" -> 1
  df$FinalDecision <- ifelse(df$FinalDecision == "low", 0, 1)
  
  # Convert to appropriate types
  df$First <- as.integer(df$First)  
  df$Second <- as.factor(df$Second) 
  df$FinalDecision <- as.factor(df$FinalDecision)  
  
  return(df)
}


# Step 3: Function to Handle Missing Values using "median" and "mean" methods

handle_missing_values <- function(df, method = "median", custom_value = NULL) {
  
  for (col in colnames(df)) {
    if (any(is.na(df[[col]]))) {  # Check if there are any NA values
      
      if (method == "mean" && is.numeric(df[[col]])) {
        df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        
      } else if (method == "median" && is.numeric(df[[col]])) {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        
      } else if (!is.null(custom_value)) {
        df[[col]][is.na(df[[col]])] <- custom_value
      }
    }
  }
  
  return(df)
}

# How This Works
# Automatically detects missing values in all columns.
# If the column is numeric, it replaces NA with:
#   Mean (if method = "mean")
# Median (if method = "median")
# Custom value (if provided, e.g., custom_value = 0)
# Leaves categorical columns unchanged (ensuring we don’t accidentally impute factor variables).



# Step 4: Apply the Cleaning Functions in Sequence
patient_data_clean <- convert_categorical(patient_data_raw)
patient_data_clean <- handle_missing_values(patient_data_clean, method = "median") # Replace NA with median

# Step 5: Function to Generate Boxplot
plot_boxplot <- function(df) {
  boxplot(df$bloodp ~ df$First, 
          col = c("lightblue", "pink"),
          main = "Blood Pressure of First Doctor's Assessment",
          xlab = "Doctor's Initial Assessment (0 = Good, 1 = Bad)", 
          ylab = "Blood Pressure")
}

# Step 6: Function to Generate Histogram
plot_histogram <- function(df) {
  hist(df$bloodp, 
       col = "skyblue", 
       main = "Distribution of Blood Pressure",
       xlab = "Blood Pressure", 
       ylab = "Frequency", 
       breaks = 5)
}

# Step 7: Execute Visualizations
plot_boxplot(patient_data_clean)
plot_histogram(patient_data_clean)

# Step 8: Print Results for Verification
print("Raw Data Before Cleaning:")
print(patient_data_raw)

print("Cleaned Data After Transformation & Imputation:")
print(patient_data_clean)

#   What This Code Does?
#   Reads the dataset with raw text-based categorical values
#   Uses convert_categorical() to programmatically convert text to numeric & factors
#   Handles missing values (NA) with handle_missing_values()
#   Applies structured R programming principles using functions
#   Generates boxplots and histograms to analyze BP & doctor ratings
