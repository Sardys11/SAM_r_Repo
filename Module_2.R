
# Module 2 - Introduction to basic R functions and Data Structures - assignment
# Student: Sardys Avile-Martinez

assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)
myMean <- function(assignment2) { return(sum(assignment2)/length(assignment2)) }
result <- myMean(assignment2)
print(result)

# How can improve this function??
# Improved myMean() function with error handling and NA handling

myMean <- function(data) {
  # Check if input is numeric and not empty
  if (!is.numeric(data)) {
    stop("Error: Input must be a numeric vector.")
  }
  if (length(data) == 0) {
    stop("Error: Input vector is empty.")
  }
  
  # This optional - At this point just adding complexity
  # Handle missing values by removing them before calculating mean
  
  clean_data <- na.omit(data)
  
  # Calculate the mean
  mean_value <- sum(clean_data) / length(clean_data)
  
  return(mean_value)
}

# Well time for testing after adding an extra layer logic to the function

#Test data with missing values

assignment2 <- c(16, 18, 14, NA, 22, 27, 17, 19, 17, 17, 22, 20, 22, NA)

# Call the improved function
result <- myMean(assignment2)
print(result)

# Conclusion: Functions are powerful because of their modularity, reusability, and scalability.
# Whether handling small cases, processing large datasets, or adapting to changing requirements, functions
# offer the flexibility to evolve with our needs or client needs.
