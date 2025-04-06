# Module 11
# Student: Sardys Avile-Martinez
# Assignment: Module # 11 Debugging and defensive programming in R

### Professor notes: The code below contains a 'deliberate' bug!  
# Find the bug and fix it.
# Report on your blog the success or failure in your debugging procedure.

# Here is the ORIGINAL code:

  tukey_multiple <- function(x) {
    outliers <- array(TRUE,dim=dim(x))
    for (j in 1:ncol(x))
    {
      outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])  ### first error having the &&, and second not having tukey.outlier defined
    }
    outlier.vec <- vector(length=nrow(x))
    for (i in 1:nrow(x))
    { 
      outlier.vec[i] <- all(outliers[i,]) 
    } 
    
    return(outlier.vec) }
  
    
      # NOTES
      
      # First, what does the function try to do?
      #   The tukey_multiple() function is trying to:
      #   
      #   Find outliers column by column in a matrix x using tukey.outlier().
      # 
      # Mark TRUE if a value is an outlier across all columns for a row otherwise FALSE.
      # 
      # Return a logical vector outlier.vec showing whether each row contains all outliers.
 


  set.seed(123)
  test_matrix <- matrix(rnorm(25), ncol = 5)
  tukey_multiple(test_matrix)
  
  traceback()
  
  #Observations:
    # First crash was on the use of "&&" because only checks the first value, and I am trying to assigning to a vector: outliers[,j]

    # Second crash is due to the tukey.outlier function  not being defined.
  
    # Using debug(tukey_multiple)  to walk through the function after the crash.
    
    debug(tukey_multiple)
    tukey_multiple(test_matrix)
 

    
############# Here is the corrected code
    
    # Here is the code:
    
    tukey_multiple <- function(x) {
      
      # Defensive programming check
      if (!exists("tukey.outlier")) {
        stop("The function 'tukey.outlier' is not defined. Please define it first.")
      }
      
      outliers <- array(TRUE, dim = dim(x))
      
      for (j in 1:ncol(x)) {
        outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])  
      }
      
      outlier.vec <- vector(length = nrow(x))
      
      for (i in 1:nrow(x)) {
        outlier.vec[i] <- all(outliers[i, ])
      }
      
      return(outlier.vec)
    }
    
    
    # Intentionally ran the function without defining tukey.outlier() to test the defensive error message
    tukey_multiple(matrix(rnorm(10), ncol=2))
    # > Error: The function 'tukey.outlier' is not defined. Please define it first. Stop the code and then define it.
    
    # Now define it
    tukey.outlier <- function(x) {
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      return(x < lower | x > upper)
    }
    
    # Try again
    tukey_multiple(matrix(rnorm(10), ncol=2))
    
    ### Finally success!!
 
  