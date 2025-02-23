# Module 6
# Student: Sardys Avile-Martinez
# Assignment # 6

### Professor notes:
# Answer the following questions and post your answer on your blog:

# 1. Consider A=matrix(c(2,0,1,3), ncol=2) and B=matrix(c(5,2,4,-1), ncol=2).
# a) Find A + B
# b) Find A - B


    A <- matrix(c(2, 0, 1, 3), ncol = 2)
    B <- matrix(c(5, 2, 4, -1), ncol = 2)
    
    A
    B
    
    #  addition
    A_plus_B <- A + B
    A_plus_B
    
    #  subtraction
    A_minus_B <- A - B
    A_minus_B

# 2. Using the diag() function to build a matrix of size 4 with the following values in the diagonal 4,1,2,3.
    
    
    diag_matrix <- diag(c(4, 1, 2, 3))
    diag_matrix
    
  
# 3. Generate the following matrix:
#   
#   ## [,1] [,2] [,3] [,4] [,5]
#   ## [1,] 3 1 1 1 1
#   ## [2,] 2 3 0 0 0
#   ## [3,] 2 0 3 0 0
#   ## [4,] 2 0 0 3 0
#   ## [5,] 2 0 0 0 3
#   Hint: You can use the command diag() to build it.
    
  # This is one way of doing it:
    
    fivebyfive_matrix <- diag(3, 5, 5)
    
    fivebyfive_matrix
    
    # Update off-diagonal elements as specified
    fivebyfive_matrix[1, 2:5] <- 1   # First row, columns 2-5
    fivebyfive_matrix[2:5, 1] <- 2   # First column, rows 2-5
    
    
    fivebyfive_matrix
    

##################
    
diag(fivebyfive_matrix)

    
# VISUALIZING WITH AI
    
    
    #Heatmap Visualization (heatmap())
    
    heatmap(fivebyfive_matrix, Rowv = NA, Colv = NA, col = heat.colors(256),
            scale = "none", main = "Heatmap of Custom Matrix")
    
    # WHAT WE WILL SEE:
    #Warm colors highlighting higher values (3s in diagonal and 2s in the first column).
    # The 1s in the first row shown with lighter shades.
    
    #Image Plot (image())
    image(1:5, 1:5, fivebyfive_matrix, col = terrain.colors(10),
          main = "Image Plot of Custom Matrix", xlab = "Columns", ylab = "Rows")
   
     # WHAT WE WILL SEE:
    # Color-coded grid, where each cell’s color corresponds to its value.
    # The main diagonal (3s) and off-diagonal values (1s and 2s) pop out distinctly.
    
    
    #3D Perspective Plot (persp())
        persp(fivebyfive_matrix, theta = 30, phi = 30, expand = 0.5, col = "skyblue",
          main = "3D Perspective of Custom Matrix")
        # WHAT WE WILL SEE:
        # A 3D surface with peaks along the diagonal (for the 3s) and elevations corresponding to the 2s and 1s.
        # Flat areas where values are zero.
    
   
   #ggplot2 Tile Plot (geom_tile())
        
    library(ggplot2)
    library(reshape2)
    
    # Convert the matrix to a data frame for ggplot
    df <- melt(fivebyfive_matrix)
    
    # ggplot2 visualization
    ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "ggplot2 Tile Plot of Custom Matrix",
           x = "Column", y = "Row") +
      theme_minimal() +
      coord_fixed()
    
    # WHAT WE WILL SEE:
    # A modern, clean tile plot where deeper blue shows higher values (3s), lighter shades represent 1s and 2s, and white indicates zeros.
    # Perfect for blogs—customizable and aesthetically pleasing.

    
##############EXPERIMENTING####################
    
    
### see here my attempt to use sweep() to create a diagonal matrix
    ####SPOILER: It didn't work :(
    
  
    custom_matrix <- diag(3, 5, 5)
    
    #  Add multiples of 5 to each row
    custom_matrix <- sweep(custom_matrix, 1, c(5, 10, 10, 10, 10), FUN = "+")
    
    # Divide the entire matrix by 5 to scale values
    custom_matrix <- sweep(custom_matrix, 1, c(5, 5, 5, 5, 5), FUN = "/")
    
    #  Subtract values to achieve the target pattern
    custom_matrix <- sweep(custom_matrix, 1, c(0, 2, 2, 2, 2), FUN = "-")
    custom_matrix <- sweep(custom_matrix, 2, c(0, 1, 1, 1, 1), FUN = "-")
    
    # Final matrix
    custom_matrix
    

    

    
    