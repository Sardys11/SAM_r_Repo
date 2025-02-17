# Module 5
# Student: Sardys Avile-Martinez
# Assignment # 5

### Professor notes:
# Your Assignment:
#   Find the value of inverse of a matrix, determinant of a matrix by using the following values:
#   A=matrix(1:100, nrow=10)
#   B=matrix(1:1000, nrow=10)
#   Post your result and procedure you took on your blog.
#   A good start will be:
#     >A <- matrix(1:100, nrow=10)  
#   >B <- matrix(1:1000, nrow=10


# Step 1: Create Matrices

A <- matrix(1:100, nrow=10)  # 10x10 matrix
B <- matrix(1:1000, nrow=10) # 10x100 matrix

print(A)
print(B)

# ✔ A is square (10×10), so it might have an inverse.
# ❌ B is not square (10×100), so it cannot have an inverse

# Step 2: Check if A is invertible

det_A <- det(A)
print(det_A)

# If det_A = 0, then A has no inverse (it is singular).
# If det_A ≠ 0, then A has an inverse.

# Step 3: Find the Inverse of A (If Possible)
# I love functions and why not create a function that checks if a matrix has inverse or not equal to zero.

if (det_A != 0) {
  A_inv <- solve(A)
  print(A_inv)
} else {
  print("Matrix A is singular and has no inverse.")
}


# Step 4: Confirm B has no inverse

if (nrow(B) == ncol(B)) {
  print("B is square, checking determinant...")
  det_B <- det(B)
  print(det_B)
} else {
  print("Matrix B is not square and cannot have an inverse.")
}
#### spoiler: Since B is 10×100, it cannot be inverted.


# Now let's visualize because I love visuals and my professor too.

library(ggplot2)
library(reshape2)

# Prepare matrices for plotting
A_df <- melt(A)
B_df <- melt(B)


#According to https://getplace.io/blog/heat-maps-the-complete-guide, this type of visualization it the best to see the patterns
#on this type of matrices.

# Heatmaps visually highlight how values are distributed across the matrix.
# In Matrix A, values increase from left to right, with darker shades indicating higher values.
# This gradient effect makes it easy to spot trends without scanning raw numbers.

# Heatmap for A
ggplot(A_df, aes(Var2, Var1, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  ggtitle("Heatmap of Matrix A") +
  theme_minimal()



# Heatmap for B
ggplot(B_df, aes(Var2, Var1, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  ggtitle("Heatmap of Matrix B") +
  theme_minimal()

# Interpreting the Heatmap for Matrix B
# Since B is 10×100 (more columns than rows), it has no inverse by definition.
# Matrix B is rectangular (10×100), meaning it has NO inverse.
#Heatmap Clue: A long horizontal shape suggests many more columns than rows.
