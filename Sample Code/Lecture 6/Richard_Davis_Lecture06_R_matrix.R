# Assign a matrix
# Three components in the function: matrix(c(), n, m)
# 1st component c(): the values of the matrix elements. Note that in R, the 
#    values are listed by columns. That is, the first n values are elements 
#    in the first column, the next n values are elements in the second
#    column, and so on
# 2nd component n: the number of rows
# 3rd component m: the number of columns

# Assign a 2x2 matrix, 1st column is 1, 3; and 2nd column is 2, 4
m1 = matrix(c(1, 3, 2, 4), 2, 2)
# Show the matrix
m1

# Assign a 3x2 matrix, 1st column: 1,3,5; 2nd column: 2,4,6
m2 = matrix(c(1,3,5,2,4,6), 3, 2)
# show the matrix
m2

# Transpose of a matrix
t(m2)

# Determinant of a matrix
det(m1)

# Inverse of a matrix
solve(m1)

# Multiplication of two matrices
m1 %*% m2 # not conformable
m2 %*% m1
