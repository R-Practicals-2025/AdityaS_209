#1
vec
#stats
min(vec)
min(vec)
max(vec)

x
sum(x)
range(x)
sort(x)

#calculate means of each column
colMeans(x)

# Define X and Y vectors
X <- c(1, 2, 3, 4) 
Y <- c(5, 6, 7)   

# Outer product between X and Y
Z <- X[1:4] %o% Y[1:3]
Z

# Outer product between Y and X
YoX <- Y[1:3] %o% X[1:4]
YoX

# Transpose of matrices
t(Z)      # Transpose of Z
t(YoX)    # Transpose of YoX

# Dot product between X and Y
X %*% Y   # Dot product
#Error due to unequal dimension


# Another way to calculate dot product
sum(X * Y) # Element-wise product summed up

# Cross product of X[1:4] with Z
crossprod(X[1:4], Z)

# Generate an identity matrix
diag(4)

#understand the class of objects
class(Z)
class(X)