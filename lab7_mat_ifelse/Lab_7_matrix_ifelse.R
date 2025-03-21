
df = read.csv("~/Documents/GitHub/AdityaS_209/lab7_mat_ifelse/BrainCancer.csv")
df = as.data.frame(df)
df

square <- function(x) x**2
head(df$time)

# 1. Solving matrix equations and review of matrix operations
amat <- matrix(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), nrow = 3, ncol = 4, byrow=TRUE)
amat #byrow is TRUE, the elements get filled row by row

amat2 <- matrix(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), 3, 4, byrow=FALSE)
amat2 #byrow is FALSE, the elements get filled column by column

colnames(amat) <- c(paste("C", 1:4, sep=""))
rownames(amat) <- c(paste("R", 1:3, sep=""))
amat

######
#Matrix
A <- matrix(c(2,5,7,3,1,8,9,10,1,12,5,10,4,17,15,11), 4, 4)
B <- matrix(c(12,5,3,17,1,18,9,10,1,12,5,10,4,15,15,4), 4, 4)
print(A)
print(B)

#element wise multiplication
element_wise_mul <- A*B
element_wise_mul

#matrix multiplication
matrix_mul <- A %*% B
matrix_mul

# Creating vectors
X <- c(5,6,7,8)
Y <- c(8,10,12,5)
print(X)
print(Y)

#outer product
outer(X, Y)

#outer
inner_product <- X*Y #inner product
inner_product

Matrix::Diagonal(x=X) #diagonal mat

#identical matrix
identity <- matrix(data = 1,nrow = 6,ncol = 6)
identity

ele <- c(3,4,-2,4,-5,1,10,-6,5)
A <- matrix(ele, nrow = 3, ncol = 3)
A

ele <- c(5,-3,13)
B <- matrix(ele, nrow = 3, ncol = 1)
B

X = A%*%B
X
X = solve(A,B) #Solves the linear equation A x X = B, returning X
X #X is an matrix-array

#generates inverse matrix
Ainv = solve(A)
A
Ainv
#check if the generated matrix is the identity matrix


check_inv <- Ainv%*%A # -1.110223e-16 and 2.775558e-17 are very close to zero.

check_inv #generates identity mat

#converting to eigen form
results <- eigen(A)
class(results) #eigen is the class
typeof(results)
mode(results)

# eigen values
eigen_value_1 <- results$values[1]
eigen_value_2 <- results$values[2]
eigen_value_3 <- results$values[3]

# eigen vectors
eigen_vector_1 <- results$vectors[,1]
eigen_vector_2 <- results$vectors[,2]
eigen_vector_3 <- results$vectors[,3]

X_1 <- A %*% eigen_vector_1
X_1

#checking and confirms that the computed eigenvector satisfies the eigen equation
#A * v = \lambda v
proof_check <- eigen_value_1 * eigen_vector_1
proof_check


#2.1
df$sq_gtv_time <- square(df$gtv) + df$time 
head(df)
dim(df)

rownames(df) <- paste0("Row-", 1:nrow(df))
rownames(df)

#2.4
colnames(df)
df$ki <- NULL #deleting column with NULL 
colnames(df)

#3
library("readxl")
data <- read_excel("~/Downloads/S1_Dataset.xlsx",sheet = 1)

head(data)
colnames(data)
dim(data)



#4
a = 15.0
b = 110.0
c = 115.0
d = 120.0
e = 125.0
f = 130.0
g = 135.0

#creating set
A <- c("a", "b", "c", "d", "e")
B <- c("d", "e", "f", "g")

union_set <- union(A, B)
print(paste("unionset:" , union_set))

intersect(A,B)
setdiff(A,B)
setequal(A,B)
union_set[A %in% B]
elaborate_union <- c(setdiff(A, b), intersect(A, B), setdiff(A, B))
elaborate_union
setequal(elaborate_union, union(A, B))

#Elements of B present in A 
list(intersect(B,A))

#Elements of B present in A 
intersect(A,B)


#5
vec_elem <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)

for (i in vec_elem) {
  if (i >12) {
    print(i)
  }
}

for (i in vec_elem) {
  if ( i < 20 && i > 10 ) {
    print(i)
  }
}

# 5.2
A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
A[!is.na(A) & A < 100]

#5.3
A[is.na(A)] <-  0
A

genename <- paste0("gene-", 1:7)
genename

gender <- c("M","M","F","M","F","F","M")


#5.5
result1 = c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 = c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 = c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 = c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 = c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 = c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 = c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)


# 5.7
df_data <- data.frame(genename, gender, result1, result2, result3, result4, result5, result6, result7)
colnames(df_data) <- c("GeneName", "Gender", paste("expt", 1:7, sep=""))
datframe = df_data
datframe

expt2_les_30 <- datframe[datframe$expt2 <30,]

#if else

angle = 75

##check the quadrant 
check_quad <- function(x) {
  angle <- x %% 360  # Normalize angle within 0-360
  if (angle == 0) {
    return("On the positive x-axis")
  } else if (angle <= 90) {
    return("First quadrant")
  } else if (angle <= 180) {
    return("Second quadrant")
  } else if (angle <= 270) {
    return("Third quadrant")
  } else {
    return("Fourth quadrant")
  }
}

check_quad(280)

num_dec_order <- function(x,y,z) {
  if (x > y & y > z){
    print(c(x, y, z))
  } else if (x > z & z > y){
    print(c(x, z, y))
  } else if (y > x & x > z){
    print(c(y, x, z))
  } else if (y > z & z > x){
    print(c(y, z, x))
  } else if (z > x & x > y){
    print(c(z, x, y))
  } else if (z > y & y > x){
    print(c(z, y, x))
  }
}

num_dec_order(3,1,6)

#6.3

ticket_cost <- function(distance, age){
  cost = min_cost = 100
  if (distance > 100 && distance <= 1000){
    cost = min_cost + (1.50*(distance-100))
  }
  if (age > 60) {
    cost = cost*0.75
  } else if(age <= 6){
    cost = cost*0.5
  } 
  return(cost)
}

ticket_cost(110,4)

# 7.1 replace all negative values to 0

replace_neg <- function(x) {
  x[x < 0] <- 0  
  return(x)
}

test <- c(-2,-1,0,1,2)
replace_neg(test)

# 7.2 function to calculate the factorial of a number using the Stirling’s approximation
factor_stirling <- function(n){
  term1 <- (n^n * exp(-n) * sqrt(2*pi*n))
  term2 <- 1 
  term3 <- (1 / (12*n))
  term4 <- (1 / (288 * n^2))
  term5 <- (139 / (51840 * n^3))
  term6 <- (571 / (2488320* n^4))
  brackets <- term2 + term3 + term4 - term5 - term6
  result <- term1 * brackets
  return(result)
}

factor_stirling(2)

# 7.3  sum the digits of a number
sum_digits <- function(x){
  result <- 0
  digits <- as.numeric(strsplit(as.character(x), "")[[1]])
  for (i in 1:length(digits)){
    result <- result + digits[i]
  }
  print(result)
}

sum_digits(221)
