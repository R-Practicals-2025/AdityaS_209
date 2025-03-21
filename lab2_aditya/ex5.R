#matrices

y <- 1:24
dim(y) <- c(2,4,3) #2= row, 4 columns, and 3 elements in the third dimension 
y
# reshaping the matrix
dim(y) <- c(3,5) #error
dim(y) <- c(2,2,6)
y

#1 Matrix
x <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3)
x
x <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3) 
x[,1] #giving an array of column 1

dim(x) <- c(2,6,2)

#2 reshaping dimension
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2) #changing row ==2, fills values in rows 
V
dim(vector) <- c(4,2) #change dimention, 4 rows and 2 columns
is.matrix(vector) 



####-------------------------
## Additional lab work
####-------------------------


#add the values by row (byrow=TRUE), fills rows first
x <- matrix(1:24, nrow = 3, byrow = T)
dim(x) <- c(2,3,2,2)
x[1,2,2,1]


#add the values by row  (byrow=FALSE), fills columns first
# vector - combination of numbers and alphabets

x <- c(1,2,3,4, "Stats", "AN")
x
dim(x) <- c(1,1,3,2)
x

#subset the matrix 
#subsetting the matrix 3rd and 4th dim with value 1
x[,,1,1]