vec <-  c(4,7,6,5,6,7) #creating a vector with a few elements
vec
class(vec) #vector with only number shows numeric class

vec <-  c(4,7,6,5,6,7, "Aditya") #change the class to string or characters
vec
class(vec)  #vector with number and alpha shows character class

#Numeric vectors
length(vec) #total number of elements in the object
min(vec)
max(vec)
vec

vec2 <- scan() #start taking input and save it in the vector
vec2[4] #output element at index 4
 
#to extract multiple elements from the vector
ind <-c(2,3,4)
vec2[ind] #function as interdect

vec <- c(3,42,56,5,8,9,10,)
vec[c(2,3,8)]
vec[-1] #remove 1st element


vec[length(vec)] #show the last element
vec[-length(vec)]

vec[c(2,3)]
vec[-2:-4]

#create a function to remove the 2 smallest and largest values from the vector
trim <- function(x){
  out <- sort(x)[c(-1,-2, -(length(x)-1), -length(x))]
  return(out)
}

#other version
trim <- function(x)  sort(x) [c(-1,-2, -(length(x)-1), -length(x))]
vec
trim(vec)


#sequence 
vec[1:3]
vec[seq(2,length(vec),2)]
x <- seq(1,10,2)


#filtering values at multiple of 2
vec
x <-  vec[1:length(vec)%%2 ==0]
x


#filtering values at < 5
x <- 0:10
x[x<=5]
sum(x[x<5])

#3 larget values in the vector
largest_3 <- function(x) sort(x) [length(x), length(x)-1, length(x)-2]
largest_3(x)

#11
#functions to calculate max and min values
which.max(x)
which.min(x)

#12 generate matrix with rows and columns values using column and row bind 
cbind(1:10,10:1)
rbind(1:10,10:1)

#13 
X <- c(1:10)
x
y <- c(1:10*5)

#14
#arthematic operators with vectors
X+y
X*y
X/y
X^y
log(X)  
exp(y)




####-------------------------
## Additional lab work
####-------------------------


#define a function
#1 way
sum <- function(x,y) {
  z= x+y
  return(z)
}

#other way
sum <- function(x,y) x+y #

sort(vec)

#creating a function that shows 2 largest and smallest elements 
smallest_largets_func <- function(x){
  output <- paste("2 Smallest element are:", sort(x)[1],"and",sort(x)[2], "|","2 largest are:", sort(x)[length(x)],"and",sort(x)[length(x)-1])
  return(output)
}