# NA - Not available, NaN - Not a Number

3/0 #any number divide by zero is infinite
class(Inf) #identify the class of the Inf number
exp(-Inf) #exponential value of Inf

(0:3)**Inf #multiplication of infinite with all the number of the sequence

#division
0/0
0/1

#checking the type of the inter
is.finite(10)
is.infinite(10)
is.infinite(Inf)

#NA values
y <- c(4,NA,7) 
y == "Na" 
is.na(y) 
y[!is.na(y)] #to extract values which are not NA

#Creating vectors
c1 <- c(1,2,NA,2)
c2 <- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(12,NA,15,16)

#Create dataframe
full.frame <-  data.frame(c1,c2,c3,c4)
full.frame

#count number of NA values in the object
length(full.frame[is.na(full.frame)])


#reduce frame with conditions
reduced.frame <- full.frame[!is.na(full.frame$c1),] #condition - fi C1 has NA values remove that row, take all columns
reduced.frame


#working with the array
v <- c(1:6,NA,NA,9:12)
v
seq(along=v)[c(7,8)]
is.na(v)
which(is.na(v))


####-------------------------
## Additional lab work
####-------------------------


vec <- c(4,7,6,5,6,7)
#check the attributes
class(vec)
length(vec)


min_max <- function(x){
  min_x <- min(x)
  max_x <- max(x)
  return(c(paste("Min value:", min_x, "Max value:", max_x)))
}

full.frame <-  data.frame(c1,c2,c3,c4)

#generating arrat with no NA values
full.frame[!is.na(full.frame)]
#generating matrix with no NA values
full.frame[!is.na(full.frame),]
#it takes the length of is.na(full.frame) and takes total length == number of rows and 
#add NA values in the matrix

full.frame
full.frame[c1,]

v <- c(1:6,NA,NA,9:12)
seq(along=v)[is.na(v)]
which(is.na(v))
