x <- c(5,3,7,8)
is.integer(x)
is.numeric(x)
x <- integer(x) #error

# changing class of the data in the variable
x <-as.integer(x)
is.integer(x) #TRUE
