ceiling(5.6) #return smallest integer > x
floor(5.6) #return largest integer < x

#function for converting value of a number to equivalent to ceiling function using floor function.
func_ceiling <- function(x) {
  num = floor(x+0.5) 
  return(num)
}

func_ceiling(6.7)
