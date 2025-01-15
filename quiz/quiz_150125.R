#1 Arthematic equation
arthem_operation <- function(k,n) {
  if (n!=0 && k!=0) {
    add <- sum(k,n)
    subs <- k-n
    div <- k/n
    mult <- k*n
    output <- paste("Addition=",add,"|","Substraction=",subs, "|","Division=",div,"|", "Multiplication=", mult)
    return(output)
  }
  
  else 
    error_int <- paste("n should not be equal to 0")
    return(print(error_int))
}

arthem_operation(1,0)


#2 Quadratic equation
a =3
b =3
c =4

quadr_eq <- function(a,b,c) {
  sqrt_info <- sqrt(abs(b**2 - 4*a*c))
  quar_eq_1 = -b + sqrt_info/(2*a)
  quar_eq_2 = -b - sqrt_info/(2*a)
  output <- paste("Roots are", quar_eq_1, "and", quar_eq_2)
  return(output)
}

quadr_eq(a,b,c)
