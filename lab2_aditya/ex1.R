#Round off 
round(123456789.123456, digits=3) #using 3 digits to use
round(123456789.123456, digits=2) #using 3 digits to use
options(digits = 10)  #selecting 10 digits to use
x <- round(123456789.123456, digits=2)
x
options(digits = 20)   #selecting 20 digits to use
x <- round(123456789.123456, digits=2)
x


#changing format of digits "f","e","E","g","G", "fg", "s"
formatC(x,format="f",digits = 3) #generates output in float
formatC(x,format="e",digits = 2) #generates output in e format
formatC(153.41,format="g",digits = 3) #generates output in integer after significant round off 


# the value in argument digits is equivalent to the number of digits to consider after decimal for rounding off 
round(12.1343,digits=3) #rounds off after 134
options(digits = 10) #setting up option to 8
round(123.12344,digits=3) #rounds off after 123
round(1234.12344,digits=4) #rounds off after 1234
round(12345.12344,digits=3) #rounds off after 123


x <- formatC(x = c(123.456), digits = 8, format="f")
x #this shows up output in 8 digits and float format


options(digits = 15)
#digit argument of the print func works for number of digits to show from the string
#while the digit option of round function 
print(round(123456788.12346,digits=13),digits=10) #shows only 10 digits out of 
print(round(123456788.123,digits=10), digits = 13) #shows only 12 digits out of 13


#
seq_r = 1:10
paste("Hello World")
paste("Hello", "World")
paste(seq_r)[4] #print the number at 4th place of the generated sequence
as.numeric(seq_r)

#use concatenate func for collapse
#func -> collapse, uses the string and joins the characters using value given in the collapse.

paste(seq_r, collapse =".") #generate sequence joined by "."
as.numeric(paste(seq_r)) #convert the sequence into integers
paste(c("Hello", "World"), 1:10, sep = "-") #concatenate combinations of strings and range of number and join them using separators
paste(c("Hello"), 1:10, sep = "-")  #concatenate combinations of strings and range of number and join them using separators


