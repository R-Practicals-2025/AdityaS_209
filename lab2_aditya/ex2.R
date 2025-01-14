
0:10 #generate a range of numbers from 0 to 10

15:5 #generate a range of numbers from 15 to 5 in reverse order

seq(0,1.5,0.1) #generate a range of numbers between 0 and 1.5 with difference of 0.1
seq(6,4,-0.2) #generate a range of numbers between 6 and 4 with difference of -0.2

N <- c(55,76,92,103,84,88,121,91,65,77,99)

#ways of generating numbers
df <- seq(from=0.04,by=0.01,length=11) 
df <- seq(from=0.04,by=0.01,along=N)
df

sequence(c(4,3,4,4,4,5))

#generating repeating number/words with range, each and times argument
rep(9,5)
rep(1:4,2) #repeat the sequence 2 times
rep(1:4,each=2) #repeat each element 2 times
rep(1:4,each=2,times=3) #repeat each element twice and the sequence 3 times
rep(1:4,1:4) #repeat each element with corresponding value 


rep(1:4,c(4,1,4,2)) #repeat each element with values given in the argument
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1)) #repeat each element (character) with values given in the argument

seq(-1,1,by=0.1) #generate sequence between -1 to 1 by difference of 0.1
seq(-1,1, length=7) #generate sequence with element length = 7
seq(6,4,by=-0.2) #generate reverse sequence with difference of -0.2


#function to generate sequence without using seq()
seq_num <- function(start, end, interval) start + 0:((end-start)/interval)*interval
seq_num(-1,1,0.1)


#generating sequence
seq(-1) #sequence starts from 1 ends at -1
seq(1) #sequence starts from 1 ends at 1
seq(1,0) #sequence starts from 1 ends at 0
