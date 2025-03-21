
0:10 #generate a range of numbers from 0 to 10

15:5 #generate a range of numbers from 15 to 5 in reverse order

seq( 0,1.5,0.1) #generate a range of numbers between 0 and 1.5 with difference of 0.1
seq(6,4,-0.2) #generate a range of numbers between 6 and 4 with difference of -0.2

N <- c(55,76,92,103,84,88,121,91,65,77,99)
#ways of generating numbers
df <- seq(from=0.04,by=0.01,length=11) 
df
df <- seq(from=0.04,by=0.01,along=N) #generates same as length of the object
df

#generating a sequence of numbers in order 
sequence(c(4,3,4,4,4,5))

#generating repeating number/words with range, each and times argument
rep(x = 9,5) #repeat a number for 5 times
rep(1:4,2) 
rep(1:4,each=2)
rep(1:4,each=2,times=3)
rep(1:4,1:4) #zipping the sequence and number of times a number should be repeated


rep(1:4,c(4,1,4,2))
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1)) #applicable to alphabets
