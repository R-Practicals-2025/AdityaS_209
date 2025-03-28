### Lab 10-11
# ‘d’<distn name> : probability density value is returned (for eg. pbinom)
# ‘p’<distn name>: cumulative probability value is returned up to specified x value
# ‘q’<distn name> : returns the x value up to which the input cumulative probability represents
# (also called the quantile function)
# ‘r’<distn name> : returns m random deviates from the distribution

df <- read.csv("~/Documents/GitHub/AdityaS_209/winequality-white.csv", sep=";", header = T)
summary(df)

###########
# I.1
x <- seq(1,100)
x
s <- sample(x,10) #without replacement
s
s <- sample(x,10, replace = T) #with replacement
s


###########
# I.2
install.packages("gtools")
library("gtools")
x <-  c("A","B","C","D")
cat("Permutations")
per <-  permutations(n = length(x), r = 3, v = x, repeats.allowed = T)
per

comb <- combinations(n = length(x), r = 3, v = x)
print(comb)

###########
# I.1 Distribution
n = 10
p=0.4
m=3

cat("binomal distribution")
pdf <- dbinom(x = m,size = n,prob = p) #Probability p
pbinom(q = m,size = n,prob = p) #Cumulative Probability q,

cat("Finding the m value corresponding to cumulative probability of 0.8.")
cum_prob = 0.8
qbinom(p = cum_prob, size = n,prob = p)

cat("5 points randomly sampled")
r_pts <- rbinom(n = 5, size = n, prob = 0.7)

###plots
pdf1 <- dbinom(x = seq(0:n),size = n,prob = 0.4)
plot(seq(0:n), pdf1, col="red", pch=16, lty=4, type = 'o', xlab = "index", main="Binomial Probability PDF")

pdf2 <- dbinom(x = seq(0:n),size = n,prob = 0.7)
lines(seq(0:n), pdf2, col="blue", pch=16, lty=4, type = 'o', ylab= "PDF")


########
# II.1
x <-  seq(0:1000)

# Set parameters
n <- 10
p <- 0.4

# Generate random samples
rand_100 <- rbinom(100, n, p)
rand_10k <- rbinom(10000, n, p)

# Create frequency tables
freq_100 <- table(rand_100)
freq_100
freq_10k <- table(rand_10k)

par(mfrow=c(2,1))
barplot(freq_100, col="violet")
barplot(freq_10k, col="green")


###########
# II.2 Hypergeometric distribution

par(mfrow=c(1,1))
N =100
K = 70
p=0.3
n=12
d2_hyper <- dhyper(x =seq(0:N),m =  K, k = N-K, n = 12  )

#Histogram
hist(d2_hyper, main = "Hypergeometric distribution", border = T)
text(x = 0.4, y = 40, labels = paste(paste("N =", N, "\nK =", K, "\nn =", n, "\np =", p)))


#cum probability
cum_prob <- phyper(q = 10, m = K, k = N - K, n=n)
round(cum_prob,3)

#x value corresponding to a cumulative probability value of 0.9.
qhyper(p = 0.9, m = K, n = n,k = N-K)

# 5 points randomly from this distribution and print these with two significant digits.
options(digits = 2)

rhyper(nn=5, m= K, n= n, k= N-K)


###########
#I.3 Geometric distribution:

p1 = 0.3
p2 = 0.8
x = seq(0:30)
pdf1 <- dgeom(x = x,prob = p1)
pdf2 <- dgeom(x = x,prob = p2)

par(mfrow=c(1,2))
# sub = paste(paste("p1 =", p1, "\np2 =", p2))
plot(x,pdf1, pch=8, col="orange", lty =4, lwd = 1, type = "o")
plot(x,pdf2, pch=8, col="skyblue", lty =4, lwd=1, type = "o")
mtext(paste("Geometric Distribution", "p1 = ", p1, "and","p2 = ", p2), side = 3, line = -2, outer = T, font = 2 )

#Found difference in the distribution of data points with respect to probability

cat("cumulative prob: ") ; pgeom(q = 4,prob = 0.3)
cat("cumulative prob = 0.2 at: ") ;  qgeom(0.2, prob = 0.1)
cat("6 random deviates with p=0.4 :") ; rgeom(n = 6,prob = 0.4)




#########
# I.4 Negative binomial
p = 0.5
r = 3 #number of successes required
y = 5 #number of failures before the rth success
p_vec <- seq(0,1,0.1)
cat(" negative binomial probability density for y=5, r=3 and p=0.3: ") ; pdf <- dnbinom(prob = p,size = r,x = y) ; pdf
cat("cumulative negative binomial probability density up to y=5 : ") ; pnbinom(q = 5, size = r, prob = 0.5)
cat("corresponding to a cumulative probabilty value of 0.5: ") ; qnbinom(p = p_vec, size = 3, prob = 0.5)
cat(paste("Using array of p vector at different p values:"), p_vec)
cat("4 random points sampled from this distribution with r=3 and p=0.3 : ") ; rnbinom(4,size = r,prob = 0.5)

x = seq(0,20)
pdf <- dnbinom(prob = p,size = r,x = x)
pdf

par(mfrow=c(1,1))
plot(pdf, type = 'o', pch=16, lty = 4, col="magenta", main = "Negative binomial distribution", ylim = c(0,0.5), xlim = c(0,length(x)))
#lines(dnbinom(prob = 0.4,size = r,x = x), type = 'o', pch=16, lty = 4, col="red", main = "Negative binomial distribution")
#lines(dnbinom(prob = 0.6,size = r,x = x), type = 'o', pch=16, lty = 4, col="red", main = "Negative binomial distribution")
#lines(dnbinom(prob = 0.8,size = r,x = x), type = 'o', pch=16, lty = 4, col="green3", main = "Negative binomial distribution")


sample_df <-  rnbinom(10000,size = r,prob = 0.5)
freq_df <- table(sample_df)
hist(freq_df, breaks = 10, xlab = "Frequency of samples", main = "Histogram of negative binomial distribution \nfollowing population", col="aquamarine")

#########
# I.5 Poisson distribution

m=7
lambda_val = 10
pdf <- dpois(x = m, lambda = lambda_val) ; cat("Compute and print the Poisson probability given λ = 10 and m = 7 : ") ;pdf
cpdf <- ppois(q = 7,lambda = lambda_val) ; cat(" the cumulative probability for the same values: ") ;  cpdf

x <-  seq(0,20)
data_1 <- rbinom(n = 1000, prob = 0.3 , size = 1000)
data_2 <- rpois(n = 1000, lambda = 1000*0.3)


##plots
par(mfrow=c(2,2))

barplot(data_1, col = "azure", main = "binomial distribution")
barplot(data_2, col="beige", main="Poisson distribution")
hist(data_1, main = "Histogram binomial distribution")
hist(data_2, main="Histogram Poisson distribution")
## large n and small p, the poisson and binomial distribution are equivalent as lambda = np
## Data is more skewed in Poisson distribution than binomial distbribution.

cat("the quantile value corresponding to cumulative probability of 0.22 and λ = 10: ") ; qpois(p = 0.22, lambda = 10)
cat("10000 random sample points from a Poisson distribution with λ = 9") ; rand_df <- rpois(n = 10000, lambda = 9)

#plots
par(mfrow=c(1,1))
hist(rand_df, xlab = "bins", col = "deepskyblue1" ,main = "Poisson distribution with λ = 9: \n10000 random sample points") 


#########
# I.6 Gaussian Distribution

x <- seq(0,100)
pdf <- dnorm(mean = 12, sd = 2, x =x)
cpdf1 <- pnorm(q = 2 ) ; cpdf1
cpdf2 <- pnorm(q = -2 ) ; cpdf2
round(cpdf2,digits = 2) == round(1- cpdf1, digits = 2) # Yes, both are same

x <- seq(-4,4,by=0.005)
rand_df <- rnorm(n = x, mean = 0, sd = 1)
hist(rand_df, breaks = 20, xlim = c(-5,5))
text(x = 4,y = 300, "mean = 0\nsd = 1")


