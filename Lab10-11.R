### Lab 10-11
# ‘d’<distn name> : probability density value is returned (for eg. pbinom)
# ‘p’<distn name>: cumulative probability value is returned up to specified x value
# ‘q’<distn name> : returns the x value up to which the input cumulative probability represents
# (also called the quantile function)
# ‘r’<distn name> : returns m random deviates from the distribution

df <- read.csv("~/learning/Biostat_209/lab10_11/winequality-white.csv", sep=";", header = T)
summary(df)

###########  
##I.1

x <- seq(1,100)
x
s <- sample(x,10) #without replacement
s
s <- sample(x,10, replace = T) #with replacement
s


###########
# I.2
#install.packages("gtools")
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
data_1 <- rbinom(n =1000, prob = 0.3 , size = 1000)
data_2 <- rpois(n = 1000, lambda = 1000*0.3) 


##plots
par(mfrow=c(1,1))

# barplot(data_1, col = "azure", main = "binomial distribution")
# barplot(data_2, col="beige", main="Poisson distribution")
# ax <- hist(data_1, main = "Histogram binomial distribution", breaks = 100)
# bx <- hist(data_2, main="Histogram Poisson distribution", breaks = 100)

hist(data_1, col = rgb(1,0,0,0.5), breaks = 50, probability = T,
     main = "Comparison of Binomial and Poisson Distributions", xlab = "Value")
lines(density(data_1), col = rgb(1,0,0,1), lwd = 2)


hist(data_2, probability = TRUE, col = rgb(0,0.5,1,0.5), breaks = 50, add= T)
lines(density(data_2), col = rgb(0.1,0.5,0.8,1), lwd = 2)

legend("topright", legend=c("Binomial","Poissson"), col=c(rgb(1,0,0,0.5),rgb(0.1,0.5,0.8,1) ), pt.cex=2, pch=15 )

cat("the quantile value corresponding to cumulative probability of 0.22 and λ = 10: ") ; qpois(p = 0.22, lambda = 10)
cat("10000 random sample points from a Poisson distribution with λ = 9") ; rand_df <- rpois(n = 10000, lambda = 9)

#plots
par(mfrow=c(1,1))
hist(rand_df, xlab = "bins", col = "deepskyblue1" ,main = "Poisson distribution with λ = 9: \n10000 random sample points") 


#########
# I.6 Gaussian Distribution

x <- seq(0,1000)
pdf <- dnorm(mean = 12, sd = 2, x =x)
cpdf1 <- pnorm(q = 2 ) ; cpdf1
cpdf2 <- pnorm(q = -2 ) ; cpdf2
round(cpdf2,digits = 2) == round(1- cpdf1, digits = 2) # Yes, both are same
x <- seq(-4,4,by=0.005)

rand_df <- rnorm(n = x, mean = 0, sd = 1)
hist(rand_df, breaks = 20, col = rgb(0.5,0.4,0.8,0.5), main = "Histogram of random number between -4 and 4")
text(x = 4,y = 300, "mean = 0\nsd = 1")

cat("the 75th quantile value for a unit normal distrbibution : ") ;  pnorm(q = 0.75, mean = 0, sd = 1)
cat("10000 random sample points from a normal distribution ") ; rand_df2 <- rnorm(n = 10000, mean = 0, sd = 1)

##plots
par(mfrow=c(1,1))
hist(rand_df, probability = T, breaks = 20, col = rgb(0.5,0.4,0.8,0.2), main = "Histogram of random number between -4 and 4")
hist(rand_df2, probability = T, add= T, col =rgb(1,0.2,0.1, 0.5), main = "Histogram of normal distribution\n n= 10000 , mean = 0, sd  =1")
legend("topright", legend = c("-4,4,by=0.005 samples", "rnorm - 10k samples"), col = c(rgb(0.5,0.4,0.8,0.2),rgb(1,0.2,0.1, 0.5)), pt.cex=3, pch=15 )

#Make a histogram plot of a ‘normalised’ binomial distribution with μ = np = 10
n = 20
p = 0.5
m <- rbinom(10000, size=n, prob=p)
ax <- dbinom(prob = 0.4,size = 0:n, x = n)
#hist(ax, breaks = 10)
x <- seq(0,10000)

#6.f - binomial - normalized variable and normal distribution
# Set parameters
n <- 100  # Since np = 10 and p = 0.5, n must be 20
p <- 0.5
mu <- n * p 

# Generate a large sample from binomial distribution
set.seed(123) 
sample_size <- 10000 #Sample pool
bin_sample <- rbinom(n = sample_size, size = n, prob = p) #randomly picking samples from sample pool
bin_sample

# Normalize the binomial random variable
# W = (m - np) / sqrt(np(1-p)) where m is the number of successes
normalized_bin <- (bin_sample - mu) / sqrt(mu * (1-p)) #normalized binomial

# Create the histogram of normalized binomial
hist(normalized_bin, probability = T, breaks = 30, 
     main = "Normalized Binomial vs. Std Normal",
     xlab = "normalized binomial sample",
     col = rgb(0.7, 0.7, 0.9, 0.7))

# Add the unit normal density curve
x <- seq(-4, 4, length.out = 1000)
norm_density <- dnorm(x, mean = 0, sd = 1)
lines(x, norm_density, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Norm Binomial", "Std Normal"), 
       fill = c(rgb(0.7, 0.7, 0.9, 0.7), NA),
       border = c("black", NA),
       lty = c(NA, 1),
       lwd = c(NA, 2),
       col = c(NA, "red"),
       bty = "n")



#6.g
poisson_samples <- list()
n <- 1000
lambda_list <- list(1,10,100,1000)

for (i in c(1,10,100,1000)) {
  var_name <- paste0("p_",i)
  poisson_samples[[var_name]] <- rpois(n = 100, lambda = i )
}

##Normalize the poisson distribution
#z = (m - lambda_val) / sqrt(lambda_val)

par(mfrow=c(2,2))
z = (poisson_samples$p_1 - 1) / sqrt(1)
hist(z, probability = T, breaks = 5)

x <- seq(-1,10,length.out = 100)
norm_density <- dnorm(x, mean = 0, sd = 1)
lines(x,norm_density, col = "red", lwd = 2, freq = F)

##lambda = 10
z = (poisson_samples$p_10 - 10) / sqrt(10)
hist(z, probability = T, breaks = 5)

x <- seq(-3,3,length.out = 100)
norm_density <- dnorm(x, mean = 0, sd = 1)
lines(x,norm_density, col = "red", lwd = 2, freq = F)


##lambda = 100
z = (poisson_samples$p_100 - 100) / sqrt(100)
hist(z, probability = T, breaks = 5)

x <- seq(-3,3,length.out = 100)
norm_density <- dnorm(x, mean = 0, sd = 1)
lines(x,norm_density, col = "red", lwd = 2, freq = F)


##lambda = 1000
z = (poisson_samples$p_1000 - 1000) / sqrt(1000)
hist(z, probability = T, breaks = 5)
summary(z)
x <- seq(-3,2,length.out = 100)
norm_density <- dnorm(x, mean = 0, sd = 1)
lines(x,norm_density, col = "red", lwd = 2, freq = F)


##6.h

library("MASS")
xy <- mvrnorm(10, mu=c(50,60), Sigma = matrix(c(4,3.7,3.7,9),2))
mat_norm <- mvrnorm(n = 1000,mu = c(50, 60), Sigma = matrix(c(4,3.7,3.7,9),2))

#variance of each columns
paste("Covar for col 1"); var(mat_norm) [2] 
paste("Covar for col 2"); var(mat_norm) [3] 

dim(mat_norm)
x <- mat_norm[,1]
y <- mat_norm[,2]

#correlation
corrval <- cor(x,y)
corrval

#plotting correlation
plot(x,y)
model <- lm(y ~ x, data = data.frame(mat_norm))
sum_model <-  summary(model)
sum_model$adj.r.squared
options(digits = 4)
text(x = 52,y=51,label = paste("Correlation val: ", corrval))
text(x = 52,y=50,label = paste("R2 val: ",sum_model$adj.r.squared))

abline(model, col= "red3")


#variance of each columns
paste("Var for col 1"); v1 <- var(x) ; v1
paste("Var for col 2"); v2 <- var(y); v2

paste("sum of var for both cols") ; v1 + v2
paste("Var of sum of both cols"); var(x + y)
print("No, the variances are not equal. Hence, the two samples are dependent ")

#h.4
var(mat_norm)
cor(x,y)
var(x)
var(y)
print("The values are matching.")


#7 Uniform distribution
rand_num <- runif(5,0,max = 1)
rand_num

rand_num <- runif(5,50,max = 100)
rand_num

rand_num <- runif(10000,1,max = 2)
hist(rand_num, main="Histogram for 10k samples")


#8 Exponential distribution
dexp(x = 3,rate = 2)
qexp(p=0.995, rate=2)

par(mfrow=c(1,1))
set.seed(22)
#rate = c(2,10,100)

x_df <-  seq(0,1, length.out=1000)

exp_2 <- pexp(x_df, rate = 2)
exp_10 <- pexp(x_df, rate = 10)
exp_100 <- pexp(x_df, rate = 100)

plot(exp_2, lwd=1, col="red", type="l", xlab="Sample Index", ylab="Probability")
lines(exp_10, col="blue")
lines(exp_100, col="orange")

legend("bottomright", legend = c("lambda = 2", "lambda = 10", "lambda = 100"), 
       fill = c("red","blue", "orange"),
       #lty = c(NA, NA,NA),
       #lwd = c(NA, 2),
       bty = "n")

### Random deviates
rexp(n=4, rate = 4)

## 9 Gamma distribution
par(mfrow=c(1,2))
plot(pgamma(q = x_df,shape = 1, scale = 4), type = "l", col="black",xlab="Sample Index", ylab="Probability", main="Gamma distribution: PDFs with different shape val")

lines(pgamma(q = x_df,shape = 2, scale = 4), type = "l", col="blue")
lines(pgamma(q = x_df,shape = 3, scale = 4), type = "l", col="red")
lines(pgamma(q = x_df,shape = 4, scale = 4), type = "l", col="magenta")


plot(pgamma(q = x_df,shape = 4, scale = 1), type = "l", col="black", xlab="Sample Index", ylab="Probability", main="Gamma distribution: PDFs with different scale val")
lines(pgamma(q = x_df,shape = 4, scale = 2), type = "l", col="blue")
lines(pgamma(q = x_df,shape = 4, scale = 3), type = "l", col="red")
lines(pgamma(q = x_df,shape = 4, scale = 4), type = "l", col="magenta")

##probability density corresponding to x = 6, α = 4 and θ = 1.
prob_density <- dgamma(x = 6,shape = 4, scale = 1)
prob_density


CPDFs_gamma <- pgamma(q = 6,shape = 4, scale = 1)
CPDFs_gamma

#x value which corresponds to a cumulative probability of 0.95
cum_0.95 <- qgamma(p = 0.95,shape = 4,rate = 1)
cum_0.95

par(mfrow=c(1,1))
rand_dev_gamma <- rgamma(n = 10**3, shape = 4, rate = 1)
hist(rand_dev_gamma, col="beige", main="10,000 random deviates ; Gamma distribution")

##10 aChi Square
plot(dchisq(x = x_df, df = 2), col="red",  type="l", ylim = c(0,1), ylab="Probability")
lines(dchisq(x = x_df, df = 3), col="blue",  type="l")
lines(dchisq(x = x_df, df = 5), col="black",  type="l")
lines(dchisq(x = x_df, df = 10), col="magenta",  type="l")

legend("topright", legend = c("df=2","df=3","df=5","df=10"), fill = c("red", "blue", "black", "magenta"))


#10b the probability density for x=6 and 5 degrees of freedom
PDF <- dchisq(x = 6, df = 5)
PDF

cum_pdf <- pchisq(q = 6, df = 10 )
cum_pdf

#85th quantile for this distribution for 6 degrees of freedom
qunt_distri <- qchisq(p = 0.85, df = 6)
qunt_distri


#random varia
hist(rchisq(n = 10**3, df = 3), breaks=30, col="red", xlab="bins", main="Chi Square: histogram of 10,000 random deviates")
text(x=15,y=100, "r=6")

#z = (x- mean(x))**2 / var(x)
z = (x_df- 2)**2 / 1

plot(dchisq(x = z,df = 1), type="l", main = "Chi Square distribution: using normalized x",
     ylab="Probability", xlab="sample index")



## CLT

par(mfrow=c(2,2), mar=c(2,2,2,2))

a <- sample(x = 1:6,replace = T, size = 10**3)
hist(a, probability = T, cex.main=0.8)

b <-  sample(x = 1:6,replace = T, size = 10**3)
hist(a+b,probability = T,cex.main=0.8)

c <-  sample(x = 1:6,replace = T, size = 10**3)
hist(a+b+c,probability = T,cex.main=0.8)

d <-  sample(x = 1:6,replace = T, size = 10**3)
hist(a+b+c+d,probability = T,cex.main=0.8)

e <-  sample(x = 1:6,replace = T, size = 10**3)
sum_tot <- a+b+c+d+e
z = ((sum_tot - mean(sum_tot))) / mean(sum_tot)
hist(a+b+c+d+e, probability = T,cex.main=0.8, breaks=20, col=c(rgb(0.7, 0.7, 0.9, 0.7))) #c(0.1,0.5,1,0.6))
hist(z, probability = T,cex.main=0.8, breaks=20, col=c(rgb(0.7, 0.7, 0.9, 0.7))) #c(0.1,0.5,1,0.6))



##Doubt
# Check the length
mu <- mean(c(a,b,c,d,e))
sd_dev <- sd(c(a,b,c,d,e))
mu
sd_dev

par(mfrow=c(1,1))

norm_dice <- rnorm(n = 1000, mean = mu, sd = sd_dev)
z = ((norm_dice - mean(norm_dice))) / mean(norm_dice)

hist(z, add=T, probability = T, col=rgb(0.1,0.5,1,0.6), breaks = 30)
curve(dnorm(x, mean = mean(z), sd = sd(z)), col = "red", lwd = 2, add = TRUE)


##ROC
library("pROC")
df <- read.csv("learning/Biostat_209/lab10_11/winequality-white.csv", sep=";")


#Create additional columns for quality thresholds
thresholds <- c(6, 7, 8, 9, 10)

for (thresh in thresholds) {
  df[[paste0("good_wine_", thresh)]] <- ifelse(df$quality >= thresh, 1, 0)
}

# Plot ROC curves for each threshold
par(mfrow = c(2, 3))

for (thresh in thresholds) {
  good_wine_col <- paste0("good_wine_", thresh)
  
  roc_obj <- roc(df[[good_wine_col]], df$alcohol)
  
  plot.roc(
    roc_obj,
    main = paste("ROC Curve for Threshold >=", thresh),
    legacy.axes = TRUE,
    ci = TRUE,
    print.auc = TRUE,
    identity.lwd = 2,
    print.thres = TRUE,
    xlim = c(1,0)
  )
}
