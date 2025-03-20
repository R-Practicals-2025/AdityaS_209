##Lab 9


#1 Plot the point (2,4) with square point character type and magenta color
plot(2, 4, pch=15, col="magenta", main="Single Point Plot")

#2 Plot sin(x) and cos(x) 
x <- seq(-pi, pi, length.out=10)
plot(x, sin(x), type="b", col="blue", pch=8, main="Sine and Cosine Functions", xlab="x", ylab="Function Value")
points(x, cos(x), type="b", col="red", pch=4)
legend("topright", legend=c("sin(x)", "cos(x)"), col=c("blue", "red"), pch=c(8, 4))

#3 Bar graph as per Fig. 4.2.1 (Example placeholders)
x <- c(1,2,3,4,5,6,7,8)
y <- c(0.2088,0.1582,0.1313,0.1313,0.1953,0.1246,0.0135,0.0370)

# Create bar plot and store bar positions
bar_positions <- barplot(y, ylim=c(0, 0.25),
                         names.arg=x, width=0.1, space=0.4,
                         ylab="Probability",
                         xlab=expression(italic(x)~"(number of assistance programs)"),
                         axes=FALSE, col="gray80", border="black")

# Add Y-axis with small tick marks inside
axis(2, at=seq(0, 0.25, by=0.05), las=1, tck=0.03)  

# Add X-axis with small tick marks
axis(1, at=bar_positions, labels=x, tick=TRUE, tck=0.03)  

# Draw a clear X-axis line
abline(h=0, col="black", lwd=2)

#4 2x3 Grid of graphs
par(mfrow=c(2,3))
plot(x, cos(x), type="l", col="red", main="x vs cos(x)")
plot(x, (x**2 /3) + 4.2, type="o", col="violet", lwd=2, lty=1, main="x vs x^(2/3) + 4.2")
hist(rbinom(1000, 12, 0.3), main="Binomial (p=0.3)", col="lightblue")
hist(rbinom(1000, 12, 0.8), main="Binomial (p=0.8)", col="pink")

x_seq <- seq(1, 10, by=0.5)
y_vals <- 50 * x_seq / (x_seq + 2)
plot(x_seq, y_vals, type="h", col=rep(c("blue", "orange")), main="Alternating Colors")
plot(x, log(x), type="s", col="orange", main="x vs log(x)")


#reset the graph subplot dimen
par(mfrow=c(1,1))

#5- Placeholder for slide recreation
y=c(2,7,5,10,8,10)
x = c(1,3,5,7,9,10)
plot(x, y, type = 'o', main="This is a graph" ,col.main = "blue",col="violet", pch=19, lty=3, lwd=2, xlab = "Time", ylab = "Performance")
label = x
text(x+0.5,y,labels = label, col="red")

#6 - Histogram of hypergeometric distribution
hist(rhyper(1000, 50, 450, 30), main="Hypergeometric Distribution", col="lightgreen")

#7- Convergence of Hypergeometric to Binomial (3x3 Grid)  ##not working 
par(mfrow=c(3,3), mar=c(2,2,2,2))
n_vals <- seq(10, 500, length.out=9)
for (n in n_vals) {
  hist(rhyper(100, 5, 50-5, round(n)), main=paste("Hypergeom, n=", round(n)), col="cyan", freq=FALSE)
  lines(density(na.omit(rbinom(1000, round(n), 5/50))), col="red")
}



#8- Poisson distributions 
par(mfrow=c(1,1))
x <- 0:60
plot(x, dpois(x, 3), type="l", col="blue", lwd=2, ylim=c(0,0.2), main="Poisson Distributions")
lines(x, dpois(x, 20), col="red", lwd=2)
lines(x, dpois(x, 45), col="green", lwd=2)
legend("topright", legend=c("Lambda=3", "Lambda=20", "Lambda=45"), col=c("blue", "red", "green"), lwd=2)

#9- Height and weight analysis
heights_weights <- read.csv("~/Downloads/SOCR-HeightWeight.csv")
hist(heights_weights$Height, main="Height Distribution", col="gray")
height_mean <- mean(heights_weights$Height)
paste("Mean:",height_mean)
height_sd <- sd(heights_weights$Height)
paste("Std dev:", height_sd)


##Perform PDF ##not done yet
hist(heights_weights$Weight, main="Weight Distribution", col="gray")
weight_mean <- mean(heights_weights$Weight)
weight_sd <- sd(heights_weights$Weight)

curve(dnorm(x, height_mean, height_sd), add=TRUE, col="blue")
curve(dnorm(x, weight_mean, weight_sd), add=TRUE, col="red")

#10- Uniform distribution 
# Function to compute the PDF of Uniform Distribution

uniform_pdf <- function(x, a, b) {
  if (x < a || x > b) {
    return(0)  # If x is outside [a, b], probability is 0
  } else {
    return(1 / (b - a))  # Uniform density function
  }
}

# Function to compute the CPDF (Cumulative Probability Density Function)
uniform_cpdf <- function(x, a, b) {
  if (x < a) {
    return(0)  # If x is less than a, probability is 0
  } else if (x > b) {
    return(1)  # If x is greater than b, probability is 1
  } else {
    return((x - a) / (b - a))  # Cumulative probability within the range
  }
}

# Function to plot the PDF of the Uniform Distribution with vertical shading
plot_uniform_pdf <- function(a, b, shade_up_to) {
  x_vals <- seq(a - 1, b + 1, length.out = 100)
  pdf_vals <- sapply(x_vals, function(x) uniform_pdf(x, a, b))
  
  plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
       main = paste("Uniform PDF: U(", a, ",", b, ")"),
       xlab = "x", ylab = "Density", ylim = c(0, 1.2))
  
  # Shade the region under the curve using vertical line segments
  shade_x_vals <- seq(a, shade_up_to, length.out = 100)
  for (x in shade_x_vals) {
    lines(c(x, x), c(0, uniform_pdf(x, a, b)), col = rgb(0, 0, 1, 0.3), lwd = 1)  # Vertical shading
  }
}

# Function to plot the CPDF of the Uniform Distribution with vertical shading
plot_uniform_cpdf <- function(a, b, shade_up_to) {
  x_vals <- seq(a - 1, b + 1, length.out = 100)
  cpdf_vals <- sapply(x_vals, function(x) uniform_cpdf(x, a, b))
  
  plot(x_vals, cpdf_vals, type = "l", col = "red", lwd = 2,
       main = paste("Uniform CPDF: U(", a, ",", b, ")"),
       xlab = "x", ylab = "Cumulative Probability")
  
  # Shade the region under the curve using vertical line segments
  shade_x_vals <- seq(a, shade_up_to, length.out = 100)
  for (x in shade_x_vals) {
    lines(c(x, x), c(0, uniform_cpdf(x, a, b)), col = rgb(1, 0, 0, 0.3), lwd = 1)  # Vertical shading
  }
}

# Execute plotting functions
par(mfrow = c(1, 2))  # Arrange two plots side by side
plot_uniform_pdf(1, 2, 1.5)  # Shade region under PDF up to x = 1.5
plot_uniform_cpdf(1, 2, 1.5)  # Shade region under CPDF up to x = 1.5



# Ex 11,12,13

exponential_pdf <- function(x, lambda) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    if (x[i] < 0) {
      result[i] <- 0
    } else {
      result[i] <- lambda * exp(-lambda * x[i])
    }
  }
  return(result)
}

exponential_cpdf <- function(x, lambda) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    if (x[i] < 0) {
      result[i] <- 0
    } else {
      result[i] <- 1 - exp(-lambda * x[i])
    }
  }
  return(result)
}

gamma_pdf <- function(x, alpha, theta) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    if (x[i] < 0) {
      result[i] <- 0
    } else {
      result[i] <- (x[i]^(alpha - 1) * exp(-x[i] / theta)) / (gamma(alpha) * theta^alpha)
    }
  }
  return(result)
}

gamma_cpdf <- function(x, alpha, theta) {
  return(pgamma(x, shape = alpha, scale = theta))
}

chisq_pdf <- function(x, df) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    if (x[i] < 0) {
      result[i] <- 0
    } else {
      result[i] <- (x[i]^(df/2 - 1) * exp(-x[i]/2)) / (2^(df/2) * gamma(df/2))
    }
  }
  return(result)
}

chisq_cpdf <- function(x, df) {
  return(pchisq(x, df))
}

plot_exponential <- function(lambda, shade_up_to) {
  x_vals <- seq(0, 5, length.out = 100)
  pdf_vals <- sapply(x_vals, function(x) exponential_pdf(x, lambda))
  cpdf_vals <- sapply(x_vals, function(x) exponential_cpdf(x, lambda))
  
  plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
       main = paste("Exponential PDF (λ =", lambda, ")"),
       xlab = "x", ylab = "Density")
  
  shade_x_vals <- seq(0, shade_up_to, length.out = 100)
  for (x in shade_x_vals) {
    y_val <- exponential_pdf(x, lambda)  
    if (y_val > 0) {  
      segments(x0 = x, y0 = 0, x1 = x, y1 = y_val, 
               col = rgb(0, 0, 1, 0.6), lwd = 1)
    }
  }
  
  plot(x_vals, cpdf_vals, type = "l", col = "red", lwd = 2,
       main = paste("Exponential CPDF (λ =", lambda, ")"),
       xlab = "x", ylab = "Cumulative Probability")
}

plot_gamma <- function(alpha, theta, shade_up_to) {
  x_vals <- seq(0, 40, length.out = 100)
  pdf_vals <- gamma_pdf(x_vals, alpha, theta)
  cpdf_vals <- gamma_cpdf(x_vals, alpha, theta)
  
  plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
       main = paste("Gamma PDF (α =", alpha, ", θ =", theta, ")"),
       xlab = "x", ylab = "Density")
  
  shade_x_vals <- seq(0, shade_up_to, length.out = 50)
  for (x in shade_x_vals) {
    y_val <- gamma_pdf(x, alpha, theta)  
    if (y_val > 0) {  
      segments(x0 = x, y0 = 0, x1 = x, y1 = y_val, 
               col = rgb(0, 0, 1, 0.6), lwd = 1)
    }
  }
  
  plot(x_vals, cpdf_vals, type = "l", col = "red", lwd = 2,
       main = paste("Gamma CPDF (α =", alpha, ", θ =", theta, ")"),
       xlab = "x", ylab = "Cumulative Probability")
}

plot_chisq <- function(df, shade_up_to) {
  x_vals <- seq(0, 50, length.out = 100)
  pdf_vals <- sapply(x_vals, function(x) chisq_pdf(x, df))
  cpdf_vals <- sapply(x_vals, function(x) chisq_cpdf(x, df))
  
  plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
       main = paste("Chi-square PDF (df =", df, ")"),
       xlab = "x", ylab = "Density")
  
  shade_x_vals <- seq(0, shade_up_to, length.out = 200)
  for (x in shade_x_vals) {
    y_val <- chisq_pdf(x, df)  
    if (y_val > 0) {
      segments(x0 = x, y0 = 0, x1 = x, y1 = y_val, 
               col = rgb(0, 0, 1, 0.6), lwd = 1)
    }
  }
  
  plot(x_vals, cpdf_vals, type = "l", col = "red", lwd = 2,
       main = paste("Chi-square CPDF (df =", df, ")"),
       xlab = "x", ylab = "Cumulative Probability")
}

# Set up 3x2 grid layout
par(mfrow = c(3, 2))

# Execute functions with shading applied using segments()
plot_exponential(10, 2.8)  # λ = 10, shade up to x = 2.8
plot_gamma(5, 3, 10)       # α = 5, θ = 3, shade up to x = 10
plot_chisq(20, 1.0)        # df = 20, shade up to x = 1.0

# Reset layout
par(mfrow = c(1, 1))



#11 Exponential distribution shading
x <- seq(0, 5, length=100)
y <- dexp(x, rate=10)
plot(x, y, type="l", col="red", main="Exponential Distribution")
polygon(c(0, seq(0, 2.8, length=100), 2.8), c(0, dexp(seq(0, 2.8, length=100), 10), 0), col=rgb(1,0,0,0.3))

# (12) Gamma distribution
x <- seq(0, 20, length=100)
y <- dgamma(x, shape=5, scale=3)
plot(x, y, type="l", col="purple", main="Gamma Distribution")

# (13) Chi-square distribution shading
x <- seq(0, 40, length=100)
y <- dchisq(x, df=20)
plot(x, y, type="l", col="brown", main="Chi-Square Distribution")
polygon(c(0, seq(0, 1, length=100), 1), c(0, dchisq(seq(0, 1, length=100), df=20), 0), col=rgb(0.6,0.3,0.1,0.3))
