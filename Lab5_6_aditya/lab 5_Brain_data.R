library("moments")


# Brain cancer data
##q1 -- import data
file_path = "~BrainCancer.csv"
print(paste0("Processing ",file_path))

df <-  read.csv(file = file_path, sep=',', header = T)

#q--2 to 4
print(dim(df))
print(colnames(df))

print(rownames(df))
head(df, 30)
summary(df)

#q 5 to 8 categorical variable
# 4 categorical variables are present
# sex, diagnosis, loc, stereo

#get the categorical/levels information
for (i in colnames(df)) {
  if (class(df[[i]]) == 'character') {
    print(paste("Levels: ", levels(df[[i]])))
    print(paste("Variable: ", unique(df[[i]])))  
  }  
}

#Set levels
df$diagnosis <- factor(df$diagnosis, levels = c( "Meningioma", "HG glioma",  "LG glioma"  , "Other"))
levels(df$diagnosis)
df$sex <-  factor(df$sex, levels = c('Female', 'Male' ))
df$loc <-  factor(df$loc, levels = c( "Infratentorial",  "Supratentorial"))
df$stereo <- factor(df$stereo, levels = c("SRS",  "SRT"))


#2.8 levels in each catergory
#format [class = number of levels, | levels ]

for (i in colnames(df)) {
  print(levels(df[[i]]))
}

# Sex =  2 | "Female" "Male"
# diagnosis =  4 | "Meningioma" "HG glioma"  "LG glioma"  "Other"
# loc = 2 |  "Infratentorial" "Supratentorial"
# stereo = 2 | "SRS" "SRT"

#3 to 9
print(paste0("Mean GTV: ", mean(df$gtv)))
print(paste0("Mean time: ", mean(df$time)))
print(paste0("Median GTV: ", median(df$gtv)))

# calc_mode

mode_df <- function(data) {
  freq_tan <- -sort(-table(data))
  mode_value <- names(freq_tan[1])
  return(mode_value)
}

print(paste0("Mode GTV: ", mode_df(df$gtv)))
print(paste0("standard dev GTV: ", sd(df$gtv)))
print(paste0("skewness GTV: ", skewness(df$gtv)))
print(paste0("kurtosis GTV: ", kurtosis(df$gtv)))


summary(df$gtv)


#q 3.8 to 3.11 Plots

hist(df$gtv)
par(mfrow=c(1,3))
boxplot(df$gtv, xlab="Spread of data", ylab  = 'GTV', range=0.1, horizontal=FALSE, border=c("blue"),col=c('red'))
boxplot(df$gtv, xlab="Spread of data", ylab  = 'GTV', range=0.2, horizontal=FALSE, border=c("blue"),col=c('red'))
boxplot(df$gtv, xlab="Spread of data", ylab  = 'GTV', range=0.05, horizontal=FALSE, border=c("blue"),col=c('red'))

par(mfrow=c(1,1))
boxplot(df$ki, xlab="Spread of data", ylab  = 'KI')
boxplot(df$time, xlab="Spread of data", ylab  = 'Time')

#q3.11 -- Time has the broadest distribution

#q4.1 to 4.3
df_gtv_20 <-  df[df$gtv > 20,]
df_row <-  df[c(1,3,8,9,13,14, 18, 21),]

#separating rows by Sex == Female indices

which(df$sex=="Female")
df_fem <- df[c(1,3  ,4  ,6  ,9, 12, 13 ,15, 16, 17, 20 ,21, 23, 24, 25, 30, 31 ,33, 35, 36, 37, 40, 41, 43, 48, 50, 51 ,52 ,55, 59, 60, 61, 63 ,66 ,67 ,69, 70 ,74 ,76 ,77 ,78 ,79 ,80 ,81 ,82),]


#4.4
df_new_col <- df$gtv*df$ki/234

df_subset = df[,c("gtv", "ki", 'time')]
df_new <- cbind(df_subset,df_new_col)

# 4.5
df_fe <- df[df$sex=="Female",]


#5

print(df$sex)
print(df$sex)

#5.1 and 5.2
df$sex <- factor(df$sex, levels = c('Male', 'Female'))
print(levels(df$sex))

#5.3
print(nlevels(df$sex))

#5.4
df$diagnosis <- factor(df$diagnosis, levels = c('Meningioma', 'HG glioma', 'LG glioma'))
print(levels(df$diagnosis))


# 6.1
#generating levels using gl() func
# print(gl(n = 4,k = 3))
# print(gl(n = 4,k = 3,length = 13))

temprature <- gl(n=3, k=2, length = 88, labels = c("Hot", "Cold", "lukewarn"))
# softness <- gl(n=3, k=2, length = 22, labels = c("Hard", "medum", "soft"))
# fac_df <- data.frame(temprature, softness)
df$temperature <-  temprature
print(df)


#7
mean_gtv_ki <- tapply(df$gtv, df$ki, mean) #This function takes data, class and function to apply on data based on its class  
print(mean_gtv_ki)

mean_gtv_ki_trim <- tapply(df$gtv, df$ki, mean, trim=0.1)  #func trim, trims the percentage of values on extremes of values 
print(mean_gtv_ki_trim)

#trying trimming without tapply func
df_ki <- subset(df, df$ki ==100)
print(df_ki$gtv)
mean(df_ki$gtv, trim = 0.20)

#8
#pmin and pmax
print(pmin(df$gtv, df$time, df$ki ))
print(pmax(df$gtv, df$time, df$ki ))


# checking pmin with different data
# x1 <- c(1,2,9,7,10)
# x2 <- c(4,76,54,44,12)
# print(pmin(x1,x2))

#9
#difference between rank, sort and order
##9.1
ranks <- rank(df$gtv)
sorted  <- sort(df$gtv)
ordered <- order(df$gtv)

gtv_rnk_ord_sor <- data.frame(df$gtv, ranks, sorted, ordered)
print(gtv_rnk_ord_sor)

##9.2
ordered_diag <-  df$diagnosis[ordered]
ordered_gtc <-  df$gtv[ordered]
ord_diag_gtc <-  data.frame(ordered_diag, ordered_gtc)
write.csv(ord_diag_gtc, 'lab4 ordered data.csv')

#10
#10.1
filter1 = df[1:6,3:8]
#10.2
filter1ma=as.matrix(filter1)
print(filter1ma)
print(class(filter1ma))
print(mode(filter1ma))
print(attributes(filter1ma))

#10.3
newcol = df$ki + df$gtv + df$time

#10.4 and 5

newcoladded <- df
newcoladded$newcol <-  newcol
print("Without cbind:")
print(colnames(newcoladded))

newcoladded2 <- cbind(df, newcol)
print("With cbind:")
print(colnames(newcoladded))


#10.5
#filtering rows 26,35
filter2 = df[c(26,35),]
newrowadded = rbind(df, filter2)
print(newrowadded)




#11
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0), nrow = 4, ncol = 5)
print(X)
print(rownames(X))
print(colnames(X))

rownames(X) <- rownames(X, do.NULL = F, prefix = 'Trial-')
print(colnames(X))
print(rownames(X))

drugs <-  c("aspirin", "paracetamol", "nurofen", 'hdeex', 'placebo')
colnames(X) <- drugs
print(X)

dimnames(X) <- list(NULL,paste("drugs", 1:5, sep=""))
print(colnames(X))
print(X)

#12
#calculations on rows and cols of a mat
mean(X[,5])
var(X[4,])
rowSums(X)
apply(X, 1, sum) #other method of row sum


#method of row sum
colSums(X)
apply(X, 2, sum) #other method of row sum


# sqrt(X)
# apply(X,2, sqrt)

#12.5 to 12.6
#Calculating mean
rowMeans(X)
apply(X,1, mean)

colMeans(X)
apply(X,2, mean)

# 12.7
group = c("A", "B", "B", "A")
rowsum(X, group)

row(X) #indicates row number
col(X) #indicates col number

list(group[row(X)], col(X))
tapply(X,list(group[row(X)], col(X)), sum)
aggregate(X, list(group), sum)


#12.8
print(apply(X,2,sample))
X <- rbind(X, apply(X,2, mean))
print(X)

X <-  cbind(X, apply(X,1,var))
heading <-  c(paste("drug.", 1:5,sep=""), 'var')
heading
#colnames(X) <-  heading #other way of adding colnamess)
cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1], dim(eg_sweep)[1], dim(eg_sweep)[2])))
cols.means

nrow=dim(eg_sweep)[1]
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("method 1")
print(eg_sweep_alt)

#applying sweep func
eg_sweep_alt2 <-  sweep(eg_sweep,2,cols)
print(eg_sweep_alt2)


#13.4
#sapply -- used for vectors
eg_sapply <- sapply(3:7, seq)
print(attributes(eg_sapply))
print(class(eg_sapply))


#14


#15
#lists
apples <- c(4,4,5,2.5,1,3.9)
cheese <- c(3.2-4.5i,12.8+2.2i)
oranges <- c(T,T,F)
chalk <- c("limestone", 'marl', 'oolite', 'CaCO3')
# data.frame(apples,oranges, chalk)

#access items
items <-  list(apples,oranges, chalk)
print(items[1])
print(items[[1]])
print(items[[3]][3])
print(items[3][3])

# Q. What happens if you try to create a dataframe of the 4 objects given above? Also
# does items[3] work the same as items[[3]] ? What’s the difference?
# Ans - 1. In order to create a dataframe of 4 objects, there is a need of adding another vector into the list of objects
# 2. No, items[3] does not work the same as items[[3]]. items[3] will acceess the items in the 3rd list while items[[3]] access the nested list of the list

#14
# Read the data from pgfull.txt
data <- read.table("../learning/Biostat_209/lab3/pgfull.txt", header = TRUE)
species <- data[, 1:54]

max_indices <- max.col(species)
print(max_indices)

# Extract the species names corresponding to the max value in each row
max_species <- names(species)[max_indices]

species_frequency <- table(max_species) # Create a frequency table of species names
print(species_frequency)
min_indices <- max.col(-species)
print(min_indices)

# Extract species names corresponding to the min value in each row
min_species <- names(species)[min_indices]

# Create a frequency table of species names for minimum values
min_species_frequency <- table(min_species)
print(min_species_frequency)


#15.2
#lapply - applicable to lists
items <-  list(first=apples,second=oranges, third=chalk)
print(names(items))
print(items$fourth) #No 4th object
print(class(items)) #shows the class of object

#15.3
lapply(items,length)
lapply(items,class)
lapply(items,mean)

#15.4
print(summary(items))
print(str(items))

#difference between class and mode attributes

# Get or set the ‘mode’ (a kind of ‘type’), or the storage mode of an R object.

#R possesses a simple generic function mechanism which can be used for an object-oriented \
#style of programming. Method dispatch takes place based on the class of the first argument to the generic function.

print(str(items))



