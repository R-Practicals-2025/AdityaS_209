library("moments")

# Heart data
## q1 -- import data
file_path = "~/Documents/GitHub/AdityaS_209/Lab5_6_aditya/Heart.csv"
print(paste0("Processing ", file_path))

df <- read.csv(file = file_path, sep=',', header = T)

# q--2 to 4
print(dim(df))
print(colnames(df))

print(rownames(df))
head(df, 30)
summary(df)

# q 5 to 8 categorical variables
# 4 categorical variables are present
# Sex, ChestPain, Thal, AHD

# Get the categorical/levels information
for (i in colnames(df)) {
  if (class(df[[i]]) == 'character') {
    print(paste0("Levels ", i))
    print(levels(df[[i]]))
    print("Variable")
    print(unique(df[[i]]))
  }  
}

# Set levels
df$ChestPain <- factor(df$ChestPain, levels = c("typical", "asymptomatic", "nonanginal", "nontypical"))
levels(df$ChestPain)
df$Thal <- factor(df$Thal, levels = c("fixed", "normal", "reversable"))
df$AHD <- factor(df$AHD, levels = c("No", "Yes"))

# 2.8 levels in each category
# format [class = number of levels, | levels]

for (i in colnames(df)) {
  print(levels(df[[i]]))
}

# Sex = 2 | "0" "1"
# ChestPain = 4 | "typical" "asymptomatic" "nonanginal" "nontypical"
# Thal = 3 | "fixed" "normal" "reversable"
# AHD = 2 | "No" "Yes"

# 3 to 9
print(paste0("Mean Chol: ", mean(df$Chol)))
print(paste0("Mean RestBP: ", mean(df$RestBP)))
print(paste0("Median Chol: ", median(df$Chol)))

# calc_mode function
mode_df <- function(data) {
  freq_tan <- -sort(-table(data))
  mode_value <- names(freq_tan[1])
  return(mode_value)
}

print(paste0("Mode Chol: ", mode_df(df$Chol)))
print(paste0("Standard deviation Chol: ", sd(df$Chol)))
print(paste0("Skewness Chol: ", skewness(df$Chol)))
print(paste0("Kurtosis Chol: ", kurtosis(df$Chol)))

summary(df$Chol)

# q 3.8 to 3.11 Plots

hist(df$Chol, main = "Distribution of Cholestrol levels")
par(mfrow=c(1,3))

##box plot with cholestrol levels among differnet range
boxplot(df$Chol, xlab="Spread of data", ylab = 'Chol', range=0.1, horizontal=FALSE, border=c("blue"), col=c('red'))
boxplot(df$Chol, xlab="Spread of data", ylab = 'Chol', range=0.2, horizontal=FALSE, border=c("blue"), col=c('red'))
boxplot(df$Chol, xlab="Spread of data", ylab = 'Chol', range=0.05, horizontal=FALSE, border=c("blue"), col=c('red'))

par(mfrow=c(1,1))
boxplot(df$AHD, xlab="Spread of data", ylab = 'AHD')
boxplot(df$RestBP, xlab="Spread of data", ylab = 'RestBP')

# q3.11 -- RestBP has the broadest distribution

# q4.1 to 4.3
#filter data with cholestrol > 200 
df_Chol_200 <- df[df$Chol > 200,]
df_row <- df[c(1,3,8,9,13,14,18,21),]

# Separating rows where Sex == 0

which(df$Sex == 0)
df_fem <- df[df$Sex == 0,]

# 4.4
df_new_col <- df$Chol * df$MaxHR / 234

df_subset = df[,c("Chol", "MaxHR", "RestBP")]
df_new <- cbind(df_subset, df_new_col) # joining two data column wise

# 4.5
df_fe <- df[df$Sex == 0,]

# 5

print(df$Sex)
print(df$Sex)

# 5.1 and 5.2
df$Sex <- factor(df$Sex, levels = c(1, 0))
print(levels(df$Sex))

# 5.3
print(nlevels(df$Sex))

# 5.4
df$ChestPain <- factor(df$ChestPain, levels = c("typical", "asymptomatic", "nonanginal", "nontypical"))
print(levels(df$ChestPain))
# 6.1
# Generating levels using gl() function

df$Temperature <- gl(n = 3, k = 2, length = nrow(df), labels = c("Hot", "Cold", "Lukewarm"))
print(df)

# 7
mean_Chol_sex <- tapply(X = df$Chol, INDEX = df$Sex, FUN = mean)  
print(mean_Chol_MaxHR)

mean_Chol_MaxHR_trim <- tapply(df$Chol, df$MaxHR, mean, trim = 0.1)  
print(mean_Chol_MaxHR_trim)

# Trying trimming without tapply function
df_MaxHR <- subset(df, df$MaxHR == 100)
print(df_MaxHR$Chol)
print(mean(df_MaxHR$Chol, trim = 0.20))

# 8
# pmin and pmax
print(pmin(df$Chol, df$RestBP, df$MaxHR, na.rm = TRUE))
print(pmax(df$Chol, df$RestBP, df$MaxHR, na.rm = TRUE))

# 9
# Difference between rank, sort, and order
## 9.1
ranks <- rank(df$Chol)
ranks

sorted <- sort(df$Chol)
sorted
ordered <- order(df$Chol)

Chol_rnk_ord_sor <- data.frame(Chol = df$Chol, ranks, sorted, ordered)
print(Chol_rnk_ord_sor)

## 9.2
ordered_diag <- df$ChestPain[ordered]
ordered_Chol <- df$Chol[ordered]
ord_diag_Chol <- data.frame(ordered_diag, ordered_Chol)

write.csv(ord_diag_Chol, 'lab4_ordered_data.csv')

# 10
# 10.1
filter1 = df[1:6, 3:8] #subsetting the data
filter1

# 10.2
filter1ma = as.matrix(filter1)
print(filter1ma)
print(class(filter1ma))
print(mode(filter1ma))
print(attributes(filter1ma))

# 10.3
newcol = df$MaxHR + df$Chol + df$RestBP

# 10.4 and 5

newcoladded <- df
newcoladded$newcol <- newcol
print("Without cbind:")
print(colnames(newcoladded))

newcoladded2 <- cbind(df, newcol)
print("With cbind:")
print(colnames(newcoladded2))

# 10.5
# Filtering rows 6, 9
filter2 = df[c(6, 9),]
newrowadded = rbind(df, filter2)
print(newrowadded)
