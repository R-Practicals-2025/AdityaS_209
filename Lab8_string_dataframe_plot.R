library(stringr)
library(readr)

## EXERCISES
#1 Palindrome

num = 456785
num2 = 12321

#function for checking palindrome
checkpalindome <- function(n){
  n <- as.character(n)
  rever <- sapply(lapply(strsplit(x = n, split = NULL), rev), paste, collapse="")
  ifelse (rever==int2, yes = paste(int2," is a palindrome"), no = "This is not a palindrome")
}  

checkpalindome(num2)

#2 #Sub string

str = "seemerightnow"
t(sapply(lapply(strsplit(str,split = ""), paste), paste))

##printing 
substring(str,first = 1,last = 3)
substring(str,first = 4,last =5)
substring(str,first = 6,last =10)


#3 GC content 
nucl <- "ATTGCGCATAGTCCGGG"

#fraction table
freq_tab <-  table(sapply(strsplit(nucl, split = ""),paste))
seq_split <- strsplit(nucl, split = "")
total <- sum(freq_tab)
C_fraction  <-  sum(seq_split[[1]]=="C") / total
G_fraction  <-  sum(seq_split[[1]]=="G") / total
paste("C fraction :",C_fraction)
paste("G fraction :",G_fraction)


#4Palindrome with complementary sequence
seq_dna="TGGATCCA"
given_compl_seq = "ACCTAGGT"

splt_sseq <- strsplit(seq_dna, NULL)[[1]]
splt_sseq

##create empty string
compl_seq <- ""
for (i in 1:nchar(seq_dna)) {
  if (splt_sseq[i]== "A") {compl_seq <- c(compl_seq,"T")  }
  if (splt_sseq[i]== "T") {compl_seq <- c(compl_seq,"A")  }
  if (splt_sseq[i]== "G") {compl_seq <- c(compl_seq,"C")  }
  if (splt_sseq[i]== "C") {compl_seq <- c(compl_seq,"G")  }
}

compl_seq  <- paste(compl_seq, collapse = "")
compl_seq

check_palindrome_dna <- function(strand1, strand2) {
  rever <- paste(rev(strsplit(strand2, NULL)[[1]]), collapse = "")
  if (rever == strand1) {
    return(paste(strand1, "is a palindrome"))
  } else {
    return("This is not a palindrome")
  }
}

#reverse the complementary sequence
check_palindrome_dna(seq_dna,compl_seq)



#5  find the largest
string ="She sells hundreds of sea oysters on the sea shore."
words <- strsplit(string,split = " ")[[1]]

check_word_length_ord <- function(sentence, index=1){
  words <- strsplit(string,split = " ")[[1]]
  sorted_array <- sort(nchar(words),decreasing = T)
  
  for (i in 1:length(words)) {
    word_len = length(lapply(strsplit(words[i], NULL), paste)[[1]])
    if (sorted_array[index]==word_len) {
      x <- paste(words[i], "This is the",index, "rd largest words")
      print(x)    
    }
  }
}

check_word_length_ord(sentence = string, index=2)


#6
df = as.data.frame(read.table(file='~/NR_Aditya/course_work/sem2/Biostatistics/worldfloras.txt',header=TRUE))
dim(df)
class(df$Country)
df$Continent <- as.factor(x = df$Continent)
table(df$Continent)

Continent_list <- levels(df$Continent)
Continent_list

### saving subset dataframe in a list
df_new <- list() #create a new list

for (i in 1:length(Continent_list)) {
  df_name <- paste(Continent_list[i])
  
  var_name <- paste(Continent_list[i])
  assign(df_name , as.data.frame(df[which(df$Continent == var_name),]))
}

par(mfrow=c(3,4))


library(moments)


for (i in 1:length(Continent_list)) {
  df_name <- paste(Continent_list[i])
  
  var_name <- Continent_list[i]
  df_subs <- df[which(df$Continent==var_name),]
  
  par(mfrow=c(1,3))
  boxplot(df_subs$Flora, main=df_name)
  cat("\n")
  print(paste(var_name))
  ##statistics
  #mean
  mu = mean(df_subs$Flora)
  print( paste("Mean: ",mu))
  
  #std dev
  std_dev = sd(df_subs$Flora)
  print( paste("Std dev: ",std_dev))
  
  #skewness
  skw = skewness(df_subs$Flora)
  print( paste("skewness: ",skw))
  
  
  #kurtosis
  kut = kurtosis(df_subs$Flora)
  print( paste("kurtosis: ",kut))
  
  boxplot(df_subs$Population, main=paste(df_name,"Population distribution"))
  hist(df_subs$Population, main=paste(df_name,"Population distribution"))
}




#7 Text to dataframe

# Remove empty rows
df <- read.table("~/NR_Aditya/course_work/sem2/Biostatistics/HumanBones.txt", sep = "\n", stringsAsFactors = FALSE, fill = TRUE, header = FALSE)
df <- df[df$V1 != "", , drop = FALSE]  # Remove empty rows

# Assign categories
#separating which do not contains opening parenthesis
df$category <- cumsum(x = !grepl("\\(", x = df$V1))
df$category <- ave(x = df$V1, ... = df$category, FUN = function(x) x[1])  ## aggregating and grouping data from each category
##function -- > returns the first element

df
# Extract bone names and counts
df$bone <- gsub(" \\(.*\\)", "", df$V1) # replacing anything inside parenthesis and saving in another column
df
df$count <- as.numeric(gsub(pattern = ".*\\((\\d+)\\)", replacement = "\\1", x = df$V1)) #matching digits and replacing with 1
df
df$count[is.na(df$count)] <- 1  # Assign 1 if count is missing
df

# Final formatting
df <- df[, c("category", "bone", "count")] #setting column names
df

# (8) Category with max bones
freq <- aggregate(count ~ category, df, sum)
freq
max_category <- freq[which.max(freq$count), ]
max_category
par(mfrow=c(1,1))
barplot(freq$count, names.arg = freq$category, las = 2)

#9 Subset legs & bones > 5 letters
legs <- subset(df, category == "Legs")
legs <-  df[df$category == "Legs",]

long_bones <- legs$bone[nchar(legs$bone) > 5]
print(long_bones)

#10 Bones starting with "M" & replace 'a' with 'A'
m_bones <- grep(pattern = "^M", x = df$bone, value = T)
m_bones <- gsub(pattern = "a", replacement = "A", m_bones)
print(m_bones)

#11 Bones ending with "e" to lowercase
e_bones <- grep("e$", df$bone, value = T)
e_bones <- tolower(e_bones)
print(e_bones)

#12 Bones with two "o"s
oo_bones <- grep("o.*o", df$bone, value = T)
print(oo_bones)

