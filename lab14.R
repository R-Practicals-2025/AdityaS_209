cat('lab14')
cat("Error bars")


means = c(20.34,19.49,25.68)
stderr = c(0.83,1.51,1.39)


bar_plts <- barplot(means, names.arg = c("A","B","C"), col="lightblue", main = "Errors on bar plot", ylim=c(0, max(means+stderr)+2))
arrows(x0 = bar_plts, y0 = means+stderr, x1 = bar_plts, y1 = means-stderr, length=0.06, angle=90, code = 3, col='black' )

x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

plot(x,y, xlab='concentration', ylab = 'optical activity',main = "Errors bars on data points", ylim = c(0, max(y)+5), xlim=c(0, max(x)+5))
arrows(x0 = x, y0 = y+errors, x1 = x, y1 = y-errors, length=0.06, angle=90, code = 3, col='black' )

x = c(10,20,30,40,50,60,70,80,90,100)
y=c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
covariance <- cov(x,y)
corr <- cor(x,y)
cat("covariance: ",covariance, '\nCorrelation: ', corr )

cat("Correlation for Longey dataset")
cor(longley) 


#II

num = 10**3
muzero = 2
sd = 0.18
x <- rnorm(n = num,mean = mu,sd = sd)


alpha = 0.05 #the significance level


x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
      137.4, 145.6, 135.6, 135.4, 121.5)

sort(pnorm(x, mean=124.6, sd = 14.5))


library(BSDA)
# One-sample Z-test
z_test <- z.test(x, mu = 124.6,sigma.x = 14.5, conf.level = 0.05)
z_test

# Print the result
print(z_test)

null = "less_than_or_equal"
  #a string indicating type of null hypothesis.
  #equal, less_than_or_equal or more_than_or_equal.
  
one_sample_Ztest <- function(x,sd_pop,mu_pop, alpha,null="less_than_or_equal") {
  mu_sample<- mean(x)
  num <- length(x)
  z = ((mu_sample - mu_pop)) / (sd_pop/sqrt(num))
  
  #two tailed test
  p_val <- 2 *(1-pnorm(z))
  p_cutoff = alpha
  
  conclusion <- ""
  if (null=="less_than_or_equal" && p_val <= p_cutoff) {
    conclusion <- "Reject Null hypothesis" } 
  else if (null=="less_than_or_equal" && p_val >= p_cutoff) {
      conclusion <- "Accept Null hypothesis"}

  if ( null == "equal" && p_val == p_cutoff) {
    conclusion  <- "Reject Null hypothesis"}
  else if (null == "equal" && p_val != p_cutoff) {
    conclusion <- "Accept Null hypothesis"}
  
  if (null == "more_than_or_equal" && p_val >= p_cutoff) {
    conclusion  <- "Reject Null hypothesis"}
  else if (null == "more_than_or_equal" && p_val <= p_cutoff) {
    conclusion <- "Accept Null hypothesis"}
  
  #Summary
  cat(conclusion,"\nObserved pval: ",p_val," p val cut-off:",alpha)
  cat("\nz = ",z)
}

#μ0 of 124.6 and σ = 14.5 with 0.05 significance level.
one_sample_Ztest(x = x,sd_pop = 14.5, mu_pop = 124.6, alpha = 0.05,null = "less_than_or_equal")


#One sample t-test
one_sample_t_test <-  function(x,muzero,alpha,null){
  '
  t-test is performed if the 
  '
  mu_sample <- mean(x)
  sd_sample = sd(x)
  num <- length(x)
  t_obser = ((mu_sample - muzero)) / (sd_sample/sqrt(num))
  
  #two tailed test
  test_type <- ""
  t_critical <- qt(1-alpha , df = num-1)
  p_val <-  pt(t_obser,df = num -1)
  conclusion <- ""
  if (null=="less_than_or_equal" ) {
    conclusion <- ifelse( t_critical <= t_obser, "Reject Null hypothesis", "Accept Null hypothesis") 
    test_type <- "One tailed t-test"} 
  
  else if ( null == "equal")
  {    conclusion <- ifelse( t_critical == t_obser, "Reject Null hypothesis", "Accept Null hypothesis") 
  test_type <- "Two tailed t-test"}
  
  else if (null == "more_than_or_equal" && t_obser <= t_critical) {
    conclusion  <- ifelse( t_critical >= t_obser, "Reject Null hypothesis", "Accept Null hypothesis") 
    test_type <- "One tailed t-test"} 
  
  #Summary
  cat("Test ->",test_type,"\n", conclusion,"\nObserved t value: ",t_obser," t critical value:",t_critical, "\np val",2*(1 - p_val),
      "\nmean", mu_sample)
}

x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)
one_sample_t_test(x=x,muzero=mu_population,alpha=0.05,null="less_than_or_equal")


##3
#proportion test

x = 710; n = 2600;p = 0.25 ; alternative="greater"
binom.test(x,n,p,alternative)
prop.test(x,n,p,alternative,correct = T)


##4
x = c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)


one_sample_variance_test <- function(x,test_sigma,alpha, null="two tailed"){
  df <- length(x) -1
  variance_sample <-  var(x)

  #calculate test statistics
  chi_sq <-  df*(variance_sample)/ test_sigma**2 #n = sample size   s**2 = sample variance test_sigma**2 = hypothesized population variance

  null = "two tailed"
  if (null == "two tailed") {
    pval <- 2*pchisq(chi_sq, df = df)
    ulim <-   qchisq(1- alpha, df = df)
    llim <-   qchisq(alpha, df = df)
  }
  
  conclusion <- ifelse(pval <= alpha, "Reject Null hypothesis", "Accept Null hypothesis")
  
  if (null == "One tailed") {
    pval <- pchisq(chi_sq, df = df)
    ulim <-   qchisq(1- alpha, df = df)
    llim <-   qchisq(alpha, df = df)
  }
  conclusion <- ifelse(pval >= alpha, "Reject Null hypothesis", "Accept Null hypothesis")
  
  #Summary
  cat("Test ->",null,"\n", conclusion,"\nObserved p value: ",pval," p cutoff value:",alpha,"\nSample variance", variance_sample, "\ndf", df)  
}


one_sample_variance_test(x,test_sigma=29,alpha=0.05, null="two tailed")


#5
x = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)
wilcox_res <- wilcox.test(x,y=NULL,alternative,mu=0,paired=FALSE, exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
wilcox_res


#III 1

pop1 <- c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,                 
           246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
                 174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,                 
                 231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
                 173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
                 218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)

pop2 = c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
        206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
        233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
        178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
        243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
        267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)


sigma_x1 = 24.6; sigma_x2 = 27.8; alpha = 0.05
two_sample_Z_test <- function(x1,x2,sigma_x1,sigma_x2,alpha,null="different mean") {

  mu_1 <- mean(x1)
  mu_2 <- mean(x2)
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  z = (mu_1-mu_2) / sqrt((sigma_x1**2 /n1) + (sigma_x2**2 /n2))
  
  pval <- pnorm(z)
  
  conclusion <- ""
  if (null=="different mean" ) {
    conclusion <- ifelse( pval <= alpha, "Reject Null hypothesis", "Accept Null hypothesis") 
    test_type <- "Two sample z-test"} 
  
  
  #Summary
  cat("Null Hypothesis ->",null,"\n\n", conclusion,"\n\nObserved p value: ",pval,
      "p cutoff value:",alpha,"\nMean population 1:", mu_1 ,
      "\nMean population 2:",mu_2)  }

two_sample_Z_test(x1 = pop1,x2=pop2,sigma_x1=sigma_x1,sigma_x2,alpha)


#III 2 Welsch’s test:
'a statistical test used to compare the means of two 
independent groups when the variances and sample size of
the two groups are not equal'

#7a
Xvr=c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar=c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)
t.test(x=Xvar,y=Yvar,alternative="two.sided",conf.level=0.95)

#7b
data_before = c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after = c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

length(data_after); length(data_before)
var(data_after);var(data_before)

#Data has same length and unequal variance

t.test(x=data_before,y=data_after,alternative="two.sided",conf.level=0.95)
# No, we do not need to add mean in the data


#3 Two-sample proportion test
'In a health survey, 520 out of 600 men and 550 out of 600 women
questioned said they use antibiotics whenever fever continues for more than 2 days.
We want to test whether there is a significant difference in the fraction of men and
women who start taking antibotics after 2 days of fever.'

#Create dataframe
df = data.frame(c(520,550), c(600-520,600-550), row.names = c("men", "women"))
colnames(df) <- c("more_than_2days","less_than_2days")
df

#split the dataset
x = df[,1]
n1 = sum(df[1,])
n2 = sum(df[2,])

prop.test(x,n=c(n1,n2),p=NULL,alternative="two.sided",correct=TRUE)


#3b Fisher test
# Higher-income Lower-income
# Tobacco abuse 11 17
# No abuse 42 39

df = data.frame(c(11,17), c(42,39), row.names = c("Tobacco abuse", "No abuse"))
colnames(df) <- c("Higher-income","Lower-income")

fisher.test(df,alternative="two.sided",conf.int=TRUE,conf.level=0.95)

#4 Two-sample variance test

alpha = 0.05
x = c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
      1047.0,987.8,1051.0,885.2,
      1049.5,1098.2,1001.5,1011.1,991.6)

y = c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
      1012.3, 1040.7, 1099.5,
      1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)


two_sample_variance_test <- function(x,y,alpha) {
  null = "two tailed F-distribution test"
  var_x = var(x)
  var_y = var(y)
  
  df_1 = length(x) -1
  df_2 = length(y) -1
  
  test_stat = var_x/var_y
  llim  = qf(alpha,df1 = df_1,df2 = df_2)
  ulim = qf(1-alpha, df1 = df_1,df2 = df_2)
  conclusion <- ifelse(llim <= test_stat && test_stat <= ulim, "Accept null hypothesis","Reject null hypothesis")  
  pval <- pf(test_stat,df_1,df_2)
  
  cat("Null Hypothesis ->",null,"Test statistics vale:",test_stat,"\n\n", conclusion,"\n\nObserved p value: ",pval,
      "p cutoff value:",alpha,"\nLower limit :", llim ,
      "\nUpper limit:",ulim)  
}

two_sample_variance_test(x,y,alpha)


#(5) Wilcoxon signed rank test
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <-  c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

wilcox.test(x=Pre_therapy, y = Post_therapy,alternative ="two.sided", conf.level = 0.95,paired = TRUE, correct = TRUE)


drug <-  c(31.7,75.0,101.1,60.5,62.8,59.3,58.9,91.3,99.1,52.0,39.1)
placebo <-c(59.3,72.7,100.5,64.7,69.0,72.7,69.6,97.4,100.6,65.1,65.7)

wilcox.test(x=drug, y = placebo)


#(7) Kruskal Wallis test:

Group_1 <- c(220,214, 203, 184, 186 ,200 ,165)
Group_2 <- c(262, 193, 225 ,200 ,164 ,266 ,179)
Group_3 <- c(272, 192, 190, 208, 231, 235, 141)
Group_4 <- c(190, 255, 247, 278, 230, 269, 289)

df <- data.frame(Group_1,Group_2,Group_3,Group_4)
alpha = 0.05


summary_krus <- kruskal.test(df)
pval <- summary_krus$p.value
conclusion <- ifelse(alpha <= pval, "Accept null hypothesis","Reject null hypothesis")  
cat(conclusion, pval)

#(8) Chi-square GoF test:

Obs <- c(32, 82, 77, 49)
Exp <-  c(40,80,80,40)

df <- length(Obs) -1
df
X_2 = sum((Obs-Exp)**2/ Exp)

llim <- qchi(0.05, df)
ulim <- qchi(0.95, df)
pval <- pchi(X_2,df)
pval

conclusion <- ifelse(alpha <= 1-pval && 1-pval <= 1-alpha, "Accept null hypothesis","Reject null hypothesis")  
cat(conclusion,": p val", 1-pval)

#(1) ANOVA test
#(a)
df <- read.csv("~/learning/Biostat_209/lab14/titanic.csv", sep=",")
head(df)

#Encoding
df$passenger_class <- as.integer(factor(df$passenger_class))

par(mfrow=c(3,1))

#PLotting histogram
for (i in 1:3) {
  hist(x = df$age[df$passenger_class==i], xlab="Passenger class", 
       col="lightblue", main=paste("Histogram of passenger class ",i))
}

#Summary anova 
anova_res <- aov(age~passenger_class,data = df)
summary(anova_res)


#b)
library("dplyr")
titanic_by_passenger_class <- group_by(df,passenger_class)
summary <- summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),group_sd=sd(age,na.rm=TRUE))
summary
cat("Yes, the standard deviations similar between the groups.")


#(c) fit the ANOVA model

"Alternate Hypo - mean age is not same across passenger classes."
df$passenger_class <- as.factor(df$passenger_class)
lmresults <- lm(age~passenger_class, data=df)
lm_fitanova <- anova(lmresults)
lm_fitanova

 cat('Conclusion:\nThe observed p-val = 2.2e-16 *** which is < 0.05. Thus we reject null hypothesis. Concluding, there is atleast one passenger class that has different age distribution.')

# (d)
'A Tukey-Kramer’s test tests the null hypothesis that there is no
difference between the population means of all pairs of groups.'

TukeyHSD(aov(lmresults))


#(e)
kruskal.test(age~passenger_class,data=df)

#(2) Cuckoo egg size problem
df <- read.csv("~/learning/Biostat_209/lab14/cuckooeggs.csv")
colnames(df)
#encoding
df$enc_host_spec <- as.integer(as.factor(df$host_species))
df

total_class <- length(unique(df$enc_host_spec))
par(mfrow=c(2,3))
dim(df)

for (i in 1:total_class) {
  hist(df$egg_length[df$enc_host_spec==i], xlab = "Egg length", 
       main=paste("Histogram of egg length distribution by host species", df[df$enc_host_spec==i,][1,1]), breaks = 10)
}

cuckoogroup_by_hspecie <- group_by(df,host_species)
summary <- summarise(cuckoogroup_by_hspecie, group_mean=mean(egg_length,na.rm=TRUE),group_sd=sd(egg_length,na.rm=TRUE))
summary

lmres <- lm(egg_length~ host_species,data = df)
anova_res <- anova(lmres)
anova_res

tuk_res <- TukeyHSD(aov(lmres))
summary(tuk_res)
