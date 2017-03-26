#######################################  Statistical Estimation.R  ###########################################
#######################################  Statistical Estimation.R  ###########################################

####                            created by : Deependra Singh Bundela
####                            Date : 2/2/2016

#################################################################################################


# Reading the CSV file
Data1 <- read.csv("E:\\UConn_Main\\R\\Session 5\\Session 5\\Data\\data1.csv")                 
# Viewing the data
View(Data1)                                                                   
# Attaching dataset
attach(Data1) 

x=Data1$x1
t.test(x,mu = 5)

###  rnorm()
###  dnorm(x,mean,sd)
###  pnorm(x,mean,sd)
###  qnorm(prob,mean,sd)

dnorm(3.327,mean=4,sd=2, log=T)


## Mean
## M = 4
M = 5


# creating a function which returen the log_likelyhood value for each assumed value for test stats (Mean here)
Log_Likelyhood_Mean = function(M)
{
# finding log_likelyhodd by summing all the probabilities
Log_Likelyhood = sum(dnorm(x,mean=M,sd=2, log=T))
return(Log_Likelyhood)
}

# creating a vector for testing range of mean values so that we can find out for which value log_likehood value is maximum.
Mvalues = seq(0,10,by=0.1)
Mvalues

## applying loh_likehood function for each values
Log_Likelyhood_Result= sapply(Mvalues, Log_Likelyhood_Mean) 

plot(Mvalues,Log_Likelyhood_Result)
which.max(Log_Likelyhood_Result)
Mvalues[]

 
x=rpois(n=30,lambda = 5)
var(rpois(30,lambda = 5))
mean(rpois(30,lambda = 5))

## 
install.packages("bbmle")
library(bbmle)

# first exmaple
x=Data1$x1
x

# structure of population
M=5 #M=m
Sd=2

####################### when both the variables are unknown ####################3

# creating a function which returen the log_likelyhood value for each assumed value for test stats (Mean here)
Log_Likelyhood_Mean = function(M,sigma)
{
  # finding log_likelyhodd by summing all the probabilities
  Log_Likelyhood = sum(dnorm(x,mean=M,sd=sigma, log=T))
  return(-1*Log_Likelyhood)
}

Log_Likelyhood_Result1 =mle2(minuslogl = Log_Likelyhood_Mean, start = list(M=15,Sigma = 2), lower=c(M=0,sigma=0), upper=c(M=20,sigma=20, method="L-BFGS-B"))
summary(Log_Likelyhood_Result1)


# third example posion distribution

# Reading the CSV file
Data2 <- read.csv("E:\\UConn_Main\\R\\Session 5\\Session 5\\Data\\data2.csv")                 
# Viewing the data
View(Data2)                                                                   
# Attaching dataset
attach(Data2) 

#x=Data2$x1
x=Data2$x2


# creating a function which returen the log_likelyhood value for each assumed value for test stats (Mean here)
Log_Likelyhood_Poison = function(l)
{
  # finding log_likelyhodd by summing all the probabilities
  Log_Likelyhood =  sum(dpois(x,lambda =l, log = T))
  return(-1*Log_Likelyhood)
}

Log_Likelyhood_Result_Poison =mle2(minuslogl = Log_Likelyhood_Poison, start = list(l=2))
summary(Log_Likelyhood_Result_Poison)


## if there are lot more zeros in poisson distribution than its mean 
## variance are not approx. equal
mean(Data2$x1)
var(Data2$x1)
mean(Data2$x2)
var(Data2$x2)


## fourth example


LLzp = function(p,l)
{
  ifelse(x==0,p + (1-p)*dpois(0,1),(1-p)*dpois(x,l))
}




