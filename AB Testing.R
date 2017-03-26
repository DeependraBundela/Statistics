#######################################  Stats in R - 7  ###########################################
#######################################  Stats in R - 7  ##########################################

####                            created by : Deependra Singh Bundela
####                            Date : 2/24/2016

#################################################################################################

###########################  AB Testing   ########################################################
# Reading the CSV file
abtesting <- read.csv("E:\\UConn_Main\\R\\abtesting.csv")                 
# Viewing the data
View(abtesting)                                                                   
# Attaching dataset
attach(abtesting)    

# Making a new column for conversion rate
abtesting$conversion_rate=(Completion)/(Visits)

# Making a subset for homepage A data
Conversion_rate_for_site_A=abtesting[Site=="A",]                                                     

# Making a subset for homepage B data
Conversion_rate_for_site_B=abtesting[Site=="B",]        
 
# Finding the difference in mean in our actual sample
Testing_stats=(mean(Conversion_rate_for_site_A$conversion_rate)-mean(Conversion_rate_for_site_B$conversion_rate))  

#  creating population
population=abtesting$conversion_rate  
f1=function()
{
  x=sample(population)
  mean(x[1:12])
  mean(x[13:24])
  return(abs(mean(x[1:12])-mean(x[13:24])))
}

sampling_distribution=replicate(10000,f1())

# plotting the data
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="magenta")
abline(v = Testing_stats)

# p value
prob=length(sampling_distribution[sampling_distribution>Testing_stats])/10000
prob

# detaching the data
detach(abtesting)

## since the p-vlaue is .4231 which is very significant. 
## So we cannot reject our null-hypothesis that it is pure chance and there is no significant difference between the two scenarios.
## We can use any of these version. It won't make much difference

########################### User story 1 ########################################################

# creating uniform distribution for task1
task1= runif(10000,min = 5,max = 9)

# creating normal distribution for task2
task2= rnorm(10000, mean = 7, sd= 2)

# creating exponential distribution for task3
task3= rexp(10000,rate = 8)

# creating uniform distribution for task4
task4= runif(1000,min=3, max= 10)

#Path one combining task1 and task2
path1= task1+ task2

#Path one combining task3 and task4
path2= task3 +task4 

# distribution for time to completa all task
Time_to_complete= ifelse(path1<path2, path2, path1)

# Mean time to complete all the task
mean(Time_to_complete)

# Median time to complete all the task
median(Time_to_complete)

probability=length(Time_to_complete[Time_to_complete<12])/length(Time_to_complete)

# Probabality that all the task will complete in 12 hours.
probability

# Density plot for total complete time
plot(density(Time_to_complete))

###########################  User Story 2  ########################################################

# Distribution for returns on stock A,B,C,D
Return_on_A=rnorm(1000,mean = 0.1,sd=0.05)
Return_on_B=runif(1000,min = -.1, max = .15)
Return_on_C=runif(1000,min= -.15,max=.25)
Return_on_D=rnorm(1000,mean=0.20, sd=0.3)


# Profits for stock A,B,C,D
Profit_on_A= 95*(1+Return_on_A)*30 - 95*30
Profit_on_B= 100*(1+Return_on_B)*15 - 100*15
Profit_on_C= 25*(1+Return_on_C)*25 - 25*25
Profit_on_D= 50*(1+Return_on_D)*50 - 50*50

# Distribution for total extpected profit(vector)
Total_expected_profit=Profit_on_A+Profit_on_B+Profit_on_C+Profit_on_D


# Total extpected profit on investment
Total_expected_profit_mean = mean(Total_expected_profit)
Total_expected_profit_mean

# Median and standard deviation for total expected profit. 
Total_expected_profit_median=median(Total_expected_profit)
Total_expected_profit_median
Total_expected_profit_sd=sd(Total_expected_profit)
Total_expected_profit_sd
# Probability of not losing money
Prob1=length(Total_expected_profit[Total_expected_profit>=100])/length(Total_expected_profit)
Prob1

# Probablity for making atleast $1500 in profit.
Prob2=length(Total_expected_profit[Total_expected_profit>=1600])/length(Total_expected_profit)
Prob2


###########################  User Story 4 ########################################################

# Read csv file
Health_data <- read.csv("E:\\Uconn_Main\\R\\Health.csv") 
# Attaching data
attach(Health_data)
# calculating mean of ofp
Testing_stats=mean(ofp)

# function for population
f1=function()
{
  x=mean(rpois(4406,lambda = 5.5)) 
  return(x)
}

# creating sample distribution
sampling_distribution=replicate(1000,f1())

#Ploting the sample
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="pink")
#Ploting the actual sample line 
abline(v = Testing_stats)

# the P value 
length(sampling_distribution[sampling_distribution>Testing_stats])

#Detaching the data
detach(Health_data)

## So here we can reject the hpothesis that mean of office visit is 5.5

###########################  User story 5 ########################################################

# Reading the csv file
Walmart_Data_file<- read.csv("E:\\Uconn_Main\\R\\walmart.csv")  
# Viewing the file
View(Walmart_Data_file)
#Attaching the data
attach(Walmart_Data_file)

# Subsetting the data for non holidays for store 1 
str1=Walmart_Data_file[Store==1 & IsHoliday=="FALSE",]                                    

# Mean of subset
str1mean=mean(str1$Weekly_Sales)                                             

# median of subset
str1median=median(str1$Weekly_Sales)                                          


# standard deviation of subset
str1sd=sd(str1$Weekly_Sales)          
str1 
plot(density(str1))


f1=function()
{
  # Creating a sample normal distribution with mean and sd same as str1
  return(mean(rnorm(nrow(str1),mean = str1mean, sd= str1sd)))            
  
}

sampling_distribution= replicate(10000,f1())
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="blue")
quantile(sampling_distribution,p=c(0.05/2,1-0.05/2))                                              

# calculating confidence interval

# Here is a functionn which takes the median of normal distrbution as quantity of interest
f1=function()
{
  
  return(median(rnorm(nrow(str1),mean = str1mean, sd= str1sd)))
  
}

sampling_distribution= replicate(1000,f1())
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="blue")
quantile(sampling_distribution,p=c(0.05/2,1-0.05/2))


# Here is a function which takes the standard deviation of normal distrbution as quantity of interest
f1=function()
{
  
  return(sd(rnorm(nrow(str1),mean = str1mean, sd= str1sd)))
  
}

sampling_distribution= replicate(1000,f1())
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="blue")
quantile(sampling_distribution,p=c(0.05/2,1-0.05/2))


# Here is a function for bootstarpping with (mean - replacement)
f1 = function()
{
  Sample_return=sample(str1$Weekly_Sales, size = nrow(str1), replace = TRUE)
  return(mean(Sample_return))
}

sampling_distribution = replicate (10000,f1())
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="blue")
quantile(sampling_distribution,p = c(0.05/2,1-0.05/2))


# Here is a function for bootstarpping (median with replacementt) 
f1 = function()
{
  Sample_return=sample(str1$Weekly_Sales, size = nrow(str1), replace = TRUE)
  return(median(Sample_return))
}

sampling_distribution = replicate (10000,f1())
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="blue")
quantile(sampling_distribution,p = c(0.05/2,1-0.05/2))

# Here is a function for bootstarpping  standard deviation with replacement
f1 = function()
{
  Sample_return=sample(str1$Weekly_Sales, size = nrow(str1), replace = TRUE)
  return(sd(Sample_return))
}

sampling_distribution = replicate (10000,f1())
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="blue")
quantile(sampling_distribution,p = c(0.05/2,1-0.05/2))


###########################  User story 6 ########################################################

# Creating vector for greater than 60 years old people
Greater_vec<-c(68,74,77,71,73,75,80,77,78,72,69,71) 
#Creating vector for greater than 60 years old people
Lower_vec=c(75,77,80,69,73,76,78,74,75,81,75,80)                                           

# Test statisctics
Testing_stats= (mean(Greater_vec)-mean(Lower_vec))

# Creating population
population=c(Greater_vec,Lower_vec)
f1=function()
{
  # shuffling the data
   x=sample(population)                                                                   
   # Assigning sampled values
  return((mean(x[1:12])-mean(x[13:24])))                                             
  
}

sampling_distribution=replicate(10000,f1())                                                     
plot(density(sampling_distribution))
polygon(density(sampling_distribution),col="Blue")
abline(v = Testing_stats)                                                                    
# Plotting the test statistic on 1 side of the density plot
abline(v = mean(sampling_distribution)+mean(sampling_distribution)-Testing_stats)                                            # Plotting the test statistic on 2nd side of the density plot

prob=length(sampling_distribution[sampling_distribution<Testing_stats|sampling_distribution>(mean(sampling_distribution)+mean(sampling_distribution)-Testing_stats)])/10000          # Finding the P values by taking area on both sides of test statistic on the density plot
prob


## since the p-vlaue is .1235 which is very significant. 
## So we cannot reject our null-hypothesis that it is by pure chance and there is no significant difference between the two scenarios.
## So the complaint is not justified and this happened pure by chance.



###########################  Ends here ########################################################
