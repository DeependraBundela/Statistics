#######################################  Removing Hetroskedesticity & multicolinearity.R  ###########################################
#######################################  Removing Hetroskedesticity & multicolinearity.R  ###########################################

####                            created by : Deependra Singh Bundela
####                            Date : 4/17/2016

#################################################################################################
  

##packages installed in the program
install.packages("ISLR")
install.packages("sandwich")
install.packages("lmtest")
library(ISLR)
attach(College)
library(lmtest)
library(sandwich)

## checking the dataset
View(College) 

## First we will try simple linear regression than we will remove Hetroskedesticity, multicolinearity etc and then do regression
##linear regression 
regression_Linear = lm(formula = Apps~.,data = College) 
summary(regression_Linear)

# plotting the linear regression
plot(regression_Linear)

## Now we will remove the hetroskedesticity 
vcovHC(regression_Linear,omega = NULL,type = "HC4") 
##From this we can see that data has Hetroskedesticity we need to fix this
coeftest(regression_Linear,df = Inf, vcovHC(regression_Linear,omega = NULL,type = "HC4")) 

##summary for regression_Linear
summary(regression_Linear) 

plot(regression_Linear)

### now we will look for influential points using cooks distance
n = nrow(College)  

##yardstick for cooksdistance
limit = 4/n 

##rounding cooks distance
cooks_dist = round(cooks.distance(reg1),4) 
length(cooks_dist[cooks_dist>limit])

## Now we will plot the influential points with limit
plot(regression_Linear,which=4,cook.levels = limit) 
abline(h=limit,col="blue")

##Calculating weights using cooks distance 
weigts1 = ifelse(cooks_dist<=cutoff,1,cutoff/z)  
update(regression_Linear,weights = weigts1) 
summary(regression_Linear) 

##Plotting regression_Linear
plot(regression_Linear)

## now we will see that if there is any multi-colinearity in the data and if we find multi colinearity than we will remove it.
vif(regression_Linear)

## installing package pls
install.packages("pls")
library(pls)

##Applying PCA -multicollinearity reduction
regression2 = pcr(Apps~.,data = College) 

summary(regression2)

##taking number of components as 5
regression2 = pcr(Apps~.,data = College,ncomp=5) 

##Plotting PCA graph
plot(regression2,plottype = "validation") 

##analysing two regression methods 

##Target variable
Target = College$Apps

##Taking predicted values of each regression models
Target1 = regression_Linear$fitted.values
Target2 = regression2$fitted.values

##checking errors between regression models created 
mean(Target1-Target)
mean(Target2-Target)














