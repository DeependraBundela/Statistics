#######################################  P_Value Tests  ###########################################
#######################################  P_Value Tests  ###########################################

####                            created by : Deependra Singh Bundela
####                            Date : 2/24/2016

#################################################################################################


admission <- read.csv("E:\\Uconn_Main\\R\\Session 4\\Session 4\\data\\admission.csv")
attach(admission)


sleep_money <- read.csv("E:\\Uconn_Main\\R\\Session 4\\Session 4\\data\\SleepMoney.csv")
attach(sleep_money)

tstat = mean(GMAT)
tstat

f1= function()
{
  x= mean(rnorm(85,mean=510,sd=sd(GMAT)))
  return(x)
}

sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)
abline(v=510+510-tstat,col="red",lwd=3)

x=length(sdist[sdist<tstat|sdist>(510+510-tstat)])
P_value=x/10000
P_value

############################  testing median  #################################################
## calculating popultaion median if you know the sample median
## hypothesis  :-  population median is 500
## 

## sample median
tstat = median(GMAT)
tstat


f1= function()
{
  ## Since population median is assumed as 500 so mean will also be 500 for normal distribution 
  x= median(rnorm(85,mean=500,sd=sd(GMAT)))
  return(x)
}

sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)
abline(v=500+500-tstat,col="red",lwd=3)

x=length(sdist[sdist<tstat|sdist>(500+500-tstat)])
P_value=x/10000
P_value

############################  testing 75th percentile  #################################################
## calculating popultaion median if you know the sample median
## hypothesis  :-  population 75 percentile is 600
##  Popultaion prameters :- What we know about population 
##  Shape : Normal
##  Mean =  546
##  standard deviation =  sample sd

# 75 th percentile

## SO TO FIND mean of the population whose 75 percentile is 600. We have to work from reverse and find a mean of 
## population whose 75th percentile is 600. In this case which comes out to be 546
qnorm(p=.75,546,sd=sd(GMAT))



## sample median
tstat = quantile(GMAT,probs = .75)
tstat


f1= function()
{
  ## Since population median is assumed as 500 so mean will also be 500 for normal distribution 
  x= quantile(rnorm(85,mean=546,sd=sd(GMAT)),probs = .75)
  return(x)
}

sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)
abline(v=546+546-tstat,col="red",lwd=3)

x=length(sdist[sdist<tstat|sdist>(546+546-tstat)])
P_value=x/10000
P_value

###########################   IQR  of  120  ######################################################
## Shape = N
## Mean = Sample mean
## sd = sample sd * 1.1


x1 = qnorm(p=.75,mean(GMAT),sd = sd(GMAT)*1.1)
x2 = qnorm(p=.25,mean(GMAT),sd = sd(GMAT)*1.1)
X1-x2

x1 = quantile(GMAT,probs = .75)
x2 = quantile(GMAT,probs = .25)
tstat = x1-x2

f1= function()
{
  ## Since population median is assumed as 500 so mean will also be 500 for normal distribution 
  x1= quantile(rnorm(85,mean=mean(GMAT),sd=sd(GMAT)*1.1),probs = .75)
  x2= quantile(rnorm(85,mean=mean(GMAT),sd=sd(GMAT)*1.1),probs = .25)
  return(x1-x2)
}

sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)

P_value

###################################  Hypothesis about Test of corelation #################################################

tstat=cor(GMAT,GPA)

f1= function()
{
  ## Since population median is assumed as 500 so mean will also be 500 for normal distribution 
  s1= rnorm(85,mean=mean(GMAT),sd=sd(GMAT))
  s2= rnorm(85,mean=mean(GPA),sd=sd(GPA))
  return(cor(s1,s2))
}

sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)





###################################  Hypothesis about Test of correlation #################################################

tstat=cor(sleep,money)

f1= function()
{
  ## Since population median is assumed as 500 so mean will also be 500 for normal distribution 
  s1= rnorm(85,mean=mean(sleep),sd=sd(sleep))
  s2= rnorm(85,mean=mean(money),sd=sd(money))
  return(cor(s1,s2))
}


sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)

####################################################################################

opar = par()
par(mfrow=c(1,2))
plot(density(GPA))
plot(density(GMAT))
par(opar)


GPA1 = (GPA - mean(GPA))/sd(GPA)
GMAT1 = (GMAT - mean(GMAT))/sd(GMAT)
plot(density(GPA1),col="blue")
lines(density(GMAT1),col="red")

q=c(.1,.3,.5,.7,.9)
x1 = quantile(GPA1,probs = q)
x2 = quantile(GPA1,probs = q)
tstat=sum(abs(x1-x2))
tstat

f1= function()
{ 
  s1=rnorm(85)
  s2=rnorm(85)
  x1=quantile(s1,probs = q)
  x2=quantile(s1,probs = q)
  return(abs(x1-x2))
}


sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)


###################### two sample test  #######################################################

twosample <- read.csv(..)

tgroup = twosample[twosample$group=="Treatment",]
cgroup = twosample[twosample$group=="Control"]

mean(tgroup$score)
mean(cgroup$score)

tstat = abs(mean(tgroup$score)-mean(cgroup$score))
tstat

pop = twosample$score
pop

f1= function()
{
  x=sample(pop)
  mean(x[1:21])
  mean(x[22:44])
  return(abs(mean(op[1:21])-mean(x[22:44])))
}

sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
abline(v=tstat,col="red",lwd=3)

length(sdist[sdist>tstat])/10000


###############################################################################

################    standard approach where we assume normal distribution  ##################
f1 = function()
{
return(mean(rnorm(85,mean = mean(GMAT),sd=sd(GMAT))))
  
}


sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
qunatile(sdist,probs = c(.5/2,1-.5,2))


sample(GMAT, size = 85, replace = T)


### bootsrtapping

################    Bootstrapping Approach  ##################
f1 = function()
{
  return(mean(sample(GMAT,size=85,replace= T)))
}


sdist = replicate(10000,f1())

plot(density((sdist)))
polygon(density(sdist),col = "orange")
qunatile(sdist,probs = c(.5/2,1-.5,2))



