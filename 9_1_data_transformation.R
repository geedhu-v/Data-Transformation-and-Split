##################################################
### PROG8430                                    ##
### Data Reduction Demonstration                ##
### Demonstration - SFS                         ##  
##################################################
#                                               ##
##################################################
# Written by Peiyuan Zhou
# ID: 123456789
##################################################
### Basic Set Up                                ##
##################################################
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

##################################################
### Install Libraries                           ##
##################################################
if(!require(rcompanion)){install.packages("rcompanion")}
library("rcompanion")#Q-Q plot
if(!require(psych)){install.packages("psych")}
library("psych")#check how skewed the data

#Create a dataset
data = c(1,3,4,5,6,100,233,1000,1500,2000,10000,45000,9000,12000,20000)
#Plot Data
plotNormalHistogram(data)# data is right skewed

#Q-Q Plot
qqnorm(data) #the result shows it is not normal distributed.
qqline(data,col="blue")

#Check how skewed the data
skew(data)
# closed to 0, data is symmetrical.
# between of (-0.5,-1) moderately left skewed; (0.5,1) moderately right skewed  
# greater than 1, or less than -1, extremely skewed  

#################
#square root transformation
#################
data_sqrt = sqrt(data) # used for moderately skewed data, but this is too high.
plotNormalHistogram(data_sqrt)

#Q-Q Plot
qqnorm(data_sqrt) #the result shows it is not normal distributed.
qqline(data_sqrt,col="blue")
#Check how skewed the data
skew(data_sqrt)

#################
#cube root transformation
#################
#data_cub=sign(data)*abs(data)^(1/3)
data_cub=(data)^(1/3)
plotNormalHistogram(data_cub) # better than squared transformation, but still not normal distributed.

#Q-Q Plot
qqnorm(data_cub) #the result shows it is not normal distributed.
qqline(data_cub,col="blue")
#Check how skewed the data
skew(data_cub)

#################
#log transformation
#################
data_log=log(data) # even stronger, used for reducing the right skewness
plotNormalHistogram(data_log)

#Q-Q Plot
qqnorm(data_log) #the result shows it is not normal distributed.
qqline(data_log,col="blue")
#Check how skewed the data
skew(data_log)



#################
#Tukey's Ladder of Powers transformation
#################
data_tuk = transformTukey(data,plotit=TRUE)
plotNormalHistogram(data_tuk)
#lambda: 0.125
#p-value:0.1559, p-value < 0.05 - reject null hypothesis (normal distribution)

#Q-Q Plot
qqnorm(data_tuk) #the result shows it is not normal distributed.
qqline(data_tuk,col="blue")
#Check how skewed the data
skew(data_tuk)


##################
########Box-Cox transformation
#################
library(MASS)
Box = boxcox(data~1,lambda=seq(-2,2,0.1))
#x: lambda
#y: corresponding profile log-likelihood values associated with each lambda
# The profile log-likelihood is a measure of how well a particular lambda fits the data, 
# higher values indicating better fit

#create a data frame with the results
lambda=Box$x[which(Box$y == max(Box$y))]
lambda
#Transform the originald ata
data_boxcox=(data^lambda-1)/lambda
plotNormalHistogram(data_boxcox)

#Q-Q Plot
qqnorm(data_boxcox) #the result shows it is not normal distributed.
qqline(data_boxcox,col="blue")
#Check how skewed the data
skew(data_boxcox)


#For Linear regression, make sure each feature is normally distributed.