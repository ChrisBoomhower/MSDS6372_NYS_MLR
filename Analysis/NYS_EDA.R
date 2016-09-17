#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## NYS_EDA.R
##############################


##############################################
## Randomly sample NYSclean
##############################################
set.seed(20) #Seed set for reproducibility
NYSample <- NYSclean[sample(nrow(NYSclean), 10000),]

##############################################
## Perform EDA
##############################################
## Generate initial model to assess fit
fit <- lm(Vehicle.Count ~ Day.Type. + Period. + Vehicle.Class. + Pay.Type., data = NYSample)
summary(fit)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)