#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## NYS_EDA.R
##############################

require(MASS)
require(car)

##############################################
## Randomly sample NYSclean
##############################################
set.seed(20) #Seed set for reproducibility
NYSample <- NYSclean[sample(nrow(NYSclean), 10000),]

##############################################
## Perform EDA
##############################################
## Preliminary EDA
par(mfrow = c(1, 1))
barplot(table(NYSclean$Vehicle.Class.))
hist(NYSclean$Vehicle.Count)

NYSample$log.Vehicle.Count <- log(NYSample$Vehicle.Count)
NYSample$sqrt.Vehicle.Count <- sqrt(NYSample$Vehicle.Count)
NYSample$rec.Vehicle.Count <- 1/(NYSample$Vehicle.Count)

par(mfrow = c(2,2))
hist(NYSample$Vehicle.Count)
hist(NYSample$log.Vehicle.Count)
hist(NYSample$sqrt.Vehicle.Count)
hist(NYSample$rec.Vehicle.Count)


## Generate initial model for EDA
fit <- lm(Vehicle.Count ~ Day.Type. + Period. + Vehicle.Class. + Pay.Type. + Day.Type2. + Number.of.Exits, data = NYSample)
par(mfrow = c(1,1))

# QQ Plot
qqPlot(fit, main = "QQ Plot")

# Studentized residuals distribution
sresid <- studres(fit)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit) # Bonferonni p-value for most extreme obs
leveragePlots(fit) # leverage plots

# Check for variable influence on response
avPlots(fit)

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
par(mfrow = c(1,1))
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot
#influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Evaluate homoscedasticity: non-constant error variance test
ncvTest(fit)

# Plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity: component + residual plot 
crPlots(fit)

# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Model summary
summary(fit)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
