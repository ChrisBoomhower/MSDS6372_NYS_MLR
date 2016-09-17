#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##############################

## Load required packages
require(dplyr)
require(chron)

## Import raw data from disk (Had to split up original data set into multiple files for pushing to GitHub)
setwd("Analysis/Data/")
NYS <- NULL
for(i in 1:5){
    NYS.temp <- read.table(paste("NYS", i, ".csv", sep = ""), sep = ",", header = TRUE )#stringsAsFactors = FALSE)
    NYS <- rbind(NYS,NYS.temp)
}

str(NYS)

## Rename long variable names
NYSclean <- rename(NYS, Pay.Type. = Payment.Type..Cash.or.E.ZPass.)
NYSclean <- rename(NYSclean, Time = Interval.Beginning.Time)
NYSclean <- rename(NYSclean, Vehicle.Class. = Vehicle.Class)

## Extract day of week and clean up time column
NYSclean$Date <- as.Date(NYSclean$Date, "%m/%d/%Y")
NYSclean$Day <- format(NYSclean$Date, "%a")

# NYSclean$Time <- as.character(NYSclean$Time)
# NYSclean$Time <- paste(substr(NYSclean$Time,1,nchar(NYSclean$Time)-2),
#                             ":",substr(NYSclean$Time,nchar(NYSclean$Time)-1,nchar(NYSclean$Time)), sep = "")

#NYSclean$Time <- chron(times = NYSclean$Time, format = "h:m")

##############################################
## Create dummy variables from factor data
## (doesn't appear dummy variables are required
##in the same way as they are in SAS due to
## factor class... Will likely drop these)
##############################################
NYSclean$Payment.EZPass <- as.numeric(NYSclean$Pay.Type. == "E-ZPass") # Payment dummy variable
NYSclean$Weekday <- ifelse(NYSclean$Day == "Sat" | NYSclean$Day == "Sun", 0, 1) # Day type dummy variable

##############################################
## Define explanatory factors by levels
##############################################
## Define weekdays vs. weekends
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
NYSclean$Day.Type. <- factor((weekdays(NYSclean$Date) %in% weekdays1), 
                        levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

## Define peak vs. nonpeak times (Per NYS Tollway classification)
NYSclean$Period. <- cut(NYSclean$Time, breaks = c(0, 600, 1000, 1500, 1900, 2359), include.lowest = TRUE,
                       labels = c("nonpeak1","peak1","nonpeak2","peak2","nonpeak3"))
levels(NYSclean$Period.)
levels(NYSclean$Period.) <- c("nonpeak", "peak", "nonpeak", "peak", "nonpeak")
levels(NYSclean$Period.)

str(NYSclean)
head(NYSclean, 40)

##############################################
## Randomly sample NYSclean
##############################################
set.seed(10) #Seed set for reproducibility
NYSample <- NYSclean[sample(nrow(NYSclean), 1000),]

##############################################
## Perform EDA
##############################################
## Generate initial model to assess fit
fit <- lm(Vehicle.Count ~ Day.Type. + Period. + Vehicle.Class. + Pay.Type., data = NYSample)
summary(fit)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)