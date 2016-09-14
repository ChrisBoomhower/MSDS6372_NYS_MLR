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
    NYS.temp <- read.table(paste("NYS", i, ".csv", sep = ""), sep = ",", header = TRUE, stringsAsFactors = FALSE)
    NYS <- rbind(NYS,NYS.temp)
}

str(NYS)

## Extract day of week and clean up time column
NYSclean <- NYS

NYSclean$Date <- as.Date(NYSclean$Date, "%m/%d/%Y")
NYSclean$Day <- format(NYSclean$Date, "%a")

NYSclean$Interval.Beginning.Time <- as.character(NYSclean$Interval.Beginning.Time)
NYSclean$Interval.Beginning.Time <- paste(substr(NYSclean$Interval.Beginning.Time,1,nchar(NYSclean$Interval.Beginning.Time)-2),
                                          ":",substr(NYSclean$Interval.Beginning.Time,nchar(NYSclean$Interval.Beginning.Time)-1,nchar(NYSclean$Interval.Beginning.Time)),
                                          sep = "")
#NYSclean$Interval.Beginning.Time <- chron(times = NYSclean$Interval.Beginning.Time, format = "h:m")

str(NYSclean)
