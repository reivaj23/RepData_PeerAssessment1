## Load libraries
library(dplyr)
library(ggplot2)


## Download zip datasets
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = "ActivityMonitotingData.zip")
downloadDate <- date()
unzip(zipfile = "ActivityMonitotingData.zip")


## Read .csv file
activityData <- read.csv("./activity.csv", header = TRUE)


##-------------------------------------------------------------------------------------------------------------
## What is mean total number of steps taken per day?
## Q1: Calculate the total number of steps taken per day
stepsPerDay <- activityData %>% group_by(date) %>% summarise(totalSteps = sum(steps))

## Q2: Make a histogram of the total number of steps taken each day
#hist(stepsPerDay$totalSteps, breaks=10)
stepsBreaks <- seq(0, 25000, 2500)
ggplot(stepsPerDay, aes(x=totalSteps)) +
        # Add title and labels
        ggtitle("Total Number of Steps Taken per Day") + xlab("Steps Per Day") + ylab("Frequency") +
        # Add histogram
        geom_histogram(breaks = stepsBreaks, color="darkblue", fill="lightblue") +
        #coord_cartesian(xlim=c(0,1000), ylim=c(0,max(iprStats$Freq)*1.1)) +
        stat_bin(breaks = stepsBreaks, geom="text", aes(label=..count..), vjust=-1.5)

## Q3: Calculate and report the mean and median of the total number of steps taken per day
mean(stepsPerDay$totalSteps, na.rm = TRUE)
median(stepsPerDay$totalSteps, na.rm = TRUE)
stepsPerDay %>% summarise(mean=mean(totalSteps, na.rm = TRUE), median=median(totalSteps, na.rm = TRUE)) %>%
        knitr::kable(format = "markdown", digits = 2, row.names = F, align = 'c',
                     col.names = c("Mean", "Median"))


##-------------------------------------------------------------------------------------------------------------
## What is the average daily activity pattern?
## Q1: Make a time series plot of the 5-minute interval and average number of step taken,
## averaged across all days
StepsTimeSeriesData <- activityData %>% group_by(interval) %>% summarise(total=sum(steps, na.rm=TRUE), average=mean(steps, na.rm = TRUE))
#plot(StepsTimeSeriesData$average, type="l")
ggplot(StepsTimeSeriesData, aes(x=interval, y=average)) + geom_line() +
        ggtitle("Average Number of Steps Taken in 5-Minute Intervals") +
        xlab("Interval") + ylab("Steps Taken in Average")

## Q2: Which 5-minute interval, on average, contains the maximum number of steps?
StepsTimeSeriesData %>% filter(average==max(average)) %>%
        knitr::kable(format = "markdown", digits = 2, row.names = F, align = 'c', col.names = c("Interval", "Mean", "Median"))


##-------------------------------------------------------------------------------------------------------------
## Imputing Missing Values
## Q1: Calculate and report the total number of missing values in the dataset, i.e. number of rows with NAs
colSums(is.na(activityData))

## Q2: Devise a strategy for filling in all of the missing values in the
## Find days with missing data (NAs)
naDays <- activityData %>% group_by(date) %>% summarise(total=sum(steps)) %>% filter(is.na(total)==TRUE) %>% select(date)

test <- activityData %>% group_by(interval) %>% filter(date==naDays[1])

for (i in 1:100) {
        if (is.na(activityData$steps[i])==TRUE) {
                print(activityData[i,])
        }
}
