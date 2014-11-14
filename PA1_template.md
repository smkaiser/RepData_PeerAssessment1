# Reproducible Research: Peer Assessment 1
Steve Kaiser (https://github.com/smkaiser)  


## Loading and preprocessing the data

```r
# load libraries
library(lubridate)
library(ggplot2)
library(scales)
# Download and unzip the data if necessary
if (!file.exists("./activity.csv")) {
    download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "./activity.zip", method = "curl", mode = "wb")
    unzip(zipfile = "./activity.zip", overwrite = TRUE, setTimes = TRUE)  
}
fileinfo <- file.info("./activity.csv")
activity <- read.csv("./activity.csv", colClasses = c("integer", "Date", "integer"))
```
Data file last modified: 2014-02-11 10:08:20 (local time)

## What is mean total number of steps taken per day?

```r
# ignore missing (NA) values
histdata <- activity[!is.na(activity$steps),]
steptotals = aggregate(histdata$steps, list(histdata$date), FUN = sum)
stepmean = format(mean(steptotals$x), scientific = F)
stepmedian = format(median(steptotals$x), scientific = F)
ggplot(steptotals, aes(x = x)) + geom_histogram(fill="#4080F0", color="black", binwidth=1000) +
    scale_y_continuous(breaks=pretty_breaks()) +
    xlab("Number of steps per day (bin = 1000)") +
    ylab("Frequency") +
    ggtitle("Histogram of number of steps per day")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

Mean steps per day: **10766**

Median steps per day: **10765**

## What is the average daily activity pattern?

```r
# Group the mean total steps by 5-minute intervals 
intervalsteps = aggregate(histdata$steps, list(histdata$interval), FUN=mean)
# Convert interval to a real time value
names(intervalsteps) = c("Interval", "MeanSteps")
intervalsteps$Time <- parse_date_time(sprintf("%04d", intervalsteps$Interval), "hm")
# Find the interval with the highest avg steps
maxinterval = intervalsteps[intervalsteps$MeanSteps == max(intervalsteps$MeanSteps),2:3]
# Plot the time series
plot(intervalsteps$Time, intervalsteps$MeanSteps, xlab = "5 minute interval starting at time", ylab="Mean number of steps during interval", main="Daily activity pattern")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

The 5-minute interval with the highest average steps starts at **08:35 AM**, with **206.1698** steps.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
