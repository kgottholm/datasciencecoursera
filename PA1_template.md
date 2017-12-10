---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
#### set working directory and read csv

```r
setwd("c:/coursera/datascience/ReproducibleResearch/repdata_data_activity")
data <- read.csv(file="activity.csv",head=TRUE,sep=",")
```

## What is mean total number of steps taken per day?
#### ignore the missing values 

```r
stepsByDay <- aggregate(steps ~ date, data, sum,na.rm=TRUE)
```

#### histogram of the total number of steps taken each day

```r
hist(stepsByDay$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

```r
summary(stepsByDay)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

## What is the average daily activity pattern?
### Make a time series plot of the 5-minute interval and the average number of steps taken

```r
meanByDay = mean(stepsByDay$steps)
medianByDay <- mean(stepsByDay$steps)
stepsByInterval <- aggregate(steps ~ interval, data, mean)
plot(stepsByInterval$interval,stepsByInterval$steps,type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- max(stepsByInterval$steps)
maxInterval <- stepsByInterval[which.max(stepsByInterval$steps),1]
```

## Imputing missing values
#### Note that there are a number of days/intervals where there are missing values (coded as NA). 
#### The presence of missing days may introduce bias into some calculations or summaries of the data.
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
dim(data)
```

```
## [1] 17568     3
```
### How many are missing

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
### fill missing values with the mean/median for that day, or the mean for that 5-minute interval, etc.
### Create a new dataset that is equal to the original dataset 

```r
alldata <- data
nas <- is.na(alldata$steps)
```
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
replaceWith <- tapply(alldata$steps, alldata$interval,mean, na.rm=TRUE, simplify=TRUE)
alldata$steps[nas] <- replaceWith[as.character(alldata$interval[nas])]
```
### Any missing values?

```r
sum(is.na(alldata$steps))
```

```
## [1] 0
```

```r
### Make a histogram of the total number of steps taken each day 
### and Calculate and report the mean and median total number of steps taken per day. 
```

```r
stepsByDay2 <- aggregate(steps ~ date, alldata, sum)
hist(stepsByDay2$steps)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

### What is the impact of imputing missing data on the estimates of the total daily number of steps? None

```r
meanByDay2 = mean(stepsByDay2$steps)
medianByDay2 <- mean(stepsByDay2$steps)
meanByDay2
```

```
## [1] 10766.19
```

```r
medianByDay2
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
### Use the dataset with the filled-in missing values for this part.
### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
### indicating whether a given date is a weekday or weekend day.

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
alldata$wd <- weekdays(as.Date(alldata$date))
alldata$dateType <- as.factor(ifelse(is.element(weekdays(as.Date(alldata$date)),weekdays), "Weekday", "Weekend"))
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
stepsByIntervalMean <- aggregate(steps ~ interval + dateType, alldata, mean)
library(lattice)
par(mfrow=c(2,1))
xyplot(stepsByIntervalMean$steps ~ stepsByIntervalMean$interval|stepsByIntervalMean$dateType, main="Average Steps per Day by Interval", type="l",layout=c(1,2))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)


