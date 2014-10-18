---
title: "PeerAssessment1"
output: html_document
---

##Loading and preprocessing the data
To begin with, we need to load the needed packages:


```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

Then we have to read the data from the csv file:


```r
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
activity$date <- as.Date(activity$date)
```

```
## Error in as.Date(activity$date): object 'activity' not found
```

##What is mean total number of steps taken per day?

First of all we want to aggregate the data by date:


```r
totalSteps <- aggregate(steps ~ date, data=activity, FUN=sum, na.rm=TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

The histogram with the total number of steps per day is:


```r
ggplot(totalSteps, aes(x=date, y=steps)) + stat_summary(fun.y="sum", geom="bar") + xlab("Date") + ylab("Total number of steps") 
```

```
## Error in ggplot(totalSteps, aes(x = date, y = steps)): object 'totalSteps' not found
```


The mean of the total number of steps taken per day is:


```r
mean(totalSteps$steps)
```

```
## Error in mean(totalSteps$steps): object 'totalSteps' not found
```


The median of the total number of steps taken per day is:


```r
median(totalSteps$steps)
```

```
## Error in median(totalSteps$steps): object 'totalSteps' not found
```


##What is the average daily activity pattern?
We aggregate the data by the mean of every individual interval:


```r
intervalSteps <- aggregate(steps ~ interval, data=activity, FUN=mean, na.rm=TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

We make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:


```r
ggplot(intervalSteps, aes(interval, steps)) + geom_line() + xlab("5 Minute Interval") + ylab("Average Number of Steps")
```

```
## Error in ggplot(intervalSteps, aes(interval, steps)): object 'intervalSteps' not found
```


The 5-minute interval that, on average across all the days in the dataset, contains the maximum number of steps is:


```r
intervalSteps[which.max(intervalSteps$steps),]
```

```
## Error in eval(expr, envir, enclos): object 'intervalSteps' not found
```

##Imputing missing values

We calculate the total number of missing values in the dataset:


```r
sum(!complete.cases(activity))
```

```
## Error in complete.cases(activity): object 'activity' not found
```

To fill the missing value from the original dataset, we decide to use the interval mean previously calculated.
First, we merge the original dataset with the mean dataset:


```r
filledData <- merge(activity, intervalSteps, by='interval')
```

```
## Error in merge(activity, intervalSteps, by = "interval"): object 'activity' not found
```

```r
colnames(filledData) <- c("interval", "steps", "date", "mean")
```

```
## Error in colnames(filledData) <- c("interval", "steps", "date", "mean"): object 'filledData' not found
```

Then we replace the missing values with the interval mean (rounded to the nearest integer):

```r
filledData$mean <- as.integer(filledData$mean)
```

```
## Error in eval(expr, envir, enclos): object 'filledData' not found
```

```r
filledData$steps <- ifelse(is.na(filledData$steps), filledData$mean, filledData$steps)
```

```
## Error in ifelse(is.na(filledData$steps), filledData$mean, filledData$steps): object 'filledData' not found
```

```r
filledData$mean <- NULL
```

```
## Error in filledData$mean <- NULL: object 'filledData' not found
```

To make a histogram of the dataset without missing values, we have to aggregate the data by day:


```r
filledDataByDay <- aggregate(steps ~ date, data=filledData, FUN=sum)
```

```
## Error in eval(expr, envir, enclos): object 'filledData' not found
```

This is the histogram with the new dataset:


```r
ggplot(filledDataByDay, aes(x=date, y=steps)) + stat_summary(fun.y="sum", geom="bar") + xlab("Date") + ylab("Total number of steps")
```

```
## Error in ggplot(filledDataByDay, aes(x = date, y = steps)): object 'filledDataByDay' not found
```

As we've done previously, the mean of the new dataset is:


```r
mean(filledDataByDay$steps)
```

```
## Error in mean(filledDataByDay$steps): object 'filledDataByDay' not found
```

And the median is:

```r
median(filledDataByDay$steps)
```

```
## Error in median(filledDataByDay$steps): object 'filledDataByDay' not found
```

The mean and the median are not very different from what we calculated before.

##Are there differences in activity patterns between weekdays and weekends?

First we create a factor variable to describe the type of day:

```r
dayType <- function(date) {
            if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
                "weekend"
            } else {
                "weekday"
            }
        }
```

Then we apply this function to the filled dataset:

```r
filledData$dayType <- as.factor(sapply(filledData$date, dayType))
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'filledData' not found
```

Finally we make a plot to compare the difference in the average number of steps between weekdays and weekends:


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    daytype <- aggregate(steps ~ interval, data = filledData, subset = filledData$dayType == type, FUN = mean)
    plot(daytype, type = "l", main = type)
}
```

```
## Error in eval(expr, envir, enclos): object 'filledData' not found
```
