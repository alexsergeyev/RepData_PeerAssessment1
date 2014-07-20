# Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
options(scipen = 1, digits = 2)
```

## Loading and preprocessing the data

Original data source available [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

Script automatically load data from compressed files available in repository. Please, set working directory to script directory.


```r
df <- read.csv(unz('./activity.zip', "activity.csv"), colClasses=c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

![plot of chunk steps](figure/steps.png) 



```r
steps_mean   <- mean(stepsd$steps)
steps_median <- median(stepsd$steps)
```

Mean and median number of steps taken per day are 10766.19 and 10765 

## What is the average daily activity pattern?


```r
stepsi <- aggregate(steps ~ interval, df, mean)
plot(stepsi,type='l',xlab="5-minute interval",ylab="steps(avg)")
```

![plot of chunk daily](figure/daily.png) 


```r
max_int <- stepsi[which.max(stepsi$steps),]$interval
```
Interval number 835 contains the maximum number of steps 

## Imputing missing values


```r
na_records <- nrow(df) - sum(complete.cases(df))
```
There are 2304 records with an NA.

Imputing original dataset

```r
df2 <- df
update <- match(df[! complete.cases(df),]$interval, stepsi$interval)
mean_update <- stepsi[update,]$step
```

```
## Warning: Name partially matched in data frame
```

```r
df2[! complete.cases(df2), c("steps")] <- mean_update
```


```r
stepsd2 <- aggregate(steps ~ date, df2, sum)
plot(stepsd2,type="h",ylab="Total Number of Steps",xlab="Date")
```

![plot of chunk steps2](figure/steps2.png) 


```r
steps_mean2   <- mean(stepsd2$steps)
steps_median2 <- median(stepsd2$steps)
```
Mean and median number of steps taken per day are:

* Original: 10766.19 and 10765 
* Imputed:  10766.19 and 10766.19

There is no impact of imputing missing data on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?


```r
weekend <- function(date) {
  ifelse((weekdays(date) %in% c("Saturday", "Sunday")),'weekend', 'weekday')
}
library(lattice)
weekday_info <- aggregate(steps ~ interval + weekend(date), df2, mean)
names(weekday_info)[2] <- "day_type"
xyplot(steps~interval|day_type, weekday_info, type='l', layout=c(1,2))
```

![plot of chunk weekday](figure/weekday.png) 
