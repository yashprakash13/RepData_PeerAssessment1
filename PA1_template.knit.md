---
title: "PA1_template"
author: "Yash Prakash"
date: "July 23, 2018"
output:
  html_document: null
  keep_md: yes
  pdf_document: default
---

## Loading and preprocessing the data

Loading the data


```r
df <- read.csv("data/activity.csv")
```

##What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
totalSteps <- aggregate(df$steps ~ df$date, df, sum)
head(totalSteps, 5)
```

```
##      df$date df$steps
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
```

2. Make a histogram of the total number of steps taken each day

```r
hist(totalSteps$`df$steps`, xlab = "Steps", col = "tomato") 
```

<img src="PA1_template_files/figure-html/unnamed-chunk-28-1.png" width="672" />

3. Calculate and report the mean and median of the total number of steps taken per day

```r
MeanSteps <- mean(totalSteps$`df$steps`, na.rm = TRUE)
MeanSteps
```

```
## [1] 10766.19
```

```r
MedianSteps <- median(totalSteps$`df$steps`, na.rm = TRUE)
MedianSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(ggplot2)
intervals <- aggregate(df$steps ~ df$interval, df, mean)
ggplot(intervals, aes(x = intervals$`df$interval` , y = intervals$`df$steps`)) + 
        geom_line(color="red", size=1) + 
        labs(title = "Average Daily Steps", x = "Interval", y = "Avg. Steps per Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-30-1.png" width="672" />

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
whichMax <- which.max(intervals$`df$steps`)
maxInterval <- intervals[whichMax, 1]
maxInterval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
nadf <- df[is.na(df$steps), ]
numNa <- nrow(nadf)
numNa
```

```
## [1] 2304
```
Thus, number of missing values are 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
df <- na.omit(df)
```

3. Create a new dataset that is equal to the original dataset

```r
newDf <- df
head(newDf, 10)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
## 295     0 2012-10-02       30
## 296     0 2012-10-02       35
## 297     0 2012-10-02       40
## 298     0 2012-10-02       45
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sumdata <- aggregate(steps~date, newDf, sum)
head(sumdata)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```


```r
hist(sumdata$steps, col = "tomato"
     , xlab = "Steps"
     , main = "Total Number of Steps per Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-36-1.png" width="672" />


```r
mean2 <- mean(sumdata$steps)
mean2
```

```
## [1] 10766.19
```

```r
median2 <- median(sumdata$steps)
median2
```

```
## [1] 10765
```
These are the new mean and medians.

The differences found are:

```r
mean2 - MeanSteps
```

```
## [1] 0
```

```r
median2 - MedianSteps
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(timeDate)

newDf$Weekday <- isWeekday(newDf$date)
head(newDf)
```

```
##     steps       date interval Weekday
## 289     0 2012-10-02        0    TRUE
## 290     0 2012-10-02        5    TRUE
## 291     0 2012-10-02       10    TRUE
## 292     0 2012-10-02       15    TRUE
## 293     0 2012-10-02       20    TRUE
## 294     0 2012-10-02       25    TRUE
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

### For weekdays:

```r
weekday <- subset(newDf, newDf$Weekday == "TRUE")
weekdayMean <- aggregate(steps ~ interval, weekday, mean)
head(weekdayMean)
```

```
##   interval     steps
## 1        0 2.3333333
## 2        5 0.4615385
## 3       10 0.1794872
## 4       15 0.2051282
## 5       20 0.1025641
## 6       25 1.5128205
```

### For weekends:

```r
weekend <- subset(newDf, newDf$Weekday == "FALSE")
weekendMean <- aggregate(steps ~ interval, weekend, mean)
head(weekendMean)
```

```
##   interval    steps
## 1        0 0.000000
## 2        5 0.000000
## 3       10 0.000000
## 4       15 0.000000
## 5       20 0.000000
## 6       25 3.714286
```

### The Panel plot is as follows:

```r
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(weekdayMean$interval, weekdayMean$steps
     , xlab = "Interval", ylab = "Number of Steps"
     , main ="Weekdays", col ="blue", type="l") 

plot(weekendMean$interval, weekendMean$steps
     , xlab = "Interval", ylab = "Number of Steps"
     , main ="Weekends", col ="orange", type="l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-42-1.png" width="672" />
