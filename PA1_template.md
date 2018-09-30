---
title: "PA1_template"
author: "KK"
date: "September 30, 2018"
output: 
  html_document: 
    keep_md: true  
---

----------------------------------------------------------------------------------------
Reproducible Research - WK2 - Project Assignment
================================================

This assignment makes use of data from a personal activity monitoring device. This  device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken

Part 1
=======
Loading and preprocessing the data




What is mean total number of steps taken per day?
  For this part of the assignment, you can ignore the missing values in the dataset.

  Calculate the total number of steps taken per day

  If you do not understand the difference between a histogram and a barplot, research        the difference between them. Make a histogram of the total number of steps taken each      day

  Calculate and report the mean and median of the total number of steps taken per day


```r
    # Calculate the total number of steps taken per day and make a histogram
    steps_per_day <- aggregate(steps ~ date, Clean_act, sum)
    head(steps_per_day)
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
    hist(steps_per_day$steps, main="Total number of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/stepsperday-1.png)<!-- -->

```r
    # Calculate and report the mean and median of the total number of steps taken per day
    summary(steps_per_day)
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


What is the average daily activity pattern?


```r
    ##  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the
    ##  average number of steps taken, averaged across all days (y-axis)

    steps_per_interval <- aggregate(steps ~ interval, Clean_act, mean)
    head(steps_per_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
    plot(steps_per_interval$interval, steps_per_interval$steps, type='l', 
    main="Average number of steps per interval across all days", xlab="Interval", 
    ylab="Average number of steps")
```

![](PA1_template_files/figure-html/stepsperinterval-1.png)<!-- -->

```r
    ## Which 5-minute interval, on average across all the days in the dataset, contains the
    ## maximum number of steps?
    
    max_steps_interval <- which.max(steps_per_interval$steps)
    steps_per_interval[max_steps_interval, ]
```

```
##     interval    steps
## 104      835 206.1698
```

Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as  NA). The presence of missing days may introduce bias into some calculations or summaries   of the data.



```r
    ## Calculate and report the total number of missing values in the dataset 

    sum(is.na(act))
```

```
## [1] 2304
```

```r
    ## Devise a strategy for filling in all of the missing values in the dataset. The
    ## strategy does not need to be sophisticated. For example, you could use the 
    ## mean/median for that day, or the mean for that 5-minute interval, etc.
    
    ## Create a new dataset that is equal to the original dataset but with the missing data     ## filled in. ## The below code uses the "mean for that 5-minute interval"
    
    new_act <- act
    for (i in 1:nrow(act)) {
      if (is.na(act$steps[i])) {
        new_act$steps[i] <- steps_per_interval[steps_per_interval$interval ==     act$interval[i], ]$steps
      }
    }
    head(new_act, 10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```
    
      
    # Calculate the total number of steps taken per day with NEW IMPUTED DATA and make a       # histogram
   

```r
    new_steps_per_day <- aggregate(steps ~ date, new_act, sum)
    head(new_steps_per_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
      hist(new_steps_per_day$steps, main="Total number of steps per day with IMPUTED DATA", xlab = "Steps per day")
    
      
   ## Calculate and report the mean and median total number of steps taken per day
   summary(new_steps_per_day)    
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
   ## Do these values differ from the estimates from the first part of the assignment? What    ## is the impact of imputing missing data on the estimates of the total daily number of    ## steps?
   
   new_act["type_of_day"] <- weekdays(as.Date(new_act$date))
   head(new_act)
```

```
##       steps       date interval type_of_day
## 1 1.7169811 2012-10-01        0      Monday
## 2 0.3396226 2012-10-01        5      Monday
## 3 0.1320755 2012-10-01       10      Monday
## 4 0.1509434 2012-10-01       15      Monday
## 5 0.0754717 2012-10-01       20      Monday
## 6 2.0943396 2012-10-01       25      Monday
```

```r
   new_act$type_of_day[new_act$type_of_day %in% c("Sunday", "Saturday")] <- "Weekend"
   new_act$type_of_day[!new_act$type_of_day %in% c("Weekend")] <- "Weekday"
   head(new_act)
```

```
##       steps       date interval type_of_day
## 1 1.7169811 2012-10-01        0     Weekday
## 2 0.3396226 2012-10-01        5     Weekday
## 3 0.1320755 2012-10-01       10     Weekday
## 4 0.1509434 2012-10-01       15     Weekday
## 5 0.0754717 2012-10-01       20     Weekday
## 6 2.0943396 2012-10-01       25     Weekday
```

```r
   new_act$type_of_day <- as.factor(new_act$type_of_day)
   new_steps_per_interval <- aggregate(steps ~ interval+type_of_day, new_act, sum)
   
   library(ggplot2)
```

![](PA1_template_files/figure-html/Calcluations for IMPUTED DATA-1.png)<!-- -->

```r
   plt <- ggplot(new_steps_per_interval, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = type_of_day)) +
    theme_gray() +
    facet_grid(type_of_day ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("No of steps Per Interval by day type")
    
   print(plt)
```

![](PA1_template_files/figure-html/Calcluations for IMPUTED DATA-2.png)<!-- -->

