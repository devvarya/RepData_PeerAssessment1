---
title: "Reproducible Research Assignment 1"
author: "Devvrat Arya"
date: "Friday, August 14, 2015"
output: html_document
---

###INTRODUCTION
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/in), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuel), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

###Data
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded here : [Dataset: Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date**: The date on which the measurement was taken in YYYY-MM-DD format

**interval**: Identifier for the 5-minute interval in which measurement was taken

### Setting up the R environment

Set **echo=TRUE** and **results="hold"** as global options for this document


```r
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
```

####Load required libraries


```r
library(ggplot2)
library(lattice)
```

###Loading and Pre-processing the Data


```r
act <- read.csv("activity.csv",header=TRUE,colClasses = c("numeric","character","numeric"))
head(act)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

The **date** field is converted to Date class

```r
act$date <- as.Date(act$date, "%Y-%m-%d")
```

### Mean total number of steps taken per day?

For this part, the missing values in the dataset are ignored

The total number of steps taken per day using the aggregate function:

```r
spd <- aggregate(steps ~ date,sum,data=act,na.rm=TRUE)
## The elements(dates) are coerced to factors before use.
head(spd)

##Another method
##spd <- tapply(act$steps,act$date,sum,na.rm=TRUE)
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

A histogram of the total number of steps taken each day:

```r
hist(spd$steps, col="slateblue1",breaks=5, main="Total Steps Taken Each Day", xlab="Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The mean of the total number of steps taken per day:

```r
mean(spd$steps)
```

```
## [1] 10766.19
```

The median of the total number of steps taken per day:

```r
median(spd$steps)
```

```
## [1] 10765
```


### The average daily activity pattern?

Average number of steps taken in each interval (Averaged across all days):

```r
sbi <- aggregate(steps ~ interval, mean ,data=act,na.rm=TRUE)
head(sbi)
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

A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
ggplot(sbi, aes(interval, steps)) +
geom_line() +
xlab("Interval") +
ylab("Mean Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
##Using base plotting system
##plot(sbi$interval,sbi$steps,type="l",xlab="Interval",ylab="Mean Steps",
##main="Time Series plot of 5-min Interval and Steps")
```

Finding the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps:


```r
max <- sbi[which.max(sbi$steps),]
max
```

```
##     interval    steps
## 104      835 206.1698
```


###Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
na <- sum(is.na(act))
na
```

```
## [1] 2304
```

The strategy used for filling in all of the missing values in the dataset is by replacing any such value with the mean for the corresponding 5-minute interval. 

Hence creating a new dataset that is equal to the original dataset but with the missing data filled in(Both parts included in the following code):


```r
pos <- which(is.na(act$steps))

actnew <- act

for(i in 1:length(sbi$interval))
  {
  key <- sbi[i,1]
  value <- sbi[i,2]
  for(j in 1:length(pos))
  {
    index <- pos[j]
    if(actnew[index,3]==key)
  {
    actnew[index,1] <- value 
  }
  }
  }

head(actnew)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

A histogram of the total number of steps taken each day using the new data set:


```r
spdnew <- aggregate(steps ~ date,sum,data=actnew,na.rm=TRUE)
hist(spdnew$steps, col="skyblue",breaks=5, main="Total Steps Taken Each Day", xlab="Steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

The mean of total number of steps taken per day:


```r
mean(spdnew$steps)
```

```
## [1] 10766.19
```

The median of total number of steps taken per day: 


```r
median(spdnew$steps)
```

```
## [1] 10766.19
```

As we can see, these values differ from the estimates from the first part of the assignment. Imputing the missing data changes the median of the total number of steps taken per day, although the mean remains the same. Also, it goes to show replacing the NA values with the mean values has led to the median value shifting towards the mean value.


### Differences in activity patterns between weekdays and weekends

For this part the dataset with the filled-in missing values is used.

Firstly, a new factor variable in the dataset was created with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:


```r
day <- weekdays(actnew$date, abbreviate = FALSE)

daytype <- vector()

for (i in 1:nrow(actnew)) 
  {
  if (day[i] == "Saturday" | day[i] == "Sunday") 
    {
    daytype[i] <- "Weekend"
    }
  else 
    {
    daytype[i] <- "Weekday"
    }
  }
```

Adding a new factor variable to the dataset:

```r
actnew$daytype <- daytype
actnew$daytype <- factor(actnew$daytype)
```

A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):


```r
sbinew <- aggregate(steps ~ interval + daytype, mean, data=actnew, na.rm=TRUE)
head(sbinew)
```

```
##   interval daytype      steps
## 1        0 Weekday 2.25115304
## 2        5 Weekday 0.44528302
## 3       10 Weekday 0.17316562
## 4       15 Weekday 0.19790356
## 5       20 Weekday 0.09895178
## 6       25 Weekday 1.59035639
```

Using the lattice plotting system:

```r
xyplot(steps ~ interval|daytype, sbinew, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png) 

Using the ggplot2 plotting system: 

```r
g <- ggplot(sbinew, aes(interval, steps)) 
g + geom_line(color="tomato") + 
  facet_wrap(~ daytype, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png) 

The greatest peak is on weekdays. While the activity peaks are more uniformly distributed on weekends.  
