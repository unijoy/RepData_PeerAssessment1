# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the data (i.e. read.csv())  
Process/transform the data (if necessary) into a format suitable for your analysis


```r
mdata <- read.csv("activity.csv",header = TRUE)
mdata[,"date"] <- as.Date(mdata[,"date"], "%Y-%m-%d" )
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

```r
perDayStep <- aggregate(mdata$steps,by=list(mdata$date),FUN = "sum")
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(perDayStep$x,breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
mean(perDayStep$x,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(perDayStep$x,na.rm = TRUE)
```

```
## [1] 10765
```

```r
# abline(v = mean(perDayStep$x,na.rm = TRUE), col = "red", lwd = 2)
# abline(v = median(perDayStep$x,na.rm = TRUE), col = "blue", lwd = 2)
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
time_series <- tapply(mdata$steps,mdata$interval,mean,na.rm=TRUE)
plot(names(time_series),time_series,type="l",col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
names(which.max(time_series))
```

```
## [1] "835"
```
## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(mdata))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
meanOfInterval <- aggregate(mdata$steps,by=list(mdata$interval),mean,na.rm=TRUE)
getMeanByInterval <- function(interval){
  meanOfInterval$x[meanOfInterval$Group.1 == interval]
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
mdata2<-transform(mdata,steps= ifelse( is.na(steps),getMeanByInterval(interval) ,steps ))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
perDayStep2 <- aggregate(mdata2$steps,by=list(mdata2$date),FUN = "sum")
par(mfrow = c(1,2))
hist(perDayStep$x,breaks=15,main = "with missing value")
hist(perDayStep2$x,breaks=15,main = "fill missing value")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(perDayStep2$x,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(perDayStep2$x,na.rm = TRUE)
```

```
## [1] 10765.59
```

* The median is a little different,the histogram are the same

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
mdata2<- transform(mdata2, weekend=as.factor(as.POSIXlt(date, format='%Y-%m-%d')$wday %in% c(0, 6) ))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)

stepByFactor <- aggregate(mdata2$steps ~ mdata2$interval+mdata2$weekend,FUN = "mean")
names(stepByFactor) <- c("interval","weekend","steps")
levels(stepByFactor$weekend) <- c("weekday","weekend")
xyplot(steps ~ interval | weekend, stepByFactor, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
