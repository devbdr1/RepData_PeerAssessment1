# Reproducible Research: Peer Assessment 1
##Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

For the purposes of this document, the data file has already been downloaded and put into the current working directory.

## Loading and preprocessing the data
I'm going to using ggplot2 for plotting and I'll use lubridate to quickly convert the date strings in the data to actual dates, for niceness of plots.  I'll also remove NAs from the data beforehand.

```r
library(lubridate)
library(ggplot2)

dat <- read.csv(unz("activity.zip","activity.csv"),header=T)
dat$date <- ymd(as.character(dat$date))   ## convert to actual dates for cleanliness
dat.ona <- na.omit(dat)  ## remove incompletes
```


## What is mean total number of steps taken per day?
#### Make a histogram of the total number of steps taken each day
To create a line representing the mean across total steps per day, tapply is used to create an aggregate sum per date.

```r
dat.ag <- tapply(dat.ona$steps, dat.ona$date, FUN=sum) ## sum by date

gg <- ggplot(data=dat.ona, aes(x=date,y=steps)) ## start the plot
gg <- gg + geom_bar(stat="sum",show.legend=FALSE) ## create a bar plot
gg <- gg + labs(x="Date",y="Total Number of Steps",title="Total Number of Steps\nby Date") ## set labels and title
gg <- gg + geom_hline(yintercept = mean(dat.ag,na.rm=TRUE), color="blue") ## create an overall mean line
gg <- gg + theme(plot.title = element_text(hjust = 0.5)) ## center the title
print(gg) ## print the plot
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Calculate and report the **mean** and **median** total number of steps taken per day
The total steps per day has already been calculated from the above code, so we can just look at the summary to get a mean and median totals steps for the days.

```r
mean(dat.ag)
```

```
## [1] 10766.19
```

```r
median(dat.ag)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

We'll calculate the mean for each interval period using the aggregate function and then make a simple line plot with ggplot2.

```r
dat.avgint <- aggregate(steps~interval, data=dat.ona, FUN=mean)
gg <- ggplot(dat.avgint, aes(x=interval, y=steps))
gg <- gg + geom_line()
gg <- gg + labs(x="Interval", y="Average Number of Steps", title="Average Steps per Interval Period")
gg <- gg + theme(plot.title = element_text(hjust=0.5))
print(gg)
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Taking our aggregated data, just use which.max to identify data.

```r
dat.avgint[which.max(dat.avgint$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```r
sum(is.na(dat))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

I think filling in the missing data with the average mean for the particular interval period makes the most sense.  We already have the average per interval calculated.

```r
dat.intmrg <- merge(dat,dat.avgint, by="interval", suffixes=c("",".intavg"))
dat.intmrg[is.na(dat.intmrg$steps),]$steps <- dat.intmrg[is.na(dat.intmrg$steps),]$steps.intavg
dat.imputed <- dat.intmrg[,1:3]
dat.imputed <- dat.imputed[with(dat.imputed, order(date,interval)),]
```

Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Using the same code from question 1:

```r
dat.agi <- tapply(dat.imputed$steps, dat.imputed$date, FUN=sum) ## sum by date

gg <- ggplot(data=dat.imputed, aes(x=date,y=steps)) ## start the plot
gg <- gg + geom_bar(stat="sum",show.legend=FALSE) ## create a bar plot
gg <- gg + labs(x="Date",y="Total Number of Steps",title="Total Number of Steps\nby Date") ## set labels and title
gg <- gg + geom_hline(yintercept = mean(dat.agi,na.rm=TRUE), color="blue") ## create an overall mean line
gg <- gg + theme(plot.title = element_text(hjust = 0.5)) ## center the title
print(gg) ## print the plot
```

![](PA1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(dat.agi)
```

```
## [1] 10766.19
```

```r
median(dat.agi)
```

```
## [1] 10766.19
```

So there was a minor impact to the data.  The mean stayed the same, which makes sense since we took the mean of each interval to plug in the holes.  The median went up slightly though, which also makes sense since we shifted the data slightly upward with the additional values.

## Are there differences in activity patterns between weekdays and weekends?
For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dat.imputed$daytype <- weekdays(dat.imputed$date)
dat.imputed[dat.imputed$daytype %in% c("Saturday","Sunday"),]$daytype <- "Weekend"
dat.imputed[!(dat.imputed$daytype %in% c("Saturday","Sunday","Weekend")),]$daytype <- "Weekday"
dat.imputed$daytype <- as.factor(dat.imputed$daytype)
```

Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
dat.intavg.daytype <- aggregate(steps ~ interval + daytype, dat.imputed, FUN=mean)
gg <- ggplot(data=dat.intavg.daytype, aes(x=interval, y=steps))
gg <- ggplot(data=dat.intavg.daytype, aes(x=interval, y=steps))
gg <- gg + facet_grid(~dat.intavg.daytype$daytype)
gg <- gg + geom_line()
gg <- gg + labs(x="Interval", y="Average Number of Steps", title="Average Steps per Interval Period")
gg <- gg + theme(plot.title = element_text(hjust=0.5))
print(gg)
```

![](PA1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
