# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First we need to load the data from activity.csv 


```r
df <- read.csv("activity.csv")
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```
## Next we load the plyr library and use the ddply function to create 


```r
library(plyr)
```

## a summary data frame byDaySummary with the total number of steps per day 
## a summary data frame byIntervalSummary with the mean number of steps per interval across the two month period. 


- Sums of steps per day
- Average steps per interval of time


```r
byDaySummary <- ddply(df, .(date), summarise, stepsPerDay = sum(steps))
```

```
## Error: missing value where TRUE/FALSE needed
```

```r
byIntervalSummary <- ddply(df, .(interval), summarise, averageStepsPerInterval = mean(steps,na.rm=TRUE))
```

```
## Error: missing value where TRUE/FALSE needed
```



## The interval variable measures five minute intervals from 0 to 55 before incremeting an hour.
- To fix this, a vector is created using date and time for each of the 5 minutes intervals.


```r
intervals24hour <- strptime(formatC(df$interval[1:288],width = 4, format = "d", flag = "0"), "%H%M")
```

```
## Error: object of type 'closure' is not subsettable
```

```r
byIntervalSummary <- cbind(intervals24hour, byIntervalSummary)
```

```
## Error: object 'intervals24hour' not found
```

## What is mean total number of steps taken per day?

Checking out the breakdown of the steps per day


```r
hist(byDaySummary$stepsPerDay,
breaks = 30,
col="blue",
main = "Steps taken per day",
xlab = "Number of steps")
```

```
## Error: object 'byDaySummary' not found
```

```r
meanStepsPerDay <- round(mean(byDaySummary$stepsPerDay,na.rm=TRUE))
```

```
## Error: object 'byDaySummary' not found
```

```r
medianStepsPerDay <- round(median(byDaySummary$stepsPerDay,na.rm=TRUE))
```

```
## Error: object 'byDaySummary' not found
```



The mean number of steps per day is 10766
The median number of steps per day is 10765


## What is the average daily activity pattern?

While plotting average steps per interval, it is possible to further review Again to get an overview of the average daily activity pattern, we can plot the number of steps in each interval averaged across the two months


```r
plot(byIntervalSummary$intervals24hour,byIntervalSummary$averageStepsPerInterval,
type = "l",  
xlab = "Interval",  
ylab = "Average steps")
```

```
## Error: object 'byIntervalSummary' not found
```

## Furthermore, the maximum activity interval can be extracted. 


```r
maxActivityInterval <- byIntervalSummary$interval[which.max(byIntervalSummary$averageStepsPerInterval)]
```

```
## Error: object 'byIntervalSummary' not found
```
The maximum average activty is at 8:35am


## Imputing missing values

Firstly, need to review how many values are missing from the dataset


```r
numberOfMissingValues <- sum(is.na(df$steps))
```

```
## Error: object of type 'closure' is not subsettable
```

There are 2304 missing values in the data


## The next piece of codes replaces missing steps values with the average for that interval across all days and places it in a data frame called dfImputed


```r
locationNA <- is.na(df$steps)
```

```
## Error: object of type 'closure' is not subsettable
```
There are 61 days, therefore repeat 61 times

```r
intervalMeansExtension <- as.data.frame(rep(byIntervalSummary$averageStepsPerInterval, times = 61)) 
```

```
## Error: object 'byIntervalSummary' not found
```

```r
names(intervalMeansExtension)  <- c("intervalMeansExtension")
```

```
## Error: object 'intervalMeansExtension' not found
```

## Append on interval Mean Extension data frame

```r
dfImputed <- cbind(df, intervalMeansExtension)
```

```
## Error: object 'intervalMeansExtension' not found
```

```r
dfImputed$steps[locationNA]<- dfImputed$intervalMeansExtension[locationNA]
```

```
## Error: object 'dfImputed' not found
```


## Examine the same frequnecy histogram and examine impact.


```r
byDaySummaryImputed <- ddply(dfImputed, .(date), summarise, stepsPerDay = sum(steps,na.rm=TRUE))
```

```
## Error: object 'dfImputed' not found
```


```r
hist(byDaySummaryImputed$stepsPerDay,
     breaks = 30,
     col="blue",
     main = "Steps taken per day with imputed values for missing values",
     xlab = "Number of steps")
```

```
## Error: object 'byDaySummaryImputed' not found
```

```r
meanStepsPerDayImputed <- round(mean(byDaySummaryImputed$stepsPerDay))
```

```
## Error: object 'byDaySummaryImputed' not found
```

```r
medianStepsPerDayImputed <- round(median(byDaySummaryImputed$stepsPerDay))
```

```
## Error: object 'byDaySummaryImputed' not found
```

The mean number of steps per day is 10766
The median number of steps per day is 10766

The mean and medium are essentially the same as last time.

## Are there differences in activity patterns between weekdays and weekends?

Now we want to compare activity difference between weekdays and weekends. First we need to create a suitable factor variable from the date column.
Assign day of week to each record.


```r
df$DayOfWeek <- as.factor(weekdays(as.Date(df$date)))
```

```
## Error: object of type 'closure' is not subsettable
```
Replace day of week with type of day it is.

```r
levels(df$DayOfWeek) <- c("Weekday","Weekday","Weekend","Weekend","Weekday","Weekday","Weekday")
```

```
## Error: object of type 'closure' is not subsettable
```
Now again we make a by interval summary of steps taken but this time seperating out into the weekends from the weekdays before we make the average.
Then we can generate a plot of interval vs average steps per interval conditioned on weekday vs weekend.


```r
byIntervalSummaryWeekdayWeekend <- ddply(df, .(interval, DayOfWeek), summarise, averageStepsPerInterval = mean(steps, na.rm=TRUE))
```

```
## Error: missing value where TRUE/FALSE needed
```

## Summary of type of day per time interval


```r
library(lattice)

xyplot(byIntervalSummaryWeekdayWeekend$averageStepsPerInterval ~ byIntervalSummaryWeekdayWeekend$interval | byIntervalSummaryWeekdayWeekend$DayOfWeek,
       type = "l", layout = c(1,2),
       xlab = "Interval",
       ylab = "Average steps per interval")
```

```
## Error: object 'byIntervalSummaryWeekdayWeekend' not found
```

USing a xy plot it is possible to see there is significant difference in activity from weekday to weekend.

