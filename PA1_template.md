# Report for Reproducible Research: Peer Assessment 1
Load the csv data, using read.csv() and store it in dat.
## Loading and preprocessing the data

```r
# loading data from csv
dat<- read.csv("activity.csv")
```
Now Let's have a look of the data.

```r
head(dat, 20)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## 11    NA 2012-10-01       50
## 12    NA 2012-10-01       55
## 13    NA 2012-10-01      100
## 14    NA 2012-10-01      105
## 15    NA 2012-10-01      110
## 16    NA 2012-10-01      115
## 17    NA 2012-10-01      120
## 18    NA 2012-10-01      125
## 19    NA 2012-10-01      130
## 20    NA 2012-10-01      135
```
The variables are:
* steps   : Number of steps taking in a 5-minute interval (missing values are
            coded as NA)
* date    : The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

There is a lot of missing values, we remove the NAs and store it in sub

```r
# remove step == NA
sub<- dat[!is.na(dat[,1]), ]
```

Have a look a sub

```r
head(sub, 10)
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

Q1: What is mean total number of steps taken per day?
As the instruction, we ignore the missing value. Solve this problem, using 
the subset sub.
* 1. Make a histogram of the total number of steps taken each day

```r
#
library(plyr)
steps_bydate <- ddply(sub, .(date), summarise, steps=sum(steps))
hist(steps_bydate$steps, main = paste("Histogram of " ,"the total number of steps taken each day"), xlab="steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

* 2. Calculate and report the mean and median total number of steps taken per     day
  Mean:

```r
meanVal <- mean(steps_bydate$steps)
meanVal
```

```
## [1] 10766.19
```
  Median: 

```r
medianVal <- median(steps_bydate$steps)
medianVal
```

```
## [1] 10765
```

Q2: What is the average daily activity pattern?
* 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
     and the average number of steps taken, averaged across all days (y-axis)

Calculate average steps at each interval, which is similar to Q1.

```r
avg_date <- ddply(sub, .(interval), summarise, "avg_steps"=mean(steps))
```
Then make a time series plot, using type: "l". 

```r
plot(avg_date$interval, avg_date$avg_steps, type="l", xlab="5-min interval", 
     ylab="Average steps",main="Average daily activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

* 2. Which 5-minute interval, on average across all the days in the dataset,
     contains the maximum number of steps?

```r
max_col <- avg_date[avg_date$avg_steps==max(avg_date$avg_steps),]
max_col
```

```
##     interval avg_steps
## 104      835  206.1698
```

We found that the max_interval is 835 with a average steps 206.1698113.

Q3: Imputing missing values
* 1. Calculate and report the total number of missing values in the dataset
     (i.e. the total number of rows with NAs)

```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```
* 2. Devise a strategy for filling in all of the missing values in the dataset.     
     The strategy does not need to be sophisticated. For example, you could use 
     the mean/median for that day, or the mean for that 5-minute interval, etc.
First, we join the original dataset with the averags steps by interval.

```r
filled <- join(dat, avg_date, by="interval")
```
Then we check the each column of the orignial dataset, if it's NA. Then make it 
equal to average steps.

```r
filled$steps[is.na(filled$steps)]<-filled$avg_steps[is.na(filled$steps)]
```
It should work, lets check.

```r
sum(is.na(filled$steps))
```

```
## [1] 0
```
* 3. Create a new dataset that is equal to the original dataset but with the 
     missing data filled in.

```r
new_dat <- filled[,c("steps", "date","interval")]
```
Let's check the is any NA in new_dat

```r
sum(is.na(filled$steps))
```

```
## [1] 0
```

```r
count(is.na(new_dat[,"steps"] == dat[,"steps"]))
```

```
##       x  freq
## 1 FALSE 15264
## 2  TRUE  2304
```
* 4. Make a histogram of the total number of steps taken each day and Calculate 
     and report the mean and median total number of steps taken per day. Do   
     these values differ from the estimates from the first part of the assignme
     -nt? What is the impact of imputing missing data on the estimates of the
     total daily number of steps?

```r
new_steps_bydate <- ddply(new_dat, .(date), summarise, steps=sum(steps))
hist(new_steps_bydate$steps, main = paste("Histogram of " ,"the total number of steps taken each day without missing value"), xlab="steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

```r
new_mean   <-  mean(new_steps_bydate$steps)
new_mean
```

```
## [1] 10766.19
```

```r
new_median <-  median(new_steps_bydate$steps)
new_median
```

```
## [1] 10766.19
```

```r
diff_mean  <-  meanVal - new_mean
diff_mean
```

```
## [1] 0
```

```r
diff_median<-  medianVal - new_median
diff_median
```

```
## [1] -1.188679
```
The new mean is 1.0766189\times 10^{4}, and new median  is 1.0766189\times 10^{4}. After computing missing value, they do not change much.



Q4: Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
weekdays <- weekdays(as.Date(filled$date))
data_weekdays <- transform(filled, day=weekdays)
data_weekdays$wk <- ifelse(data_weekdays$day %in% c("Saturday", "Sunday"),"weekend", "weekday")
average_week <- ddply(data_weekdays, .(interval, wk), summarise, steps=mean(steps))
```


```r
xyplot(steps ~ interval | wk, data = average_week, layout = c(1, 2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png) 
```
