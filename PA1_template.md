---
title: "PA1_template.Rmd"
output: html_document
---
# Reproducible Research: Peer Assessment 1

You need to install first the knitr package 


```r
install.packages('knitr')
```

```
## Installing package into 'C:/Users/ejer1/Documents/R/win-library/3.1'
## (as 'lib' is unspecified)
```

```
## Warning: package 'knitr' is in use and will not be installed
```

```r
library(knitr)
```

## Loading and preprocessing the data

"Load the data"


```r
unzip(zipfile = "repdata_data_activity.zip")
data <- read.csv("activity.csv")
dim(data)
```

```
## [1] 17568     3
```

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(data)
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

"Process/transform the data (if necessary) into a format suitable for your analysis"


```r
data$date <- as.Date(data$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?

"For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day"


```r
total <- aggregate(steps ~ date, data, sum)
head(total)
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
dim(total)
```

```
## [1] 53  2
```

```r
summary(total)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

```r
hist(total$steps, main="Histogram of total steps per day", xlab= "Total steps per day", breaks = 10)
```

![plot of chunk hist1](figure/hist1-1.png) 

"Calculate and report the mean and median total number of steps taken per day"


```r
meandata<-mean(total$steps, na.rm = TRUE)
mediandata<-median(total$steps, na.rm = TRUE)
```

The mean value is 1.0766189 &times; 10<sup>4</sup> and the median value is 10765.

## What is the average daily activity pattern?

"Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)"


```r
total1 <- aggregate(steps ~ interval, data, mean)
head(total1)
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
plot(total1$steps ~ total1$interval, type = "l", xlab = "Time Intervals (step of 5-minutes)",  ylab = "Number of steps taken (all Days)", main = "Average Number of Steps, averaged across all Days - 5 min Intervals")
```

![plot of chunk plot](figure/plot-1.png) 

"Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?"


```r
max <- max(total1$steps)
index<-match(max,total1$steps)
interval<-total1$interval[index]
```

The  104th interval of 5 minutes interval (corresponding to 835 minutes) contains the maximum number of steps on average across all the days: 206.1698113.

## Imputing missing values

"Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)"


```r
Nbna<-length(which(is.na(data$steps)))
```

The total number of missing values in the dataset is 2304.

"Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in."

For the missing value, the mean (average) value for the 5-minute interval is provided.


```r
data1<-data
naindices <- which(is.na(data$steps))
for (i in naindices)
{
    index<-match(data$interval[i],total1$interval)
    data1$steps[i]<-total1$steps[index]
}
dim(data1)
```

```
## [1] 17568     3
```

```r
summary(data1)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
head(data1)
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

"Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day."


```r
total2 <- aggregate(steps ~ date, data1, sum)
head(total2)
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
dim(total2)
```

```
## [1] 61  2
```

```r
summary(total2)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
hist(total2$steps, main="Histogram of total steps per day", xlab= "Total steps per day", breaks = 10)
```

![plot of chunk hist2](figure/hist2-1.png) 


```r
meandatabis<-mean(total2$steps)
mediandatabis<-median(total2$steps)
```

The mean value is 1.0766189 &times; 10<sup>4</sup> and the median value is 1.0766189 &times; 10<sup>4</sup>.

"Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?"

Yes, the median value was changed. 

We are adding days in the new file (8 days - 53 in "total and 61 in "total2"). We can see from the summary of data, that for each of the dates - 288 datas were provided. It can be concluded that all the missing data (2304=288*8) are corresponding to those 8 days and that no other days were affected. For those days, we added the mean / average number for each of the steps. This results for all those days in a value for the total number of steps of 10766.19 (see the first line in the head(total2) which was missing in the file "total""). This value is equal to the first mean value which was calculated. 

It can be noted that the first mean is calculated by summing all the steps taken accross one day and then calculating the average. The sum of all the steps for each day is done, then these results are added and then, the result is divided by 53. 

Similarly, the approach which was taken, consists for each of the missing step to replace the missing value by the average value at the corresponding interval. This value is calculated by summing all the steps corresponding to a given interval and then, to divide by 53 (each interval appears only once per day). Since, if one value is missing all of them are missing for a given day, we are adding the calculated mean values (/53) for each of the intervals and for the whole day. Therefore, this results in summing all the steps values contained in the file and to divide this sum by 53 as for the first mean value. In other words the sum(mean)=mean(sum) in this exercise.
 
It can be checked that the adition of the new dates having numbers of steps equal to the mean value is not going to change the mean value:

meandata=sum/53

meandatabis=(sum+8xmeandata)/61=(sum+8xsum/53)/61

61x53xmeandatabis=53xsum+8xsomme=61xsum

meandatabis=sum/53=meandata

For the median value, if you order total2 by steps, you will see that the new days are in the "middel" of the file (rows 28 to 35), since the median is based on the values of the steps given in row 30, this will give the mean value also, and this value can no longer be an integer as in the first case...


## Are there differences in activity patterns between weekdays and weekends?


"For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day."

I am leaving in Denmark, therefore, I need to change the local information to get the days in English.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
daytype <- weekdays(data1$date)
daytype <- ifelse(daytype == "Saturday" | daytype == "Sunday", "weekend","weekday")
total3<-cbind(data1,daytype)
head(total3)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

"Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data."


```r
total4 <- aggregate(total3$steps, by = list(total3$interval, total3$daytype), 
    mean)
names(total4) <- c("interval", "daytype", "steps")
summary(total4)
```

```
##     interval         daytype        steps        
##  Min.   :   0.0   weekday:288   Min.   :  0.000  
##  1st Qu.: 588.8   weekend:288   1st Qu.:  2.047  
##  Median :1177.5                 Median : 28.133  
##  Mean   :1177.5                 Mean   : 38.988  
##  3rd Qu.:1766.2                 3rd Qu.: 61.263  
##  Max.   :2355.0                 Max.   :230.378
```

```r
library(lattice)
xyplot(steps ~ interval | daytype, total4, type = "l", layout = c(1, 2), 
xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk plot1](figure/plot1-1.png) 
