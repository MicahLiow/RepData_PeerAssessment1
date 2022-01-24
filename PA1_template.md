---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    
---



## Loading and preprocessing the data

We can start by loading in packages that will help with our analysis, `dplyr`, `tidyr`, `ggplot2` and `Cairo`.  
The `Cairo` package is here to anti-alias the line plots so that they look prettier.


```r
library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)
```

After that, we download the .zip file from the URL, then save it under the filename and location stored in the `zip` variable.


```r
zip <- "./data.zip"
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", zip)
```

Then we unzip the file to extract its contents, and delete it to keep the folder clean. 


```r
unzip(zip)
file.remove(zip)
```

Finally, we read the contents of data.zip (a single CSV file) into a dataframe, `data`. We also convert the `date` column's entries to the `Date` format and the `interval` entries into a 24H time format.


```r
data <- read.csv("./activity.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(interval = formatC(as.numeric(interval), width = 4, flag = "0"))
```

## What is mean total number of steps taken per day?

Let us now construct a histogram showing us how many steps were taken on each day. We can begin by summing up the step counts throughout the day (with some help from `tapply`) and storing it in a dataframe, `DailySteps`. Note that the last three lines of the code below is concerned with making the dataframe look presentable: convert the rownames into a proper column of values, making sure the dates are in the `date` format and not characters, and finally setting the column name for our data column. 


```r
DailySteps <- data %>% 
  with(tapply(steps, date, sum, na.rm = T)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("date") %>%
  mutate(date = as.Date(date))
colnames(DailySteps)[2] <- "steps"
```

Using the `str` function will give us an overview of the dataframe's content:


```r
str(DailySteps)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: int  0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
```

Using `ggplot2` to create a histogram of daily step counts:

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

We shall now compute the mean and median daily step counts.


```r
summarise(DailySteps, mean = mean(steps, na.rm = T), median = median(steps, na.rm = T))
```

```
##      mean median
## 1 9354.23  10395
```

Hence we now know that this individual takes an average of 9354 steps daily (rounding to the nearest whole number), and their median daily step count is 10395 steps. 

## What is the average daily activity pattern?

To do this, we need to take the average of step counts that are of the same 5-minute interval.  
This can be done using a method similar to the previous section, using `tapply` and `mean`.


```r
IntervalSteps <- data %>% 
  with(tapply(steps, interval, mean, na.rm = T)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("interval") %>%
  mutate(interval = formatC(as.numeric(interval), width = 4, flag = "0"))
colnames(IntervalSteps)[2] <- "steps"
```

Let us also run `str` on the new dataframe, `IntervalSteps`:


```r
str(IntervalSteps)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: chr  "0000" "0005" "0010" "0015" ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

Note that `IntervalSteps` contains 288 rows, which makes sense, because there are 288 5-minute intervals in 24 hours.  

Using this data, we can plot a time series of the average number of steps taken throughout the day.

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

From this plot, we can see that the highest step count is slightly more than 200 steps, and is located somewhere at around 0830. We can find the exact step count and interval as such:


```r
Maxsteps <- max(IntervalSteps$steps)
Maxinterval <- IntervalSteps[IntervalSteps$steps == Maxsteps, 1]
print(c(Maxsteps, Maxinterval))
```

```
## [1] "206.169811320755" "0835"
```

Hence we see that on average, the maximum number of steps (206, after rounding to the nearest whole number) is clocked between 8:35 AM and 8:40 AM.

## Imputing missing values

First, we count the number of `NA`s present in `data`.


```r
NumNA <- sum(is.na(data))
print(NumNA)
```

```
## [1] 2304
```

Thus there is a total of 2304 `NA`s. To impute these values, we can replace these `NA`s with the average step count corresponding to that interval. For instance, if the average step count for during the `n`th interval is 100, all `NA`s that appear during this interval are instead replaced with a value of 100.  

This can be done using a `for` loop, where each entry in the `steps` column is replaced with the interval mean (taken from `IntervalSteps`) if it is `NA`, and leaves it alone otherwise.


```r
DataImputed <- data

for(i in 1:nrow(data)){
  step <- DataImputed[i,1]
  int <- DataImputed[i,3]
  DataImputed[i,1] <- ifelse(is.na(step), IntervalSteps[IntervalSteps$interval == int,2], step)
}
```

To make sure that all `NA`s have been removed, we can count the number of `NA`s again:


```r
NewNumNA <- sum(is.na(DataImputed))
print(NewNumNA)
```

```
## [1] 0
```

As such, we have successfully removed all `NA`s from the dataset. Now let us look at a histogram of our daily total steps to see if anything has changed, using the method from our first section.


```r
DailyStepsImputed <- DataImputed %>% 
  with(tapply(steps, date, sum)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("date") %>%
  mutate(date = as.Date(date))
colnames(DailyStepsImputed)[2] <- "steps"
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

And, like in the first section, let us also find the mean and median total steps.


```r
summarise(DailyStepsImputed, mean = mean(steps), median = median(steps))
```

```
##       mean   median
## 1 10766.19 10766.19
```

As you can see, imputing the missing values has increased the mean from 9354 steps to 10766 steps. Also, the mean and median are now the same. This is likely because the missing values skewed the data to have more low values. Imputing these missing values with the mean value reduces this skew, hence giving us a clearer picture of the data's distribution.

## Are there differences in activity patterns between weekdays and weekends?

As the final act, we shall compare the activity patterns on weekdays and weekends.  
We do this by first creating a new factor column in `DataImputed` that tells us if the date is a weekday or weekend.


```r
WeekdaySteps <- DataImputed %>%
  mutate(weekday = weekdays(date)) %>%
  mutate(weekday = ifelse(weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday")) %>%
  mutate(weekday = as.factor(weekday))

str(WeekdaySteps)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: chr  "0000" "0005" "0010" "0015" ...
##  $ weekday : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Then we can find the average daily activity based on whether it is a weekday or weekend:


```r
weekdayDailySteps <- WeekdaySteps %>%
  with(tapply(steps, list(interval, weekday), mean)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("interval") %>%
  mutate(interval = formatC(as.numeric(interval), width = 4, flag = "0")) %>%
  gather("weekday", "steps", -interval)
str(weekdayDailySteps)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ interval: chr  "0000" "0005" "0010" "0015" ...
##  $ weekday : chr  "weekday" "weekday" "weekday" "weekday" ...
##  $ steps   : num  2.251 0.445 0.173 0.198 0.099 ...
```

Thus we have a dataframe that tells us how many steps were taken during each interval on average, based on whether the interval was during a weekday or weekend. We can now plot this data as we did when finding the average daily activity pattern:

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Thus, we see that on weekdays this user takes more steps in the morning, whereas on weekends their step counts tend to be more spread out over the day.
