---
title: "Reproducible Research: Peer Graded Assessment Week 2"
author: "Rosliza Hamzah"
date: "4 June 2017"
output: html_document
---


This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. 
The data consists of two months of data from an anonymous individual collected 
during the months of October and November, 2012 and 
include the number of steps taken in 5 minute intervals each day.

The data is a comma delimited file, it includes 17,568 observations of 3 variables:
steps: Number of steps taken in a 5 min interval
date: The date when the measurement was taken in YYY-MM-DD format
interval: Identifier for the 5-min interval in which the measurement was taken


##Loading and preprocessing the data

```r
#set working directory
setwd("~/GitHub/m5wk2/RepData_PeerAssessment1-master")

#loading data
activityData <- read.csv ("activity.csv", header = T, sep = ",", stringsAsFactors = F)

#convert the date column
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#Let's check the dimensions and a few rows of our newly created data frame
dim(activityData)
```

```
## [1] 17568     3
```

```r
head(activityData)
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
#Analysis

#A. What is the mean total number of steps taken per day?

```r
library (dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
AvgDay <- activityData %>% group_by(date) %>%
      summarize(total.steps = sum(steps, na.rm = T), 
      mean.steps = mean(steps, na.rm = T))

#the calculate mean and the median
summary(AvgDay$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## Plots

```r
#plotting the histogram of the total steps:
library(ggplot2)
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     diamonds
```

```r
g <- ggplot(AvgDay, aes(x=total.steps))
g + geom_histogram(binwidth = 2500, colour="black", fill="pink") + 
      theme(axis.text = element_text(size = 12),  
      axis.title = element_text(size = 14)) + 
      labs(y = "Frequency") + labs(x = "Total steps/day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


##Conclusion
The histogram shows the largest count around the 10000-12500 step class thus we can infer that the median will be in this interval


## B. What is the daily activity pattern?

In this section we will average the number of steps across each 5 min interval, this will give us an idea of the periods where the person might be the most and 
the least active (aka, a screen shot of a "typical/average" day).


```r
#Group the data by interval and then calculate the mean of each interval group:
AvgInterval <- activityData %>% 
      group_by(interval) %>%
      summarize(mean.steps = mean(steps, na.rm = T))

#The maximum number of average steps
max_steps <- max(AvgInterval$mean.steps)
max_steps
```

```
## [1] 206.1698
```

```r
#The 5-minute interval that contains the maximum number of steps
interval_max_steps<-AvgInterval[which.max(AvgInterval$mean.steps),]$interval
interval_max_steps
```

```
## [1] 835
```
## Plots

```r
#plotting the graph of the Mean Number of Steps:
g <- ggplot(AvgInterval, aes(x = interval, y = mean.steps))
g + geom_line(color="blue") +  
      geom_vline(xintercept = interval_max_steps,color = "red", linetype = "dashed", size = 1) +
      geom_hline(yintercept = max_steps, color = "red", linetype = "dashed",size = 1) +
      theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14, face = "bold")) + 
                  labs(y = "Mean number of steps") + labs(x = "Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
##Conclusion
We can observe the largest amount of steps occurs between time intervals 500 and 1000. The maximum average number of steps is: 206 and occurs in time interval #835

## C. Imputing missing values


```r
#calculate the percentage of missing data as well as the number of rows that contain an NA.
mean(is.na(activityData$steps))
```

```
## [1] 0.1311475
```

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
#calculate the missing values in the the mean number of steps for each 5 min interval:
sum(is.na(AvgInterval$mean.steps))
```

```
## [1] 0
```

```r
#creating a duplicate of the original data named newData to store a appropriate values AvgInterval:
newData <- activityData

for (i in 1:nrow(newData)) {
      if (is.na(newData$steps[i])) {
            index <- newData$interval[i]
            value <- subset(AvgInterval, interval==index)
            newData$steps[i] <- value$mean.steps
      }
}

#showing the data
head(newData)
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

```r
#to group the data by date and calculate daily totals:
newAvg <- newData %>% group_by(date) %>%
summarize(total.steps = sum(steps, na.rm = T))
```
## Plots

```r
#Plotting the histogram
g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500, colour="green", fill="yellow") + theme(axis.text = element_text(size = 12),
                                                                           axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
#reviewing the 5 number summaries and standard deviations of the original data AvgDay vs the data with the imputed values newData
summary (AvgDay$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

```r
sd(AvgDay$total.steps, na.rm=T)
```

```
## [1] 5405.895
```

```r
summary (newAvg$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
sd(newAvg$total.steps, na.rm=T)
```

```
## [1] 3974.391
```

##Conclusion
The mean and the median stay the same, however the 1st quantile of the new data slides closer to the mean. When we look at the standard deviation values, we can also observe that the new data has a smaller standard deviation, thus the effect of imputing NAs with the mean values for the time intervals is a decrease in the spread, we obtained a distribution that is more concentrated around the center of gravity

## 4. Are there differences in activity patterns between weekdays and weekends?


```r
#creating a new column in newData containing the values weekend or weekday:
newData$day <- ifelse(weekdays(newData$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

#creating two subsets weekend and weekday data:
wkend <- filter(newData, day == "weekend")
wkday <- filter(newData, day == "weekday")

#creating a group by the intervals and calculate the mean number of steps for each time interval
wkend <- wkend %>%
      group_by(interval) %>%
      summarize(mean.steps = mean(steps)) 
wkend$day <- "weekend"

wkday <- wkday %>%
      group_by(interval) %>%
      summarize(mean.steps = mean(steps)) 
wkday$day <- "weekday"

#merge both data sets into newInterval
newInterval <- rbind(wkend, wkday)
newInterval$day <- as.factor(newInterval$day)
newInterval$day <- relevel(newInterval$day, "weekend")
```
##Plots


```r
#plotting a graph for weekday and weekend
g <- ggplot (newInterval, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), 
                                             axis.title = element_text(size = 14)) + labs(y = "Number of Steps") + labs(x = "Interval")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

##Conclusion
There is a differences in activity patterns between weekdays and weekends.
