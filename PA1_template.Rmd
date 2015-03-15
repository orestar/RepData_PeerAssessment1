---
title: "Reproducible Research - Peer Assesssment 1"
author: "AB"
date: "Tuesday, March 10, 2015"
output: html_document
---

============================================

#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up.
These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks.
But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day.
The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Data

###Dataset:
[Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:  
**steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)  
**date:** The date on which the measurement was taken in YYYY-MM-DD format  
**interval:** Identifier for the 5-minute interval in which measurement was taken  
*The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.*  

#Assignment
This assignment will be described in multiple parts.
You will need to write a report that answers the questions detailed below in a single R markdown document that can be processed by knitr and be transformed into an HTML file.
Throughout your report make sure you always include the code that you used to generate the output you present.
When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code.
For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)
###Step 2: Fork/clone the GitHub repository created for this assignment.


#Part 1 : Loading and preprocessing the data
  
- Loading the data
```{r loaddata,echo=TRUE}
setwd("C:/Users/ore/Desktop/Coursera/5_Reproducible_Research/data")
activity = read.csv("activity.csv")
```

- Process/transform the data into a format suitable for analysis
- Ignore the missing values in the dataset
- Calculate the total number of steps taken per day

```{r,echo=TRUE}
total1 <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

###**Question 1**: What is mean total number of steps taken per day?

- Make a histogram of the total number of steps taken per day

```{r,echo=TRUE}
hist(total1$steps, col = "gray", border = "pink", main = paste("Histogram of total steps per day"),xlab = "Total steps per day")
```

- Calculate and report the mean and median of the total number of steps taken per day

```{r,echo=TRUE}
#Mean of the total number of steps taken per day
mean(total1$steps)
#Median of the total number of steps taken per day
median(total1$steps)
```

###**Question 2** : What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

```{r,echo=TRUE}
#Aggregate steps to get average number of steps in an interval across all days  
interval1 <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
#Generate the line plot of the 5-minute interval  (x-axis)
#and the average number of steps averaged per day (y-axis)
plot(steps ~ interval, data = interval1, type = "l", col = "gray", main ="Average number of steps per day",xlab="Interval", ylab = "Average number of steps")
```

###**Question 3** : Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r,echo=TRUE}
#Get the 5-minute interval that contains the maximum number of steps
interval1[which.max(interval1$steps), ]$interval
```

##Imputing missing values

*Note that there are a number of days/intervals where there are missing values (coded as NA).*  
*The presence of missing days may introduce bias into some calculations or summaries of the data.*
  
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r,echo=TRUE}
#Show the number of rows that contain NA
sum(is.na(activity$steps))
```

- Strategy for filling in all of the missing values in the dataset.
The strategy does not need to be sophisticated.
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**The strategy for filling chosen has been to use the function "interval2" to get the mean steps for that particular 5-minute interval.**

```{r,echo=TRUE}
interval2 <- function(interval) {
    interval1[interval1$interval == interval, ]$steps
}
```


###Create a new dataset that is equal to the original dataset but with the missing data filled in.  

We loop across the rows of the data frame and if the value found is NA we apply the corresponding value of interval and replace it.

```{r,echo=TRUE}
#Imputting missing data
total2 <- activity  # New dataset
count = 0  # Count the number of data filled in
for (i in 1:nrow(total2)) {
    if (is.na(total2[i, ]$steps)) {
        total2[i, ]$steps <- interval2(total2[i, ]$interval)
        count = count + 1
    }
}
cat("There was a total of",count,"NA values that were filled.\n\r")
```

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  

```{r,echo=TRUE}
total3 <- aggregate(steps ~ date, data = total2, sum)
hist(total3$steps, col = "gray", border = "pink", main="Histogram of total steps per day", xlab="Total steps in a day")
#Mean of total number of steps per day
mean(total3$steps)
#Median of total number of steps per day
median(total3$steps)
```

###**Question 5** : Do these values differ from the estimates from the first part of the assignment?

Due to our strategy to fill the NAs, the mean value is the same as previously.
On another hand, the median value shows a small difference.

###**Question 6** : What is the impact of imputing missing data on the estimates of the total daily number of steps?
It can create a small difference.

###**Question 7** : Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here.
Use the dataset with the filled-in missing values for this part.

###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}
#Convert days
total2$day = ifelse(as.POSIXlt(as.Date(total2$date))$wday%%6 == 
    0, "weekend", "weekday")
```

# For Sunday and Saturday : weekend, Other days : weekday

```{r,echo=TRUE}
total2$day = factor(total2$day, levels = c("weekday", "weekend"))
```

###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r,echo=TRUE}
interval2 = aggregate(steps ~ interval + day, total2, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = interval2, aspect = 1/2, 
    type = "l",xlab="Interval", 
      ylab = "Average number of steps")
```
