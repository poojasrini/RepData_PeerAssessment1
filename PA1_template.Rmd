---
title: "PA1_template.Rmd"
output: html_document
---

```{r,echo=TRUE}
knitr::opts_chunk$set(warning=FALSE)
```

Loading and preprocessing the data  

->Load the data 

```{r }
activityData <- read.csv("./activity.csv")
summary(activityData)
```

1. What is mean total number of steps taken per day?  

->Calculate the total number of steps taken per day

```{r, echo=TRUE}
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
```

-> A histogram of the total number of steps taken each day

```{r , echo=TRUE}
hist(stepsPerDay$steps)
```

-> The mean of the total number of steps taken per day:

```{r,echo=TRUE}
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
```

-> The median of the total number of steps taken per day:

```{r,echo=TRUE}
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

2. What is the average daily activity pattern?  
  
  -> Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r,echo=TRUE}
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
```
  
  -> Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  
```{r,echo=TRUE}
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

3. Imputing missing values

-> Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r,echo=TRUE}
totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissings
```

-> Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r,echo=TRUE}
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
```

-> Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r,echo=TRUE}
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}
```

-> Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r,echo=TRUE}
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```

-> The mean of the total number of steps taken per day:

```{r,echo=TRUE}
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
```

-> The median of the total number of steps taken per day:
 
```{r,echo=TRUE}
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
```

4. Are there differences in activity patterns between weekdays and weekends?

  -> Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
```

-> Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r,echo=TRUE}
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```