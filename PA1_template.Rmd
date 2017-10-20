---
title: "PA1_template"
author: "William Matthews"
date: "October 18, 2017"
output: html_document
---

## Reproducable Reasearch Week 2 Project 1

## Set working directory
```{r}
setwd("C:/Users/Bill/Documents/Coursera/JohnsHopkins/Course 5 - Reproducible Research/Week 2/Project 1")
```

## Download data file and unzip
```{r}
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity.zip")
unzip("activity.zip", exdir = ".")
```

```{r}
library(ggplot2)
library(data.table)
library(dplyr)
```

## Loading and preprocessing the data. 
## 1. Read data into Rstudio, change class of date variable from factor to date. 
```{r}
activity <- read.table("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
sapply(activity,class)
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

## 1. Calculate the total number of steps taken per day
```{r}
sum_steps <- aggregate(activity$steps, by=list(date=activity$date), FUN=sum)
head(sum_steps)
colnames(sum_steps)[2] <- "steps"
```

## 2. Make a histogram of the total number of steps taken each day.
```{r}
hist(sum_steps$steps, breaks = 20, main = "Total Steps Per Day", xlab = "Steps Per Day", 
     ylab = "Frequency (Days)", col = "blue" )
```

## 3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
meanSteps <- mean(sum_steps$steps, na.rm = TRUE)
meanSteps
```

```{r}
medianSteps <- median(sum_steps$steps, na.rm = TRUE)
medianSteps
```

## What is the average daily activity pattern?

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r}
avgsteps <- aggregate(steps~interval, data = activity, mean)
plot(steps~interval, data = avgsteps, type="l", xlab = "Interval", ylab = "Average Steps",
     main = "Daily Average Steps Per Interval")
```

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
avgsteps[which.max(avgsteps$steps),]
```
## Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(activity$steps))
```

## 2. Devise a strategy for filling in all of the missing values by replacing the NAs in the dataset 
## with the average for that 5-minute interval. 
```{r}
nas <- is.na(activity$steps)
interval_mean <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE, simplify = TRUE)
```

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
data_complete <- activity
data_complete$steps[nas] <- interval_mean[as.character(data_complete$interval[nas])]
```

## Check for NAs
```{r}
sum(is.na(data_complete$steps))
```

```{r}
str(data_complete)
```

## 4. Make a histogram of the total number of steps taken each day 
```{r}
sum_steps2 <- aggregate(data_complete$steps, by=list(date=data_complete$date), FUN=sum)
colnames(sum_steps2)[2] <- "steps"
```

## 2. Make a histogram of the total number of steps taken each day.
```{r}
hist(sum_steps$steps, breaks = 20, main = "Total Steps Per Day", xlab = "Steps Per Day", 
     ylab = "Frequency (Days)", col = "green" )
```

## Calculate and report the mean and median total number of steps taken per day. 
```{r}
meanSteps2 <- mean(sum_steps2$steps, na.rm = TRUE)
meanSteps2
```

```{r}
medianSteps2 <- median(sum_steps2$steps, na.rm = TRUE)
medianSteps2
```

## Do these values differ from the estimates from the first part of the assignment? 
## The mean and meadian are now equal. 

## Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" using the weekdays() function indicating whether a given date is a weekday or weekend day.
```{r}
data_complete_day <- mutate(data_complete, day = ifelse(weekdays(data_complete$date) == "Saturday" |
                                                          weekdays(data_complete$date) == "Sunday",
                                                        "Weekend", "Weekday"))
data_complete_day$day <- as.factor(data_complete_day$day)
str(data_complete_day)
```

```{r}
head(data_complete_day)
```

## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

## Get average steps
```{r}
interval_complete <- data_complete_day %>% group_by(interval,day) %>% summarise(steps=mean(steps))
```

## Time series panel plot
```{r}
s <- ggplot(interval_complete, aes(x=interval, y=steps, color = day)) +
  geom_line() +
  facet_wrap(~day, ncol = 1, nrow = 2)
print(s)
```

## There is more early activity on weekdays and overall more activity on the weekends. 