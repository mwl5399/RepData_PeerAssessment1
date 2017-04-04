---
title: "ReproducibleResearch1"
author: "Matt Livingston"
date: "April 2, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load the dataset.

```{r load}
# Read the dataset.
data <- read.csv("activity.csv")
```

## Steps per day

```{r histogram}
# Use aggregate to compile Daily Data.
dailyData <- aggregate(steps ~ date, data, sum)
hist(dailyData$steps,xlab = "Steps Per Day",main = "Histogram of Daily Steps")
avg <- mean(dailyData$steps)
med <- median(dailyData$steps)
```

## Time series plot

```{r time_series}
# Use aggregate to compile a time series.
td <- aggregate(steps ~ interval, data, sum)
plot(td$steps ~ td$interval, type = "l",xlab = "Time",ylab = "Steps")

# Find the index of the max value and the value of interval in that row.
rowMax <- which.max(td$steps)
max <- td[rowMax,]
maxInt <- max$interval
```

## Filtering the NA's

```{r NAs}
# Count the NAs and assign a replacement variable.
NAs <- sum(is.na(data))
rep <- mean(data$steps, na.rm = TRUE)
data[is.na(data)] <- rep

# Use aggregate as before. 
newDailyData <- aggregate(steps ~ date, data, sum)
hist(newDailyData$steps,xlab = "Steps Per Day", main = "Histogram of Daily Steps")
newAvg <- mean(newDailyData$steps)
newMed <- median(newDailyData$steps)
```

## Plot weekday vs. weekend activity

```{r Days}
# Create a day of the week column. The data is now cleaned of NAs.
data$day <- weekdays(as.Date(data$date))

# Define all days as weekdays
data$daytype <- "weekday"

# Separate the weekends.
data$daytype[data$day %in% c("Saturday", "Sunday")] <- "weekend"

# Find the mean at each interval for each daytype.
library(ggplot2)
dayMeans <- aggregate(steps ~ interval + daytype, data, mean)
qplot(interval, steps, data = dayMeans, geom=c("line"),xlab = "Interval", 
        ylab = "Number of steps", main = "") +
        facet_wrap(~ daytype, ncol = 1)


```