---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
  keep_md : true
---

### Loading and preprocessing the data

First of all we read the csv file into our variable called `dat`
```{r, warning=FALSE, message=FALSE}
library(readr)
dat <- read_csv('activity.csv') #file has been unzipped and put in the working directory
```

### What is mean total number of steps taken per day?

We will use `dplyr` and `ggplot2` package for graphing, the original data will be first grouped by the date before making the histogram plot
```{r, warning=FALSE, message=FALSE}
library(dplyr)
library(ggplot2)
library(lubridate)
pro1 <- dat %>% group_by(date) %>% summarize(total.steps = sum(steps, na.rm = T))
g1 <- ggplot(pro1, aes(total.steps))
g1 + geom_histogram(color = 'blue', binwidth = 2500) + labs(x = 'Total steps', y = 'Number of days', title = 'Total number of steps taken each day') + theme(plot.title = element_text(hjust = 0.5))

```

We can use the same dataset `pro1` generated previously to compute mean and median
```{r, warning=FALSE, message=FALSE}
# For mean value
print(paste('Mean number of steps taken each day is:', round(mean(pro1$total.steps, na.rm = T)), sep = ' '))
print(paste('Median number of steps taken each day is:', round(median(pro1$total.steps, na.rm = T)), sep = ' '))
```

### What is the average daily activity pattern?

In this problem we grouped the original data by interval, time series of plot is better presented as trend line.
```{r,warning=FALSE, message=FALSE}
pro4 <- dat %>% group_by(interval) %>% summarize(average.steps = mean(steps, na.rm = T))
g4 <- ggplot(pro4, aes(interval, average.steps))
g4 + geom_line() + labs(x = 'Time interval', y = 'Average steps', title = 'Time series plot of the average number of steps taken') + theme(plot.title = element_text(hjust = 0.5))
```

Using the previously generated dataset `pro4`, we can find the maximum number of steps and the interval associated with it
```{r}
max_day <- pro4[which.max(pro4$average.steps), 'interval']
print(paste('The 5-minute interval of', max_day, 'contains the maximum number of steps', sep = ' '))
```

### Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
print(paste('Total number of missing values in the dataset is', sum(is.na(dat$steps))))
```
2. Devise a strategy for filling in all of the missing values in the dataset.

We will first find the index of NA in column 'steps' and fill in the associated average steps of the interval. Then we merge with original dataset `dat` with `pro4` that has average steps for each interval. Finally, we mapped the average steps into blanks with the associated index
```{r}
pro5 <- merge(dat, pro4, by = 'interval') %>% arrange(date)
pro5[which(is.na(pro5$steps)),'steps'] <- round(pro5[which(is.na(pro5$steps)), 'average.steps'])
dat_filled <- pro5 %>% select(steps, date, interval) # new dataset that has NA filled 
```
3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Repeat the first few steps we did at the beginning with the new dataset `dat_filled`
```{r}
pro6 <- dat_filled %>% group_by(date) %>% summarize(total.steps = sum(steps, na.rm = T))
g6 <- ggplot(pro6, aes(total.steps))
g6 + geom_histogram(color = 'blue', binwidth = 2500) + labs(x = 'Total steps', y = 'Number of days', title = 'Total number of steps taken each day(with filled data') + theme(plot.title = element_text(hjust = 0.5))
```

We can use the same dataset `pro1` generated previously to compute mean and median
```{r}
print(paste('Mean number of steps taken each day is:', round(mean(pro6$total.steps, na.rm = T)), sep = ' '))
print(paste('Median number of steps taken each day is:', round(median(pro6$total.steps, na.rm = T)), sep = ' '))
```
We can see that by filling up the NA values, both the mean and median number increase

### Are there differences in activity patterns between weekdays and weekends?

To answer this question, we need to create another variable `day` to show the date is weekday or weekend, this can be done using `lubridate` package as we loaded previously

```{r}
dat_day <- dat_filled %>% mutate(day = ifelse(wday(date) >= 1 & wday(date) <= 5, 'Weekday', 'Weekend' )) %>% group_by(interval, day) %>% summarize(average.steps = mean(steps))
g7 <- ggplot(dat_day, aes(interval, average.steps))
g7 + geom_line(aes(color = day)) + facet_wrap(~day, nrow = 2) + labs(x = 'Time interval', y = 'Average steps', title = 'Weekday vs Weekend activity') + theme(plot.title = element_text(hjust = 0.5))
```


