### Introduction

## It is now possible to collect a large amount of data about personal movement using activity monitoring devices such
## as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a
## group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in
## their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are
## hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.
## This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute
## intervals through out the day. The data consists of two months of data from an anonymous individual collected
## during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Data Processing
library(ggplot2)

activity<- read.csv("D:/profile/documents/GitHub/activity.csv")
activity$date<- as.Date.factor(activity$date, "%m/%d/%Y" )
weekday<- weekdays(activity$date)
activity <-cbind(activity, weekday)
summary(activity)

## 1. What is mean total number of steps taken per day?
activity.tsteps<- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity.tsteps)<- c("dates", "steps")
hist(activity.tsteps$steps, main = "Total number of steps taken per day", xlab = "Total s
teps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))

# Mean number of steps taken per day
mean(activity.tsteps$steps)

# Median number of steps taken per day
median(activity.tsteps$steps)

## 2. What is the average daily activity pattern?
average.daily.activity<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)
names(average.daily.activity)<-c("interval", "mean")
plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")

average.daily.activity[which.max(average.daily.activity$mean),]$interval

## 3. Imputing missing values
sum(is.na(activity$steps))
clean.steps<- average.daily.activity$mean[match(activity$interval,average.daily.activity$interval)]

activity.clean <- transform(activity, steps = ifelse(is.na(activity$steps), yes = clean.steps, no = activity$steps))
total.clean.steps<- aggregate(steps ~ date, activity.clean, sum)
names(total.clean.steps)<- c("date", "daily.steps")

# Mean of the total number of steps taken per day
mean(total.clean.steps$daily.steps)

# Median of the total number of steps taken per day
median(total.clean.steps$daily.steps)

## 4. Are there differences in activity patterns between weekdays and weekends?
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday")
  {y <- "Weekend"} else
  {y <- "Weekday"}
  y
})

activity.datetype<- aggregate(steps~interval+datetype, activity,mean, na.rm =TRUE)
ggplot(activity.datetype, aes(x = interval, y = steps, color = datetype))+ geom_line() + labs(title = "Average daily steps by date type", x = "Interval", y = "Average number of steps") + facet_wrap(~datetype, ncol = 1, nrow = 2) 

