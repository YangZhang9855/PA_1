Peer Assessment 1 by Yang Zhang
===============================

Loading and preprocessing the data

```{r,echo=TRUE}
activity<-read.csv('~/Desktop/Coursera/RR/activity.csv',header=TRUE)
```

What is mean total number of steps taken per day?
*Make a histogram of the total number of steps taken each day

```{r,echo=TRUE}
attach(activity)
sumstep<-tapply(steps,date,sum,na.rm=TRUE)
barplot(sumstep,xlab='date',ylab='sumstep')
detach(activity)
```
*Calculate and report the mean and median total number of steps taken per day
```{r,echo=TRUE}
sumstep<-as.data.frame(as.table(sumstep))
colnames(sumstep)<-c('date','sumstep')
mean(sumstep$sumstep)
median(sumstep$sumstep)
```
What is the average daily activity pattern?
*Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r,echo=TRUE}
attach(activity)
pattern<-tapply(steps,interval,mean,na.rm=TRUE)
plot(pattern,xlab='interval',ylab='steps')
```
*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r,echo=TRUE}
maxstep<-max(pattern)
pattern2<-as.data.frame(as.table(pattern))
colnames(pattern2)<-c('interval','meanstep')
maxstep<-pattern2[which(pattern2$meanstep==maxstep),]
maxstep
```
Imputing missing values
*Calculate and report the total number of missing values in the dataset 
```{r,echo=TRUE}
sum(is.na(activity$steps))
```
*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated,creat the new dataset data1
```{r,echo=TRUE}
data1<-activity$steps
data2<-sumstep<-tapply(steps,date,mean,na.rm=TRUE)
data2<-rep(data2,each=length(data1)/length(data2))
missing <- which(is.na(data1))
data1[missing] <- data2[missing]
activity$newsteps<-NULL
activity$newsteps<-data1
```
*Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
```{r,echo=TRUE}
attach(activity)
sumstep2<-tapply(newsteps,date,sum,na.rm=TRUE)
barplot(sumstep2,xlab='date',ylab='sumstep')
activity$newstep<-NULL
detach(activity)
```
*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Answer: No.To improve the observation numbers or sample size.

Are there differences in activity patterns between weekdays and weekends?
*Create a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.
```{r,echo=TRUE}
attach(activity)
activity$day<-NULL
activity$day<-weekdays(as.Date(date))
newv<-ifelse(activity$day %in% c('Saterday','Sunday'), 'weekend', 'weekday')
activity$day<-newv
detach(activity)
```
*Make a panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r,echo=TRUE}
attach(activity)
weekdays<-activity[day=='weekday',]
weekend<-activity[day=='weekend',]
detach(activity)
attach(weekdays)
pattern2<-tapply(steps,interval,mean,na.rm=TRUE)
plot(pattern2,main='weekday',xlab='interval',ylab='steps')
detach(weekdays)
attach(weekend)
pattern2<-tapply(steps,interval,mean,na.rm=TRUE)
plot(pattern2,main='weekend',xlab='interval',ylab='steps')
```
