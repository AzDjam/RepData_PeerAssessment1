---
title: "Reproducible Research projet1"
author: "Djamila Azib"
date: "March 9, 2018"
output:
  html_document: default
  
---



#Loading and preprocessing the data


```r
if (!file.exists("dfActivity.csv")) 
  unzip("activity.zip") 
dfActivity<-read.table("activity.csv",header=T,sep=",")
dfActivity$date<- as.Date(dfActivity$date)
```

#What is mean total number of steps taken per day?

**1. Make a histogram of the total number of steps taken each day**


```r
stepsPerDay <- dfActivity%>% group_by(date)%>%filter(!is.na(steps))%>%
               summarise(totalSteps = sum(steps, na.rm=TRUE))

hist(stepsPerDay $totalSteps, breaks=5, xlab="total Steps", main = "Total Steps per Day")
```

<img src="Figure/fig1 Total Steps per Day -1.png" width="672" />

**2. Calculate and report the mean and median total number of steps taken**


```r
# mean of Steps
meanSteps<-mean(stepsPerDay$totalSteps)
cat("The average number of steps taken each day is:",meanSteps,"steps")
```

```
## The average number of steps taken each day is: 10766.19 steps
```

```r
# Median of Steps
medianSteps<-median(stepsPerDay$totalSteps)
cat ("The median number of steps taken each day is ", medianSteps)
```

```
## The median number of steps taken each day is  10765
```

#What is the average daily activity pattern?


**1.Create line plot of average number of steps per interval across all days**





```r
stepsPerInterval<-dfActivity%>% group_by(interval)%>%filter(!is.na(steps))%>%
               summarise(AvgSteps = mean(steps, na.rm=TRUE))
g<- ggplot(stepsPerInterval, aes(x=interval, y=AvgSteps))
g+      geom_line(color="blue", size=1) +
      labs(title = "Average steps per interval across all days", 
       x = "interval", y = "Avg Steps per interval")
```

<img src="Figure/fig2 Average steps per Interval-1.png" width="672" />


       
**2. Which interval, on average across all the days contains the maximum of steps?**


```r
##Maximum steps by interval
maxSteps <- max(stepsPerInterval$AvgSteps)
cat("the maximum steps is",maxSteps)
```

```
## the maximum steps is 206.1698
```

```r
##Which interval contains the maximum average number of steps
maxInterval=stepsPerInterval[stepsPerInterval$AvgSteps==maxSteps,1]
cat("The interval contains the maximum average number of steps is",maxInterval$interval)
```

```
## The interval contains the maximum average number of steps is 835
```

#Imputing missing values


**1. Calculate and report the total number of missing values in the dataset**


```r
nbNA=sum(is.na(dfActivity$steps))
cat("The total number of missing values =",nbNA)
```

```
## The total number of missing values = 2304
```


**2. Devise a strategy for filling in all of the missing values in the dataset** 

The missing data are replaced by the mean of steps per interval across all days 



```r
tidyActivity<- dfActivity

avgInterval<- tapply(tidyActivity$steps, tidyActivity$interval, mean, na.rm=TRUE)
```



**3. Create a new dataset  equal to the original dataset  with the missing data filled in**


```r
NAs<- is.na(tidyActivity$steps)
tidyActivity$steps[NAs] <- avgInterval[as.character(tidyActivity$interval[NAs])]
```


**4. Make a histogram of the total number of steps taken each day and Calculate**



```r
stepsPerDay2<- tidyActivity%>%
group_by(date)%>%
summarise(totalSteps = sum(steps, na.rm=TRUE))


hist(stepsPerDay2 $totalSteps, breaks=5, xlab="total Steps ", main = "Daily Steps filling the missing data")
```

<img src="Figure/fig3 Dailly steps with NAs Replaced -1.png" width="672" />

```r
meanSteps2<- mean(stepsPerDay2$totalSteps, na.rm=TRUE)
cat("The average number of steps taken each day is:",meanSteps2,"steps")
```

```
## The average number of steps taken each day is: 10766.19 steps
```

```r
medianSteps2<- median(stepsPerDay2$totalSteps, na.rm=TRUE)
cat ("The median number of steps taken each day is ", medianSteps2)
```

```
## The median number of steps taken each day is  10766.19
```


##What is the impact of imputing missing data on the estimates of the totaldaily number of steps?

Before filling in all the missing values in the dataset,the mean is 10766.19  and the  median is 10765 

After filling in all the missing values in the dataset,the mean remains the same, while the median value increased slightly ( 10766.19)


#Are there differences in activity patterns between weekdays and weekends?

**1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" **


```r
tidyActivity<- tidyActivity%>%
 mutate(weekType= ifelse(weekdays(tidyActivity$date)=="Saturday" | weekdays(tidyActivity$date)=="Sunday", "Weekend", "Weekday"))

stepsPerInterval2<<- tidyActivity%>%
group_by(interval, weekType)%>%summarise(avgSteps = mean(steps, na.rm=TRUE))
```


**2. Make a panel plot containing a time series plot of the interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**


```r
g<- ggplot(stepsPerInterval2, aes(x=interval, y=avgSteps,color=weekType))
 g+      geom_line() +
     labs(title = "Average steps per interval across all days", 
          x = "interval", y = "Avg Steps per interval")+facet_wrap(~weekType, ncol = 1, nrow=2)
```

<img src="Figure/fig4 Averagesteps  Weektype-1.png" width="672" />

The plot shows a slight difference in activity during the weekdays and the weekend days.

