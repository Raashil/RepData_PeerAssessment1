---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

The present project analyses how many steps a person takes in a day, clustered by intervals of 5 minutes.
It explains how to get and organize data, also how to process so the researcher can get useful informations.



## Loading and preprocessing the data

The first step of this project is composed by preparing the RStudio, loading the data from web and process it to further analyses. Code below will install and load packages if they're not already loaded, otherwise it will do anything. The subsequent code will create variable "day of week", with corresponding day of week for each date.

Installing and loading packages:

```r
packages <- c("ggplot2", "dplyr", "lubridate")

package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)
```


Downloading and processing data:

```r
directory <- getwd()
dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName <- "movement.zip"
dataPath <- paste(directory, fileName, sep = "/")
folderName <- "movement"

if (!file.exists(fileName)) {
    download.file(url = dataUrl, destfile = dataPath)
}

if (!file.exists(folderName)) {
    unzip(dataPath, exdir = folderName)
}

activity <- read.csv(paste(directory, folderName, "activity.csv", sep = "/"))

activity$date <- ymd(activity$date)
activity$weekend <- as.factor(ifelse(weekdays(activity$date)=="sábado" | weekdays(activity$date)=="domingo","weekend","weekday"))
activity$dayofweek <- as.factor(weekdays(activity$date))
```



## What is mean total number of steps taken per day?

To get mean of steps per day, must sum total number of steps per day, grouping it by day, as follows:

```r
stepsbyday <- activity %>%
    group_by(date) %>%
    summarise(steps = sum(steps))
head(stepsbyday)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


After data is properly processed, can be created an histogram to observe distribution of steps per day:

```r
qplot(steps,data=stepsbyday,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

<img src="PA1_template_files/figure-html/hist steps per day-1.png" style="display: block; margin: auto;" />


Also with grouped data, mean and median of steps taken per day can be calculated:

```r
mean(stepsbyday$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsbyday$steps, na.rm = TRUE)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

Data can be grouped by interval, so can be observed the pattern of activity through the day.

```r
stepsbyinterval <- activity %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = TRUE))
head(stepsbyinterval)
```

```
## # A tibble: 6 x 2
##   interval  steps
##      <int>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
## 6       25 2.09
```



```r
ggplot(stepsbyinterval, aes(x=interval, y=steps))+ geom_line()
```

<img src="PA1_template_files/figure-html/steps by interval graph-1.png" style="display: block; margin: auto;" />


Other informations can be taken when data is grouped by interval, like interval of major activity, as follows: 

```r
data.frame(stepsbyinterval[which(stepsbyinterval$steps== max(stepsbyinterval$steps)),])
```

```
##   interval    steps
## 1      835 206.1698
```



## Imputing missing values

Missing value can disturb analyses results, misguiding researches. As shown bellow, this data set has great number of missing values, that must be fixed, once research has to have consistent results.

```r
nrow(activity)-nrow(na.omit(activity))
```

```
## [1] 2304
```


To fix missing values, they will be replaced with mean calculated removing NA.
After fixing missing values issue, a new histogram exhibit a better distribution, without high concentration in 0.

```r
activity_no_NA <- activity[which(!is.na(activity$steps)),]
interval_only <- activity_no_NA %>% group_by(interval) %>% summarise(average=mean(steps))
interval_only$average <- as.integer(interval_only$average)
activity_na <- activity[which(is.na(activity$steps)),]
activity_na$steps <- ifelse(activity_na$interval==interval_only$interval,interval_only$average)
activity_impute <- rbind(activity_no_NA,activity_na)
nrow(activity_na)
```

```
## [1] 2304
```



```r
stepsByDay_impute <- activity_impute %>% group_by(date) %>% summarise(stepsperday = sum(steps))
qplot(stepsperday,data=stepsByDay_impute,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

<img src="PA1_template_files/figure-html/histogram after fixing missing value-1.png" style="display: block; margin: auto;" />


Also, a new mean and median can be calculated.

```r
totalstepsperday_impute <- activity_impute %>% group_by(date) %>% summarise(stepsperday = sum(steps))
  mean_n_median <- totalstepsperday_impute %>% summarise(average=mean(stepsperday),median=median(stepsperday))
  mean_n_median
```

```
## # A tibble: 1 x 2
##   average median
##     <dbl>  <int>
## 1  10750.  10641
```



## Are there differences in activity patterns between weekdays and weekends?

Finally, a analysis about the difference of activity shows that in a weekday, people are more active in the first 1000 minutes of the day, then it significantly reduces, while in weekends, steps are more equally distributed through the day.

```r
meansteps <- activity_impute %>%
  group_by(interval,weekend) %>%
  summarise(average = mean(steps))

  qplot(interval,average,data=meansteps,geom="line",facets=weekend~.,xlab="5-minute interval",ylab="average number of steps",main="Average steps pattern between Weekday and Weekend")
```

<img src="PA1_template_files/figure-html/comparing steps taken in weekdays vs weekends-1.png" style="display: block; margin: auto;" />







































