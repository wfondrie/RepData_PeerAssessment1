# Reproducible Research: Peer Assessment 1

Below is my submission for the first peer assessment in the Reproducible Research in the Coursera Data Science specialization. 

## Loading and preprocessing the data

The data was loaded into R by directly downloading from the link provided. It was then unzipped and read into R using read.csv(). Minimal preprocessing was done at this point, only to convert fields to appropriate formats.



```r
# 1. Reading in the data:
# Data downloaded on 08/15/2015
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              "data/data.zip", method ="curl")
unzip("data/data.zip", exdir = "data")

data <- read.csv("data/activity.csv")
data$date <- as.Date(as.character(data$date))
data$steps <- as.numeric(data$steps)
```


## What is mean total number of steps taken per day?

The code below produces a histogram of the total number of steps per day. 

```r
# 2. Total steps per day -> histogram -> report mean/median
library("plyr")
library("ggplot2")
library("reshape2")
library("knitr")

## Total steps
daySums <- ddply(data[,c("date","steps")], .(date), summarise,
                 TotalSteps = sum(steps))
names(daySums)[2] <- "steps"

## Making histogram
histogram <- ggplot(daySums, aes(steps)) + 
    geom_bar(binwidth=1000) + # geom_bar in ggplot makes a histogram with one variable
    xlab("Total Steps in a Day") +
    ylab("Number of Days") +
    ggtitle("Histrogram of Total Steps per Day")
histogram
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median are then calculated and displayed in the table below.


```r
## Calculating Median and Mean
values <- data.frame(Mean = mean(daySums$steps, na.rm = T), 
                     Median = median(daySums$steps, na.rm = T))

kable(values,row.names=F, align = 'l')
```



Mean       Median 
---------  -------
10766.19   10765  

## What is the average daily activity pattern?

The average number of steps was calculated for each time interval and plotted as a line plot. The code below generates this.


```r
# 3. Time series of 5 min periods averaged accross all days
## Calculating interval means over all days
meanInt <- ddply(data[,c("interval","steps")], .(interval), summarise,
                 MeanSteps = mean(steps, na.rm = T))
plot(meanInt$interval,meanInt$MeanSteps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

From the interval averages, the interval with the highest average number of steps was chose. this is displayed in the table below.


```r
## Determining the highest averaging interval
maximum <- meanInt[meanInt$MeanSteps == max(meanInt$MeanSteps),]

kable(maximum,row.names=F, align = 'l')
```



interval   MeanSteps 
---------  ----------
835        206.1698  


## Imputing missing values

The missing values were imputed using the average number of steps for a given interval. This results the same overall average in the data, as shown by the histogram.


```r
# 4. Imputing NA values
## Counting number of NA's
numNA <- sum(is.na(data$steps))

## imputing the NA values
data <- ddply(data, .(date,interval), mutate, 
               filledSteps =  if(is.na(steps)){
                   meanInt[interval == meanInt$interval,"MeanSteps"]
                   } else { steps })

## Total steps
daySumsFilled <- ddply(data[,c("date","filledSteps")], .(date), summarise,
                 TotalSteps = sum(filledSteps))
names(daySumsFilled)[2] <- "steps"

## Making histogram
histogram2 <- ggplot(daySumsFilled, aes(steps)) + 
    geom_bar(binwidth=1000) + # geom_bar in ggplot makes a histogram with one variable
    xlab("Total Steps in a Day") +
    ylab("Number of Days") +
    ggtitle("Histrogram of Total Steps per Day")
histogram2
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Below we print the median and mean for both datasets (imputed and with NA's). While the imputation minorly changes the median of the data, it intentionally leaves the mean unchanged.


```r
## Calculating Median and Mean
values[2,] <- data.frame(Mean = mean(daySumsFilled$steps, na.rm = T), 
                     Median = median(daySumsFilled$steps, na.rm = T))
row.names(values) <- c("With NA's", "NA's Imputed")

kable(values,row.names=T, align = 'l')
```

               Mean       Median   
-------------  ---------  ---------
With NA's      10766.19   10765.00 
NA's Imputed   10766.19   10766.19 

## Are there differences in activity patterns between weekdays and weekends?

Below we summarize the average number of steps by weekdays and weekends. This is then plotted in separate panels using the ggplot2 plotting system.


```r
# 5. Weekdays vs Weekends
data$day <- weekdays(data$date)
data$weekday <- rep("Weekday",nrow(data))
data$weekday[data$day == "Saturday" | data$day == "Sunday"] <- "Weekend"
data$weekday <- as.factor(data$weekday)
avgStepsWeek <- ddply(data, .(interval, weekday), summarise,
                      avgSteps = mean(filledSteps))

plot <- ggplot(avgStepsWeek, aes(x=interval, y=avgSteps)) + 
    facet_grid(weekday ~ .) + 
    geom_line() +
    xlab("Time interval") +
    ylab("Average number of steops") +
    ggtitle("Average Steps on Weekdays and Weekends")
plot
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


Thank you for reading!
