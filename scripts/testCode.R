
# 1. Reading in the data:

# Data downloaded on 08/15/2015
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              "data/data.zip", method ="curl")
unzip("data/data.zip", exdir = "data")

data <- read.csv("data/activity.csv")
data$date <- as.Date(as.character(data$date))
data$steps <- as.numeric(data$steps)

# 2. Total steps per day -> histogram -> report mean/median
library("plyr")
library("ggplot2")
library("xtable")
library("reshape2")

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

## Calculating Median and Mean
values <- data.frame(Mean = mean(daySums$steps, na.rm = T), 
                     Median = median(daySums$steps, na.rm = T))

valTable <- xtable(values, type = "html")
print(valTable)

# 3. Time series of 5 min periods averaged accross all days
## Calculating interval means over all days
meanInt <- ddply(data[,c("interval","steps")], .(interval), summarise,
                 MeanSteps = mean(steps, na.rm = T))
plot(meanInt$interval,meanInt$MeanSteps, type="l")

## Determining the highest averaging interval
maximum <- meanInt[meanInt$MeanSteps == max(meanInt$MeanSteps),]
maxTable <- xtable(maximum)
print(maxTable)
