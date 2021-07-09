---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
data <- read.csv(unz('activity.zip', 'activity.csv'), header = TRUE, sep = ",", )
unlink(temp)

data$date <- as.Date(data$date)
data$day <- weekdays(data$date)
```

##Histogram of the total number of steps taken each day



```r
#Group data by date, and sum the steps for each day
gpd <- data %>% group_by(date) %>% summarize(summedsteps = sum(steps))
#make weekdate column for graph
gpd$date <- as.Date(gpd$date)
gpd$day <- weekdays(gpd$date)
gpd$Date <- paste(gpd$day, gpd$date, sep = ".")

#open png
#png('plot1.png')
#graph barplot: indicate y-axis data, x-axis data
par(mar = c(10, 6, 1, 1))
barplot(height = gpd$summedsteps, names = gpd$Date, 
        #reduce x-axis label font size, reduce y axis label font size
        cex.names = 0.5, cex.axis = 0.5, 
        #adjust x-axis label (side 1): text, and move it down (line = 6)
        xlab = "Date", cex.lab = 2, cex.lab = 0.8,
        # make x-axis labels vertical
        las = 2,
        #adjust y-axis label (side 2): text, and move it left (line = 4)
        ylab = "Number of Steps",
        #title of chart
        main = "Number of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#turn off png        
#dev.off()
#resent default device
#getOption("device")
```


## What is mean total number of steps taken per day?

```r
mn <- round(mean(gpd$summedsteps, na.rm = TRUE), 2)
md <- median(gpd$summedsteps, na.rm = TRUE)
str1 <- "The mean number of steps taken each day is "
str2 <- ", and the median number of steps taken each day is "
str3 <- "."
print(paste(str1, mn, str2, md, str3))
```

```
## [1] "The mean number of steps taken each day is  10766.19 , and the median number of steps taken each day is  10765 ."
```


## What is the average daily activity pattern?



```r
# group data by date and get the mean and median number of steps for each day
# these measures of central tendency will be for five minute intervals.
gpd1 <- data %>% group_by(date) %>% summarize(meansteps  = 
        as.numeric(mean(steps, na.rm = TRUE)), mediansteps = median(steps, 
        na.rm = TRUE))

#modify column names for the modified x-axis labels
gpd1$date <- as.Date(gpd1$date)
gpd1$day <- weekdays(gpd1$date)
gpd1$Date <- paste(gpd1$day, gpd1$date, sep = ".")

#open png
#png('plot2.png')

# I don't care for the grey background so I go for the classic theme.
# plot line graph       
ggplot()  + theme_classic()+ geom_line(aes(y = meansteps, x = Date, group = 1), 
        #omiting na's will make for a continuous line
        data = na.omit(gpd1)) +
        #title the graph
        ggtitle("Five-Minute Step Averages per Day") + 
        #adjust axes text              center the title
        theme(plot.title = element_text(hjust = 0.5),
        #make room on the    top   bottom and left
        plot.margin = margin(12, 2, 10,        12),
        #adjust axis label. make x labels vert, adjust justifications and size
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        #adjust yaxis label, adjust justifications and size
        axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 6)) +
        #give x and y axes titles
        xlab("Date") + ylab("Five-Minute Step Average")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#turn off png        
#dev.off()
#resent default device
#getOption("device")
```

####The 5-minute interval that, on average, contains the maximum number of steps


```r
#find which day has the max reading
daymax <- gpd1[which.max(gpd1$meansteps),]
#subset the data for that day
subbed <- subset(data, date == daymax$date)
#find the max reading of that day
max5 <- max(subbed$steps)
#identify the interval
w <- which.max(subbed$steps)
timeofday <- round(w/12,2)
s <- paste("The ", w,"th interval, is the maximum steps taken on", daymax$date,
           ", the day of the average five-minute maximum, which is ", timeofday, "or 3:40 pm.")
print(s)
```

```
## [1] "The  185 th interval, is the maximum steps taken on 2012-11-23 , the day of the average five-minute maximum, which is  15.42 or 3:40 pm."
```


## Imputing missing values

There are a bunch of missing data for steps taken throughout the dataset.
How much missing data in 'steps'?

```r
nuNa <- sum(is.na(data$steps))
str4 <- "There are a total of "
str5 <- " missing points of data in 'steps'."
print(paste(str4, nuNa, str5))
```

```
## [1] "There are a total of  2304  missing points of data in 'steps'."
```

The mean steps taken per day is 10766.19. But we can't impute the missing values
with that since the missing values are for 5 minute time intervals. We have
to find the five minute average for each day. Each day will have it's own
average to substitute in, the average of which is 37.38.

We can group the data by date, and get the averages per date:

```r
datemean <- data %>% group_by(date) %>% summarize(meansteps = mean(steps))
#to calculate the above average of averages:
mean(datemean$meansteps, rm.na = TRUE)
```

```
## [1] NA
```

The difficulty I had was finding a way to grab each value from that vector of
61 averages, and substitute that one value for each instance of 'na' for that
day in the original dataset.

Hours of trial and error, and internet searching later, I found this snippet
which I modified for this problem, which seems to do the trick.
(I could provide a bunch of failed attempts, but I'm not sure that's part of the
assignment.)

```r
data$steps <- with(data, ave(steps, date, FUN = function(x) replace(x, is.na(x), 
        mean(x, na.rm = TRUE))))
```




```r
#Group data by date, sum by steps. Note, this data has the imputed values
newdf <- data %>% group_by(date) %>% summarize(summedsteps = sum(steps))
newdf <- na.omit(newdf)

#modify column names for the modified x-axis labels
newdf$day <- weekdays(newdf$date)
newdf$Date <- paste(newdf$day, newdf$date, sep = ".")

#open png
#png('plot3.png')
#Set up graph area
par(mar = c(6, 6, 1, 1))
#graph barplot: indicate y-axis data, x-axis data
barplot(height = newdf$summedsteps, names = newdf$date, 
        #title of chart
        main = "Number of Steps per Day", xlab = "Date",
        # make x-axis labels vertical
        las = 2,
        #reduce x-axis label font size, reduce y axis label font size
        cex.names = 0.5, cex.axis = 0.5) + 
        #adjust x-axis label (side 1): text, and move it down (line = 6)
       # mtext(side = 1, text = "Date") +
        #adjust y-axis label (side 2): text, and move it left (line = 4)
        mtext(side = 2, text = "Number of Steps", line = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```
## numeric(0)
```

```r
#turn off png        
#dev.off()
#resent default device
#getOption("device")
```

Comparing this histogram with the earlier one, we can see data are filled in, 
but the averages don't noticably change.

What are the new values for central tendency?

```r
mn2 <- round(mean(gpd$summedsteps, na.rm = TRUE), 2)
md2 <- median(gpd$summedsteps, na.rm = TRUE)
str6 <- "The mean number of steps taken each day is "
str7 <- ", and the median number of steps taken each day is "
str8 <- "."
print(paste(str6, mn2, str7, md2, str8))
```

```
## [1] "The mean number of steps taken each day is  10766.19 , and the median number of steps taken each day is  10765 ."
```

## Are there differences in activity patterns between weekdays and weekends?


```r
#Remove the missing values
dataO <- na.omit(data)
#Turn the weekdays into factors
wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dataO$d <- factor((weekdays(dataO$date) %in% wdays), levels = c(FALSE, TRUE), 
        labels = c('weekend', 'weekday'))
#subset the dataframe into 2 by factor
dataweekends <- dataO[dataO$d == "weekend",]
dataweekdays <- dataO[dataO$d == "weekday",]
#Group subsets by time interval and take mean number of steps for each time interval
gdataweekends <- dataweekends %>% group_by(interval) %>% summarize(meansteps = mean(steps))
gdataweekdays <- dataweekdays %>% group_by(interval) %>% summarize(meansteps = mean(steps))
#Set the range of the y-axis so both panels are the same
rng <- range(gdataweekends$meansteps, gdataweekdays$meansteps)
#initiate the plot
#png('plot4.png')
par(mfrow = c(2,1), mar = c(4, 4, 2, 1))
#Top panel - 'Weekends'
plot(gdataweekends$interval, gdataweekends$meansteps, type = "l", pch = 20, 
     col = "blue", ylim = rng,
     main = "Average daily steps on \nweekends", cex.main = 0.75,
     xlab = "", cex.axis = 0.5, cex.lab = 0.8,
     ylab = "", las = 2,)
#Bottom panel - 'Weekdays'
plot(gdataweekdays$interval, gdataweekdays$meansteps, type = "l", pch = 20, 
     col = "blue", ylim = rng,
     main = "Average daily steps on \nweekdays", cex.main = 0.75,
     xlab = "Interval", ylab = "", cex.axis = 0.5, cex.lab = 0.8,
     las = 2)
#Marginal text to be used for y axis label
mtext("Number of Steps", side = 2, adj = 2.2, padj = -4, cex = 0.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#turn off png        
#dev.off()
#resent default device
#getOption("device")
```
