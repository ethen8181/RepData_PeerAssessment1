# Coursera Reproducible Research Peer Assessment 1

# This assignment makes use of data from a personal activity monitoring device. 
# This device collects data at 5 minute intervals through out the day. 
# The data consists of two months of data from an anonymous individual collected 
# during the months of October and November, 2012 and include the number of steps taken 
# in 5 minute intervals each day.

# The variables included in this dataset are:
# steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was taken
# The dataset is stored in a comma-separated-value (CSV) file 
# there are a total of 17,568 observations in this dataset.


suppressMessages( library(dplyr) )
suppressMessages( library(plyr)  )
library(data.table)
library(reshape2)
library(ggplot2)

# Loading and preprocessing the data
setwd("C:/Users/ASUS/RepData_PeerAssessment1")
unzip("activity.zip")

files <- list.files()
DT    <- fread( files[1], stringsAsFactors = FALSE )


# ------------------------------------------------
# What is mean total number of steps taken per day


# calculate the total steps by for each day
totalstepsByDay <- DT[ , sum( steps, na.rm = TRUE ), by = date ]
setnames( totalstepsByDay, "V1", "Steps")


# 1. Make a histogram of the total number of steps taken each day

# create a function, the formal argument : 
# data = data to plot the graph ; fillcolor = color of the bar ; plottitle = title of the plot   
# change the labels of x-axis to prevent them from clumping together
totalStepPlot <- function( data, fillcolor, plottitle )
{
    ggplot( data, aes( x = date, y = Steps ) ) + geom_histogram( stat = "identity", fill = fillcolor ) +  
      ggtitle( plottitle ) + xlab( "Date: 2012/10/1 ~ 2012/11/30" ) + theme(
        plot.title = element_text( size = 24, face = "bold" ),
        axis.title = element_text( size = 18 ),
        axis.text  = element_text( size = 12 )) + scale_x_discrete( labels = c( seq(1,31), seq(1,30) ) )
}
totalStepPlot(totalstepsByDay, "royalblue", "Total number of steps taken each day" )
    

# 2. Calculate and report the mean and median total number of steps taken per day

# Create a function to see that with one single call
detail <- function(x)
{
    fun <- list( mean = mean, median = median )
    lapply(fun, function(f){ f(x) } )
}
detail1 <- unlist( detail(totalstepsByDay$Steps) )


# ------------------------------------------------
# What is the average daily activity pattern ?


# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
averagesteps <- DT[, list( mean = mean( steps, na.rm = TRUE ) ), by = interval ]

ggplot( averagesteps, aes( x = interval, y = mean ) ) + geom_line( size = 1.5, color = "aquamarine3" ) +
  ggtitle( "Average Steps Taken of Each Interval\n(averaged across all days)" ) + ylab( "Steps" ) + theme(
    plot.title   = element_text( size = 24, face = "bold" ),
    axis.title   = element_text( size = 18 ),
    axis.text    = element_text( size = 12 ))


# 2. Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
filter( averagesteps, mean == max(mean) )$interval


# ------------------------
# Imputing missing values


# 1. Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
sum( is.na(DT$steps) )


# 2. Devise a strategy for filling in all of the missing values in the dataset. The
# strategy does not need to be sophisticated. For example, you could use
# the mean/median for that day, or the mean for that 5-minute interval, etc.

# fill in the NAs with the mean for that 5-minute interval
NA_fill <- averagesteps$mean[ match( DT[ !complete.cases(DT) ]$interval , averagesteps$interval ) ] 


# 3. Create a new dataset that is equal to the original dataset but with the
# missing data filled in.

# convert the steps from integer class to numeric
# new dataset = DT1
DT$steps <- sapply( DT$steps, as.numeric )
DT1      <- DT

DT1[ !complete.cases(DT1) ]$steps <- NA_fill
View(DT)

# 4. Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day. Do
# these values differ from the estimates from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total
# daily number of steps?

# The dataset without filling in the NAs is totalstepsByDay
View(totalstepsByDay)
totalstepsByDay1 <- DT1[ ,list( Steps = sum( steps, na.rm = TRUE ) ) , by = date ]


# use the totalStepPlot, the formal arguments are 
# data = data to plot the graph ; fillcolor = color of the bar ; plottitle = title of the plot
totalStepPlot( totalstepsByDay1, "cyan3", "Total number of steps taken each day\n(With NAs filled)" )

# Use the function detail to calculate the mean and median for totalstepsByDay1.
# Compare it with detail1, 
# which is the mean and median of the dataset that NAs are not filled.
detail2 <- unlist( detail( totalstepsByDay1$Steps ) )
list( Excluded = detail1, filled = detail2)


# Plot totalstepsByDay, totalstepsByDay1 in one graph to compare
# Merge the two dataset together and reshape it to be used for ggplot
mergedata         <- merge  ( totalstepsByDay,totalstepsByDay1, by = "date" )
mergedata$Steps.x <- sapply ( mergedata$Steps.x, as.numeric )    
mergedata         <- melt   ( mergedata, id.vars = "date", value.name = "Steps", variable.name = "Data" )
mergedata$Data    <- revalue( mergedata$Data, c( "Steps.x" = "NAs excluded", "Steps.y" = "NAs filled" ) )
# head(mergedata)

ggplot( mergedata, aes( x = date , y = Steps, fill = Data  ) ) + geom_histogram( stat = "identity", position="dodge" ) +  
  ggtitle( "Total number of steps taken each day" ) + xlab( "Date: 2012/10/1 ~ 2012/11/30" ) + theme(
    plot.title   = element_text( size = 24, face = "bold" ),
    legend.title = element_text( size = 16 ),
    legend.text  = element_text( size = 12 ),
    axis.title   = element_text( size = 18 ),
    axis.text    = element_text( size = 12 )) + scale_x_discrete( labels = c( seq(1,31), seq(1,30) ) ) 



# -------------------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset
# with the filled-in missing values for this part.


# 1. Create a new factor variable in the dataset with two levels – “weekday”
# and “weekend” indicating whether a given date is a weekday or weekend day.
sapply(DT1,class)

DT1$date <- as.Date( DT1$date, format = "%Y-%m-%d" )
DT1[, week := weekdays(DT1$date) ] 

# convert the weekdays to just weekend and weekday, my weekdays function is printed out in Chinese
# FYI : 星期六 = Saturday ; 星期日 = Sunday
DT1$week <- ifelse( DT1$week %in% c( "星期六", "星期日" ), "weekend", "weekday" )
DT1$week <- sapply( DT1$week, as.factor )


# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis).
weeksteps <- DT1[ , list( steps = mean( steps, na.rm = TRUE ) ), by = list( interval, week )  ]

ggplot( weeksteps, aes( x = interval, y = steps, color = week ) ) + geom_line( size = 1.5 ) +
  facet_wrap( ~ week , ncol = 1 ) + theme(
    strip.text   = element_text( face = "bold", size = rel(1.5) ),
    axis.title   = element_text( size = 18 ),
    axis.text    = element_text( size = 12 ),
    legend.title = element_text( size = 16 ),
    legend.text  = element_text( size = 14 ))


