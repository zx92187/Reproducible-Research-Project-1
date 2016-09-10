---
output: word_document
---
#working directory already been set

activity <- read.csv("activity.csv",quote="\"")

act <- read.csv("activity.csv",
                colClasses = c("numeric", "character","integer"))
summary(act)




# For this part of the assignment, you can ignore the missing values in the dataset.
 
# Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day


library(lubridate)
library(plyr)


# Attaching package: 'plyr'
# The following object is masked from 'package:lubridate':
#     here

library(dplyr)

library(ggplot2)
library(lattice)
library(data.table)

library(knitr)
library(rmarkdown)
library("markdown")



actAgg <- tapply(act$steps, act$date, FUN = sum, na.rm = TRUE)
print(actAgg)

library(lubridate)
act$date <- ymd(act$date)
summary(actAgg)

library(data.table)
actAgg_dt=data.table(actAgg)
summary(actAgg_dt)
mean(actAgg_dt$V1)
median(actAgg_dt$V1)

library(plyr)
library(dplyr)
steps1 <- act %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
  
  hist(steps1$steps, main="Histogram of Steps" ,
     xlab="Steps", ylab="Count") 
abline(v = mean(steps1$steps), col = "red", lwd = 2)

#What is the average daily activity pattern?

intervl <- act %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) %>%
  print 
  

# Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
# 'l' means line  
plot(intervl, type = 'l', col = "blue")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervl[which.max(intervl$steps), ]$interval


#Imputing missing values

#There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce #bias into some calculations or summaries of the data.

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

#values missing in orIginal dataset
missing1 <- sum(is.na(act))
missing1


#Create a new dataset with the missing data filled in:

act_miss_rep  <- transform(act, steps = ifelse(is.na(steps), round(mean(steps, na.rm=TRUE)), steps))


#missing values replaced verification

missing2 <- sum(is.na(act_miss_rep))
missing2


#Summary of new dataset with missing values replaced:

#values missing in orginal dataset
summary(act_miss_rep)


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps #taken per day.

steps2 <- act_miss_rep %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print 
  
  
hist(steps2$steps, main="Histogram of Steps" , sub="Missing Values replaced with mean of interval steps" ,
     xlab="Steps", ylab="Count") 
abline(v = mean(steps2$steps), col = "red", lwd = 2)


#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on #the estimates of the total daily number of steps?

#aggregate corrected data by date to calculate daily stats 

actAgg2 <- tapply(act_miss_rep$steps, act_miss_rep$date, FUN = sum, na.rm = TRUE)
act$date <- ymd(act_miss_rep$date)

#mean and median total number of steps taken per day
mean(actAgg2)

median(actAgg2)


diff_in_means=mean(actAgg)-mean(actAgg2)
diff_in_means


summary(actAgg)


summary(actAgg2)

#What is the impact of imputing missing data on the estimates 
#of the total daily number of steps?

summary(actAgg2)-summary(actAgg)


#Are there differences in activity patterns between weekdays and weekends?

#A new factor variable (d_type) in a third dataset (act_weekday_ind) has been created. It has two levels - “weekday” and #“weekend”– indicating whether a given date is a weekday or weekend day.

act_weekday_ind=act_miss_rep
act_weekday_ind$d_type[as.POSIXlt(act_weekday_ind$date)$wday %in% c(0,6)] <- "weekday"
act_weekday_ind$d_type[as.POSIXlt(act_weekday_ind$date)$wday %in% c(1:5)] <- "weekend"
table(act_weekday_ind$d_type,as.POSIXlt(act_weekday_ind$date)$wday)


#Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or #weekend days (y-axis).

actAgg3 <- act_weekday_ind  %>% 
  group_by(d_type, interval) %>%
  summarize(steps3 = mean(steps))
  
  
  library(ggplot2)
ggplot(actAgg3, aes(interval,steps3))+geom_line(color="red")+
    facet_wrap(~d_type, ncol=1)
    
    

