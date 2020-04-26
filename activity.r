#read file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "step_data.zip"
download.file(url, destfile)
unzip(destfile)
activity <- read.csv("activity.csv", sep = ",")

#add a column of day that the first day is 2012-10-01
activity$date<-as.Date(as.character(activity$date))
activity$day<-activity$date-activity$date[1]+1

#Have a look at the data
#str(activity)
#head(activity)

#Total number of steps taken for each day
totalStep_day<-tapply(activity$steps,activity$day,sum,na.rm=TRUE)
#Barplot total number of steps taken for each day
barplot(totalStep_day,main='Total steps taken per day',
     xlab = 'Day',ylab='Total steps')
#add lines of mean and median in red and blue respectively
abline(h = mean(totalStep_day), lty = 1, lwd = 1, col = "red")
abline(h = median(totalStep_day), lty = 1, lwd = 1, col = "blue")
legend('top',c('Mean','Median'),col=c('red','blue'),lty = c(1,1),lwd=c(1,1))

#Histogram of total number of steps taken for each day
hist(totalStep_day,main='Total steps taken per day',
        xlab = 'Total steps per day',ylab='Frequency')
#add lines of mean and median in red and blue respectively
abline(v = mean(totalStep_day), lty = 1, lwd = 1, col = "red")
abline(v = median(totalStep_day), lty = 1, lwd = 1, col = "blue")
legend('topright',c('Mean','Median'),col=c('red','blue'),lty = c(1,1),lwd=c(1,1))

#Average number of steps taken for each interval
avgStep_interval<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
#Plot the average number of steps with intervals
plot(avgStep_interval,ty='l',main='Average steps vs. daily time interval',
     xlab = 'Daily time interval',ylab = 'Average steps')
#maximum average number of steps
x_max=as.numeric(which(avgStep_interval==max(avgStep_interval)))
y_max=max(avgStep_interval)
p<-c(round(x_max),round(y_max))
points(t(p),pch=8,col='red',cex=2)

#Calculate and report the total number of missing values in the dataset
na_number_steps<-sum(is.na(activity$steps))
#na_number_steps

#split the activity date with days
activity_day<-split(activity,activity$day)
avgStep_day<-as.numeric(tapply(activity$steps,activity$day,mean,na.rm=TRUE))

#strategy to filling in missing values
#the missing value is equal to the average steps with interval
activity_NA<-data.frame()
for(i in 1:length(avgStep_day)){
        if (sum(!is.na(activity_day[[i]][,1]))==0){
                activity_day[[i]]$steps<-avgStep_interval
        }else if(sum(is.na(activity_day[[i]]$steps))!=0){
                for (j in 1:length(activity_day[[i]]$steps)){
                        activity_day[[i]][j,1]<-avgStep_interval[j]
                }
        }
        activity_NA<-rbind(activity_NA,activity_day[[i]])
}

#Barplot total number of steps taken for each day without NAs
totalStep_NA<-tapply(activity_NA$steps,activity_NA$day,sum,na.rm=TRUE)
barplot(totalStep_NA,main='Total steps taken per day without NA',
        xlab = 'Day',ylab='Total steps')
abline(h = mean(totalStep_NA), lty = 1, lwd = 1, col = "red")
abline(h = median(totalStep_NA), lty = 2, lwd = 1, col = "blue")
legend('top',c('Mean','Median'),col=c('red','blue'),lty = c(1,2),lwd=c(1,1))

#Histogram of total number of steps taken for each day without NAs
hist(totalStep_NA,main='Total steps taken per day without NA',
        xlab = 'Steps per day',ylab='Frequency')
abline(v = mean(totalStep_NA), lty = 1, lwd = 1, col = "red")
abline(v = median(totalStep_NA), lty = 2, lwd = 1, col = "blue")
legend('top',c('Mean','Median'),col=c('red','blue'),lty = c(1,2),lwd=c(1,1))

#position weekdays and weekends
activity$week<-weekdays(activity$date)
activity$week<-factor(activity$week,
                      levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                      ordered = TRUE)
activity$weekday[activity$week%in%c('Monday','Tuesday','Wednesday','Thursday','Friday')]<-'weekday'
activity$weekday[activity$week%in%c('Saturday','Sunday')]<-'weekend'

#average number of steps for weekdays and weekends
activity$weekday<-factor(activity$weekday,levels = c('weekday','weekend'))
avgStep_wd<-tapply(activity[activity$weekday=='weekday','steps'],
                         activity[activity$weekday=='weekday','interval'],mean,na.rm=TRUE)
avgStep_we<-tapply(activity[activity$weekday=='weekend','steps'],
                   activity[activity$weekday=='weekend','interval'],mean,na.rm=TRUE)

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
par(mfrow=c(1,2))
plot(avgStep_wd,ty='l',main='weekday',xlab = 'Daily time interval',ylab = 'Average steps')
plot(avgStep_we,ty='l',main='weekend',xlab = 'Daily time interval',ylab = 'Average steps')
par(mfrow=c(1,1))