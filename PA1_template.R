# 1. Loading and preprocessing the data
# Show any code that is needed to
# - Load the data (i.e. read.csv())
# - Process/transform the data (if necessary) into a format suitable for your analysis

data <- read.csv("C:/Users/Pep Llontop/RepData_PeerAssessment1/activity.csv")

#2. What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# - Make a histogram of the total number of steps taken each day
# - Calculate and report the mean and median total number of steps taken per day

Totaldailysteps <- with(data, tapply(data$steps, data$date, sum, na.rm=TRUE))
hist(Totaldailysteps,col = "light blue", main="Histogram of Total Steps Per Day",xlab = "Total Steps Per Day")
mean(Totaldailysteps, na.rm=TRUE) #9354.23
median(Totaldailysteps, na.rm=TRUE) #10395

#3. What is the average daily activity pattern?
# - Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)
# - Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
avgdailysteps <- with(data, tapply(data$steps,data$interval,mean,na.rm=TRUE))
plot(unique(data$interval),avgdailysteps,type="l", main = "Average Number of Steps Per Time Interval",xlab = "5-Minute Time Intervals", ylab = "Average Number of Steps")
maxminuteint <- which.max(avgdailysteps) 

#4. Imputing missing values
# Note that there are a number of days/intervals where there are missing values
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.
# - Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
# - Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the 
# mean/median for that day, or the mean for that 5-minute interval, etc.
# - Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
# - Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily
# number of steps?
findNAs <- sum(is.na(data$steps)) #2034L

#devise the filling strategy using the avg steps for that time
splitbyint <- split(data, data$interval)
avgsteps <- rep(avgdailysteps,each=61)
avgstep_data <- cbind(data,avgsteps)

for(i in 1:length(data$steps)){
        if (is.na(data$steps[i])==TRUE){
                avgstep_data$steps[i]<-avgstep_data$avgsteps[i]
        }
}
#now remove the last column
finaldata <-avgstep_data[,c("steps","date","interval")]
newTotaldailysteps <- with(finaldata, tapply(finaldata$steps, finaldata$date, sum, na.rm=TRUE))
hist(newTotaldailysteps, main = "Total Number of Steps, Missing Values Added",xlab = "Total Number of Steps")
mean(newTotaldailysteps, na.rm=TRUE) #10889.8
median(newTotaldailysteps, na.rm=TRUE) #11015

#4. Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.
# - Create a new factor variable in the dataset with two levels - "weekday" and 
# "weekend" indicating whether a given date is a weekday or weekend day.
# - Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). 
weekday <-weekdays(as.Date(data$date,format = "%Y-%m-%d"),abbreviate = TRUE)
weekfactor <- NULL
for(j in 1:length(weekday)){
        if(weekday[j]=="Sat" | weekday[j]=="Sun"){
                weekfactor <- rbind(weekfactor,"weekend")
        }
        else{
                weekfactor<-rbind(weekfactor,"weekday")
        }
}
weekfactor <- as.factor(weekfactor)
finaldata <- cbind(finaldata[,c("steps","date","interval")],weekfactor)

#split into weekday and weekend
weekdaydata <- finaldata[weekfactor=="weekday",]
avgweekday <- with(weekdaydata, tapply(weekdaydata$steps,weekdaydata$interval,mean,na.rm=TRUE))

weekenddata <- finaldata[weekfactor=="weekend",]
avgweekend <- with(weekenddata, tapply(weekenddata$steps,weekenddata$interval,mean,na.rm=TRUE))

par(mar = c(0,0,0,0),mfrow=c(2,1),oma=c(3.5,4,2,2))
plot(unique(data$interval),avgweekday,type="l",col="blue",xaxt='n')
plot(unique(data$interval),avgweekend,type="l",col = "dark green")
mtext("Average Number of Steps",side=2,outer=TRUE,line=2)
mtext("Time Interval",side=1, outer=TRUE, line = 2)
mtext("Comparing Averages for Weekday and Weekend", side = 3, outer=TRUE, line = .5)
mtext("Weekdays",side=3,outer=FALSE,line = 5.5,adj=1,cex=.8,col = "dark blue")
mtext("Weekends",side=1,outer=FALSE,line=-.5,adj=1,cex=.8,col="dark green")