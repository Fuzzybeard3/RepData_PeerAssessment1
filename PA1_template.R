#Loading and preprocessing the data
library(ggplot2)
library(plyr)
activity <- read.csv("activity.csv")
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
clean <- activity[!is.na(activity$steps),]
#What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")
#Make a histogram of the total number of steps taken each day
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
#Calculate and report the mean and median of the total number of steps taken per day
as.integer(mean(sumTable$Steps))
as.integer(mean(sumTable$Steps))

#What is the average daily activity pattern?
#Make a time series plot
library(plyr)
library(ggplot2)
clean <- activity[!is.na(activity$steps),]
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
#Which 5-minute interval
maxSteps <- max(intervalTable$Avg)
intervalTable[intervalTable$Avg==maxSteps,1]

#Imputing missing values
#Calculate and report the total number of missing values in the dataset
nrow(activity[is.na(activity$steps),])
#Devise a strategy for filling in all of the missing values in the dataset.
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
nadata<- activity[is.na(activity$steps),]
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
mergeData <- rbind(clean, newdata2)
#Make a histogram
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")
as.integer(mean(sumTable2$Steps))
as.integer(median(sumTable2$Steps))
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )

#Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
#Make a panel plot containing a time series plot
library(lattice) 
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")