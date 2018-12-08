'install.packages("dplyr")'
'install.packages("lubridate")'
'install.packages("data.table")'
'install.packages("tibble")'
'install.packages("ggplot2")'

library("dplyr")
library("lubridate")
library("data.table")
library("tibble")
library("ggplot2")
library(lattice)

#1a reading the data file using fread and tbl_df, also cleaning up the date column simultaneously using mutate#
rawdata<- tbl_df(fread("activity.csv", na.strings = "NA") ) %>%  mutate(date = ymd(date))

#1b filtering out NA values#
data <- filter(rawdata, steps != 'NA')

#2a aggregating data to get total steps each day#
aggregated_totalsteps <- aggregate(x =data[c("steps","interval")], by = list(data$date),FUN = sum)


#2b creating histogram of total steps per day using qplot#
qplot(aggregated_totalsteps$steps, geom="histogram",binwidth= 5000, main = "Histogram for Steps", xlab = "Total Steps Per Day")

#3a mean steps#
Meansteps <- mean(aggregated_totalsteps$steps)

#3b median steps#
Mediansteps <- median(aggregated_totalsteps$steps)

#4a aggregrating data by interval for time series plot#
Byinterval <- aggregate(data$steps, by = list(data$interval), mean)
colnames(Byinterval)<-c("intervalID", "Mean steps")

#4b Time Series plot#

plot(Byinterval$intervalID,Byinterval$`Mean steps`,type="l", 
     
     xlab="5-Minute Interval",ylab="Mean Steps Taken",
     
     main="Steps during the Day")

'Interval with most activity'

Byinterval$intervalID[which.max(Byinterval$`Mean steps`)]

#5 Imputing Missing Values#

'Let us re-read the raw data'

data2<- tbl_df(fread("activity.csv", na.strings = "NA") ) %>%  mutate(date = ymd(date))

'How many missing values'
sum(!complete.cases(data2))

'Replacing missing values with mean from that
 specific interval (Ex: Missing value in interval
 5 replaced by mean step value in interval 5'

summary <- data2[complete.cases(data2), ] %>% group_by(interval) %>% summarise(mean=mean(steps));
incomplete <- data2[!complete.cases(data2),];
updated <- merge(x=incomplete, y=summary, by="interval", all.x=T);
updated$steps <- updated$mean;
imputed <- rbind(data2[complete.cases(data2), ], select(updated, steps, date, interval))

'no more NA values'
sum(!complete.cases(imputed))

#6a aggregating data2 to get total steps each day#
aggregated_totalsteps2 <- aggregate(x =imputed[c("steps","interval")], by = list(imputed$date),FUN = sum)


#6b creating histogram of total steps per day using qplot#
qplot(aggregated_totalsteps2$steps, geom="histogram",binwidth= 5000, main = "Histogram for Steps", xlab = "Total Steps Per Day")

#6c mean steps#
Meansteps2 <- mean(aggregated_totalsteps2$steps)

#6d median steps#
Mediansteps2 <- median(aggregated_totalsteps2$steps)

#7 Classifying Weekday/Weekend

imputed$day<-ifelse(as.POSIXlt(as.Date(imputed$date))$wday%%6==0,
                         "weekend","weekday")
 
imputed$day<-factor(imputed$day,levels=c("weekday","weekend"))


WeekdayWeekEndData <- aggregate(steps~interval+day,imputed,mean)
xyplot(steps~interval|factor(day), data=WeekdayWeekEndData,aspect=1/2, type="l")
