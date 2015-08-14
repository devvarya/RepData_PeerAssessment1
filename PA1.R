act <- read.csv("activity.csv",header=TRUE,colClasses = c("numeric","character","numeric"))
head(act)
act$date <- as.Date(act$date, "%Y-%m-%d")

spd <- aggregate(steps ~ date,sum,data=act,na.rm=TRUE)
## The elements(dates) are coerced to factors before use.
head(spd)

##Another method
##spd <- tapply(act$steps,act$date,sum,na.rm=TRUE)

hist(spd$steps, col="blue",breaks=5, main="Total Steps Taken Each Day", xlab="Steps")

mean(spd$steps)
median(spd$steps)

sbi <- aggregate(steps ~ interval, mean ,data=act,na.rm=TRUE)
head(sbi)

##plot(sbi$interval,sbi$steps,type="l",xlab="Interval",ylab="Mean Steps",
##main="Time Series plot of 5-min Interval and Steps")

library(ggplot2)
ggplot(sbi, aes(interval, steps)) +
geom_line() +
xlab("Interval") +
ylab("Mean Steps")

max <- sbi[which.max(sbi$steps),]
max

na <- sum(is.na(act))
na

pos <- which(is.na(act$steps))

actnew <- act

for(i in 1:length(sbi$interval))
  {
  key <- sbi[i,1]
  value <- sbi[i,2]
  for(j in 1:length(pos))
  {
    index <- pos[j]
    if(actnew[index,3]==key)
  {
    actnew[index,1] <- value 
  }
  }
  }

head(actnew)

spdnew <- aggregate(steps ~ date,sum,data=actnew,na.rm=TRUE)
hist(spdnew$steps, col="blue",breaks=5, main="Total Steps Taken Each Day", xlab="Steps")

mean(spdnew$steps)
median(spdnew$steps)

day <- weekdays(actnew$date, abbreviate = FALSE)

daytype <- vector()

for (i in 1:nrow(actnew)) 
  {
  if (day[i] == "Saturday" | day[i] == "Sunday") 
    {
    daytype[i] <- "Weekend"
    }
  else 
    {
    daytype[i] <- "Weekday"
    }
  }

actnew$daytype <- daytype
actnew$daytype <- factor(actnew$daytype)

sbinew <- aggregate(steps ~ interval + daytype, mean, data=actnew, na.rm=TRUE)
head(sbinew)

library(lattice)
xyplot(steps ~ interval|daytype, sbinew, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")

library(ggplot2)
g <- ggplot(sbinew, aes(interval, steps)) 
g + geom_line(color="violet") + 
  facet_wrap(~ daytype, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()
