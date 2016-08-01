library(lubridate)
library(zipcode)
library(dplyr)
library(plotly)
library(geosphere)
library(circlize)
library(plotly)
options(max.print=999999999)
trip <- read.csv(file="/Users/rahulramanujam/Desktop/201408_trip_data.csv",head=TRUE)
names(trip) <- c('id', 'duration', 's.date', 's.station',
                 's.terminal', 'e.date', 'e.station', 'e.terminal',
                 'bike', 'type', 'zip')
names(trip)
prepareTrip <- function(trip) {
  trip$duration.m <- trip$duration / 60
  trip$s.date <- mdy_hm(trip$s.date, tz='US/Pacific')
  trip$e.date <- mdy_hm(trip$e.date, tz='US/Pacific')
  trip$s.month <- as.factor(month(trip$s.date, label=T, abbr=T))
  trip$s.wday <- as.factor(wday(trip$s.date, label=T, abbr=T))
  trip$s.day <- day(trip$s.date)
  trip$s.hour <- as.factor(hour(trip$s.date))
  trip$e.month <- as.factor(month(trip$e.date, label=T, abbr=T))
  trip$e.wday <- as.factor(wday(trip$e.date, label=T, abbr=T))
  trip$e.day <- day(trip$e.date)
  trip$e.hour <- as.factor(hour(trip$e.date))
  data(zipcode)
  trip$zip[trip$type == 'Customer'] <- NA   # zipcodes of Customer are invalid
  trip$zip <- clean.zipcodes(trip$zip)      # clean up
  trip$zip[!(trip$zip %in% zipcode)] <- NA  # remove zipcodes which don't exist
  trip$zip <- as.factor(trip$zip)
  return(trip)
}
trip <- prepareTrip(trip)

# popular day of the week - split by consumers and subscribers
table(trip$s.wday,trip$type)
wday <- round(prop.table(table(trip$s.wday,trip$type)),4)*100
wday

# Popular time of day
table(trip$s.hour,trip$type)
round(prop.table(table(trip$s.hour,trip$type)),4)*100

# Duration greater than 30 minutes - incurs additional fee
table(trip$duration.m > 30)
round(prop.table(table(trip$duration.m > 30)),4)*100
table(trip$duration.m > 30, trip$type)
round(prop.table(table(trip$duration.m > 30, trip$type), margin=2),4)*100

# Penalty for subscribers
penalty.subs <- trip[trip$type == 'Subscriber' & trip$duration.m > 30, ]
summary(penalty.subs$duration.m)
qplot(penalty.subs$duration.m, geom='histogram', binwidth=2, xlim=c(30, 100),
      main='Subscribers\' Rides more than 30 minutes', xlab='Trip Time (mins)', ylab='Frequency', fill = I("purple"))
table(cut(penalty.subs$duration.m, breaks=seq(30, 210, 15)), useNA='always')
round(prop.table(table(cut(penalty.subs$duration.m, breaks=seq(30, 210, 15)), useNA='always')), 3)

# Penalty for customers
penalty.cust <- trip[trip$type == 'Customer' & trip$duration.m > 30, ]
summary(penalty.cust$duration.m)
qplot(penalty.cust$duration.m, geom='histogram', binwidth=2, xlim=c(30, 100),
      main='Customers\' Rides more than 30 minutes', xlab='Trip Time (mins)', ylab='Frequency', fill = I("purple"))
table(cut(penalty.cust$duration.m, breaks=seq(30, 210, 15)), useNA='always')
round(prop.table(table(cut(penalty.cust$duration.m, breaks=seq(30, 210, 15)), useNA='always')), 3)

# Routes with 30-60 minutes
routes_30 <- table(trip$s.station,trip$e.station,trip$duration.m > 30)
View(routes_30)
