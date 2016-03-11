library(plyr)           # NEEDED TO DO SOME COOL DATA MANIPULATIONS
library(ggplot2)        # LETS ME PLOT THE TICKETS
library(ggmap)          # LETS ME GRAB A MAP OF PHILLY TO PLOT THE TICKETS AGAINST
library(randomForest)   # RANDOM FORESTS = PREDICTIONS... YES!

ptix <- read.csv("Parking_Violations.csv")      ## DATA FILE FROM OPENDATAPHILLY

## QUESTIONS TO ANSWER
# How many tickets are there?
numtix <- dim(ptix)[1]

# What is the range of dates in the data frame?
## CONVERT TO A DATETIME OBJECT
ptix$Issue.Date.and.Time <- as.POSIXct(strptime(as.character(ptix$Issue.Date.and.Time), 
                                                format = "%m/%d/%Y %I:%M:%S %p"))

earliest <- min(ptix$Issue.Date.and.Time)       ## EARLIEST = MINIMUM DATETIME
latest <- max(ptix$Issue.Date.and.Time)         ## LATEST = MAXIMUM DATETIME
range <- latest - earliest                      ## RANGE IS THE DIFFERENCE

# Are there missing days/data?
## CONVERT THE DATETIME TO BE A DATE ONLY
days <- as.data.frame(as.Date(ptix$Issue.Date.and.Time, format = "%m/%d/%Y"))
names(days) <- "date"                   ## NAME THE COLUMN TO SOMETHING NICE
## COUNT THE DATA BY DATE - HOW MANY TICKETS DO WE HAVE EVERY DAY
count_by_day <- ddply(days, .(date), summarize, count = length(date))

# What was the biggest/smallest fine? What were those fines for? Who issued those fines?
ptix$Fine <- gsub("\\$", "", ptix$Fine)         ## REMOVE THE '$'
ptix$Fine <- as.numeric(ptix$Fine)              ## CHANGE IT TO A NUMERIC

## GRAB THE IMPORTANT COLUMNS FOR WHEN THE FINE IS THE MAXIMUM
maxfine_1 <- ptix[ptix$Fine == max(ptix$Fine), c(1, 5, 7, 8, 9, 10)]
## GRAB THE IMPORTANT COLUMNS FOR WHEN THE FINE IS THE MINIMUM
minfine_1 <- ptix[ptix$Fine == min(ptix$Fine), c(1, 5, 7, 8, 9, 10)]
ptix$Fine[ptix$Fine == min(ptix$Fine)] <- 2000  ## CHANGING THE $1 FINES TO $2000
## GRAB THE IMPORTANT COLUMNS FOR WHEN THE FINE IS THE MINIMUM NOW
minfine_1 <- ptix[ptix$Fine == min(ptix$Fine), c(1, 5, 7, 8, 9, 10)]

# What was the average fine?
summary(ptix$Fine)

# What day had the most fines? Least fines?
## DAY WITH THE MOST FINES ISSUED
maxday <- count_by_day[count_by_day$count == max(count_by_day$count), ]
## DAY WITH THE LEAST FINES ISSUED
minday <- count_by_day[count_by_day$count == min(count_by_day$count), ]
avday <- mean(count_by_day$count)       ## CALCULATE THE MEAN

# How much $ in fines did they write each day? Average? 
ptix <- cbind(ptix, days)       ## ADD THE FORMATTED DAYS FIELD TO THE DATA FRAME
## SUMMARIZE THE SUM OF FINES PER DAY
dollars_per_day <- ddply(ptix, .(date), summarize, dols = sum(Fine))
## DAY WITH THE MOST DOLLARS OF FINES ISSUED
maxdols <- dollars_per_day[dollars_per_day$dols == max(dollars_per_day$dols), ]
## DAY WITH THE LEAST DOLLARS OF FINES ISSUED
mindols <- dollars_per_day[dollars_per_day$dols == min(dollars_per_day$dols), ]
## THE AVERAGE AMOUNT IN FINES ISSUED IN A DAY
avgdols <- mean(dollars_per_day$dols)

# What hour of the day had the most fines?
hours <- as.data.frame(strftime(ptix$Issue.Date.and.Time, format = "%H"))
names(hours) <- "hour"
count_by_hour <- ddply(hours, .(hour), summarize, count = length(hour))
maxhour <- count_by_hour$hour[count_by_hour$count == max(count_by_hour$count)]
minhour <- count_by_hour$hour[count_by_hour$count == min(count_by_hour$count)]
barplot(height = count_by_hour$count, names.arg = count_by_hour$hour)

# What day of the week had the most fines?
wday <- as.data.frame(strftime(ptix$Issue.Date.and.Time, format = "%A"))
names(wday) <- "weekday"
count_by_wday <- ddply(wday, .(weekday), summarize, count = length(weekday))
maxwday <- count_by_wday$weekday[count_by_wday$count == max(count_by_wday$count)]
minwday <- count_by_wday$weekday[count_by_wday$count == min(count_by_wday$count)]
barplot(height = count_by_wday$count, names.arg = count_by_wday$weekday)

# What day of the month had the most fines?
mday <- as.data.frame(strftime(ptix$Issue.Date.and.Time, format = "%d"))
names(mday) <- "day_of_month"
count_by_mday <- ddply(mday, .(day_of_month), summarize, count = length(day_of_month))
maxmday <- count_by_mday$day_of_month[count_by_mday$count == max(count_by_mday$count)]
minmday <- count_by_mday$day_of_month[count_by_mday$count == min(count_by_mday$count)]
barplot(height = count_by_mday$count, names.arg = count_by_mday$day_of_month)

month <- as.data.frame(months(ptix$Issue.Date.and.Time))
names(month) <- "month"
count_by_month <- ddply(month, .(month), summarize, count = length(month))

# What state has the most fines?
count_by_state <- ddply(ptix, .(State), summarize, count = length(State))
maxstate <- count_by_state[count_by_state$count == max(count_by_state$count), ]
minstate <- count_by_state[count_by_state$count == min(count_by_state$count), ]

# What is the furthest someone came to get a fine?
# Who has the most fines?
ID <- paste(ptix$State, ptix$Plate.ID, sep = "")
fines <- as.data.frame(cbind(ptix$Fine, ID))
names(fines) <- c("fine", "ID")

count_by_plate <- ddply(fines, .(ID), summarize, count = length(ID), 
                        totalFine = sum(as.numeric(fine)))

maxcount <- count_by_plate[count_by_plate$count == max(count_by_plate$count), ]

# Who paid the most in fines?
maxfine <- count_by_plate[count_by_plate$totalFine == max(count_by_plate$totalFine), ]

head(ptix[ptix$Plate.ID == 1612270 & ptix$State == "PA",])
tail(ptix[ptix$Plate.ID == 1612270 & ptix$State == "PA",])

# How many people have been issued fines?
numpeeps <- dim(count_by_plate)[1]

# What Agency Issued the Most Tickets?
count_by_agency <- ddply(ptix, .(Issuing.Agency), summarize, 
                         count = length(Issuing.Agency))
count_by_agency <- count_by_agency[order(count_by_agency$count, decreasing = TRUE), ]

# Where were the most fines?
## CREATE THE DATA SET TO FEED INTO TABLEAU FOR VISUALIZATIONS
count_by_coord <- ddply(ptix[ptix$Coordinates != "", ], 
                        .(Coordinates, Location.Standardized), 
                        summarize, count = length(Coordinates))
latlon <- count_by_coord$Coordinates
latlon <- gsub("\\(", "", latlon)
latlon <- gsub("\\)", "", latlon)
latlon <- strsplit(latlon, ",")
latlon <- ldply(latlon, rbind)
names(latlon) <- c("lat", "lon")
latlon$lat <- as.numeric(as.character(latlon$lat))
latlon$lon <- as.numeric(as.character(latlon$lon))
count_by_coord <- cbind(count_by_coord, latlon)

write.csv(count_by_coord, "gis_data.csv", row.names = FALSE)
