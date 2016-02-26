library(plyr)
library(ggmap)
library(ggplot2)
library(scales)

setwd("~/Google Drive/Grad School/Programming Practice/Philly Parking Tickets")

ptix <- read.csv("Parking_Violations.csv")

## QUESTIONS TO ANSWER
# What is the range of dates in the data frame?
ptix$Issue.Date.and.Time <- as.POSIXct(strptime(as.character(ptix$Issue.Date.and.Time), 
                                     format = "%m/%d/%Y %I:%M:%S %p"))

earliest <- min(ptix$Issue.Date.and.Time)
latest <- max(ptix$Issue.Date.and.Time)
range <- latest - earliest

# Are there missing days/data?
days <- as.data.frame(as.Date(ptix$Issue.Date.and.Time, format = "%m/%d/%Y"))
names(days) <- "date"
count_by_day <- ddply(days, .(date), summarize, count = length(date))
dim(count_by_day)       ## MISSING 19 days
counts <- table(days)
barplot(counts)

# How many tickets are there?
numtix <- dim(ptix)[1]

# What was the biggest/smallest fine? What were those fines for? Who issued those fines?
ptix$Fine <- gsub("\\$", "", ptix$Fine)
ptix$Fine <- as.numeric(ptix$Fine)

maxfine <- ptix[ptix$Fine == max(ptix$Fine), c(1, 5, 7, 8, 9, 10)]
minfine <- ptix[ptix$Fine == min(ptix$Fine), c(1, 5, 7, 8, 9, 10)]
ptix$Fine[ptix$Fine == min(ptix$Fine)] <- 2000  ## CHANGING THE $1 FINES TO $2000
## TO MATCH THE OTHER ATV TICKETS
minfine <- ptix[ptix$Fine == min(ptix$Fine), c(1, 5, 7, 8, 9, 10)]

# What was the average fine?
summary(ptix$Fine)

# What day had the most fines? Least fines?
maxday <- count_by_day[count_by_day$count == max(count_by_day$count), ]
minday <- count_by_day[count_by_day$count == min(count_by_day$count), ]
avday <- mean(count_by_day$count)

# How much $ in fines did they write each day? Average? 
ptix <- cbind(ptix, days)
dollars_per_day <- ddply(ptix, .(date), summarize, dols = sum(Fine))
maxdols <- dollars_per_day[dollars_per_day$dols == max(dollars_per_day$dols), ]
mindols <- dollars_per_day[dollars_per_day$dols == min(dollars_per_day$dols), ]
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

# How many people have been issued fines?
numpeeps <- dim(count_by_plate)[1]

# Where were the most fines?
count_by_coord <- ddply(ptix[ptix$Coordinates != "", ], .(Coordinates), 
                        summarize, count = length(Coordinates))

count_by_coord <- cbind(count_by_coord, perc = 0)
count_by_coord$bins <- count_by_coord$count / sum(count_by_coord$count)

count_by_coord$Coordinates <- gsub("\\(", "", count_by_coord$Coordinates)
count_by_coord$Coordinates <- gsub("\\)", "", count_by_coord$Coordinates)

latlon <- strsplit(count_by_coord$Coordinates[count_by_coord$Coordinates != ""], ",")

## 287145 tickets do not have location data - skipping them for now

latlon <- ldply(latlon, rbind)
names(latlon) <- c("lat", "lon")

count_by_coord <- cbind(count_by_coord, latlon)
count_by_coord$lat <- as.numeric(as.character(count_by_coord$lat))
count_by_coord$lon <- as.numeric(as.character(count_by_coord$lon))

philly1 <- get_map(location = "Philadelphia", maptype = "satellite", zoom = 10)

map1 <- ggmap(philly1, extent = "device") + geom_density2d(data = count_by_coord, 
                                                           aes(x = lon, y = lat)) + 
        stat_density2d(data = count_by_coord, aes(fill = ..level.., alpha = ..level..),
                       size = 0.01, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
        scale_alpha(range = c(0, 0.3), guide = FALSE)

philly2 <- get_map(location = "Philadelphia", maptype = "satellite", zoom = 12)

map2 <- ggmap(philly2, extent = "device") + geom_density2d(data = count_by_coord, 
                                      aes(x = lon, y = lat)) + 
        stat_density2d(data = count_by_coord, aes(fill = ..level.., alpha = ..level..),
                       size = 0.01, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
        scale_alpha(range = c(0, 0.3), guide = FALSE)

philly3 <- get_map(location = "Philadelphia", maptype = "satellite", zoom = 14)

map3 <- ggmap(philly3, extent = "device") + geom_density2d(data = count_by_coord, 
                                                           aes(x = lon, y = lat)) + 
        stat_density2d(data = count_by_coord, aes(fill = ..level.., alpha = ..level..),
                       size = 0.01, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
        scale_alpha(range = c(0, 0.3), guide = FALSE)

# Can I predict the amount of parking tickets by the weather?
hist(count_by_day$count)

# Can I do some k-means clustering?