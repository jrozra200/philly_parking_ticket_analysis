setwd("~/Google Drive/Grad School/Programming Practice/Philly Parking Tickets")

library(plyr)
library(randomForest)

# Can I predict the amount of parking tickets by the weather?
days <- as.data.frame(as.Date(ptix$Issue.Date.and.Time, format = "%m/%d/%Y"))
names(days) <- "DATE"
count_by_day <- ddply(days, .(DATE), summarize, count = length(DATE))

hist(count_by_day$count)

train <- count_by_day[count_by_day$DATE < "2014-08-01", ]
test <- count_by_day[count_by_day$DATE >= "2014-08-01", ]

weather_data <- read.csv("weather_data.csv")
weather_data$DATE <- as.Date(as.POSIXct(strptime(as.character(weather_data$DATE), 
                                                 format = "%Y%m%d")), 
                             format = "%m/%d/%Y")

train <- join(train, weather_data, by = "DATE")
test <- join(test, weather_data, by = "DATE")
holidays <- as.Date(c("2012-01-02", "2012-01-16", "2012-02-20", "2012-05-28",
                      "2012-07-04", "2012-09-03", "2012-10-08", "2012-11-12",
                      "2012-11-22", "2012-12-25", "2013-01-01", "2013-01-21", 
                      "2013-02-18", "2013-05-27", "2013-07-04", "2013-09-02", 
                      "2013-10-14", "2013-11-11", "2013-11-28", "2013-12-25",
                      "2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26",
                      "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11",
                      "2014-11-27", "2014-12-25", "2015-01-01", "2015-01-09",
                      "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07"))

train$STATION <- NULL
train$STATION_NAME <- NULL
train$MDPR[train$MDPR < 0] <- 0
train$DAPR[train$DAPR < 0] <- 0
train$PRCP[train$PRCP < 0] <- 0
train$SNWD[train$SNWD < 0] <- 0
train <- train[train$TMAX > 0, ]
train <- train[train$TMIN > 0, ]
train <- train[!is.na(train$TMAX), ]
train$SNOW[train$SNOW < 0] <- 0
train$TOBS <- NULL
train$WT01[train$WT01 < 0] <- 0
train$WT03[train$WT03 < 0] <- 0
train$WT04[train$WT04 < 0] <- 0
train$WT01 <- NULL
train$WT04 <- NULL
train$WT03 <- NULL
train$TMAX <- train$TMAX / 10
train$TMIN <- train$TMIN / 10
train$DOW <- as.factor(weekdays(train$DATE))
train$HOL <- 0
train$HOL[as.character(train$DATE) %in% as.character(holidays)] <- 1
train$HOL <- as.factor(train$HOL)

summary(train)

forest <- randomForest(count ~ SNOW + TMIN + DOW + HOL, data = train, 
                       importance = TRUE, ntree = 10000)

plot(forest)
varImpPlot(forest)
forest$importance

plot(x = train$TMIN, y = train$count)

linmod <- lm(count ~ DOW + HOL + TMIN + SNOW, data = train)

test$STATION <- NULL
test$STATION_NAME <- NULL
test$MDPR[test$MDPR < 0] <- 0
test$DAPR[test$DAPR < 0] <- 0
test$PRCP[test$PRCP < 0] <- 0
test$SNWD[test$SNWD < 0] <- 0
test <- test[test$TMAX > 0, ]
test <- test[test$TMIN > 0, ]
test <- test[!is.na(test$TMAX), ]
test$SNOW[test$SNOW < 0] <- 0
test$TOBS <- NULL
test$WT01[test$WT01 < 0] <- 0
test$WT03[test$WT03 < 0] <- 0
test$WT04[test$WT04 < 0] <- 0
test$WT01 <- NULL
test$WT04 <- NULL
test$WT03 <- NULL
test$TMAX <- test$TMAX / 10
test$TMIN <- test$TMIN / 10
test$DOW <- as.factor(weekdays(test$DATE))
test$HOL <- 0
test$HOL[as.character(test$DATE) %in% as.character(holidays)] <- 1
test$HOL <- as.factor(test$HOL)

test$RF <- round(predict(forest, test), 0)
test$LM <- round(predict.lm(linmod, test), 0)

# Can I do some k-means clustering?