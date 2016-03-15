##########################################################
##########################################################
## Can I predict the amount of parking tickets per day? ##
##########################################################
##########################################################

########################################
## LOAD PACKAGES AND READ IN RAW DATA ##
########################################

library(plyr)
library(randomForest)

ptix <- read.csv("Parking_Violations.csv")      ## DATA FILE FROM OPENDATAPHILLY

## READ IN THE WEATHER DATA (FROM NCDC)
weather_data <- read.csv("weather_data.csv")

## LIST OF ALL FEDERAL HOLIDAYS DURING THE RANGE OF THE DATA SET 
holidays <- as.Date(c("2012-01-02", "2012-01-16", "2012-02-20", "2012-05-28",
                      "2012-07-04", "2012-09-03", "2012-10-08", "2012-11-12",
                      "2012-11-22", "2012-12-25", "2013-01-01", "2013-01-21", 
                      "2013-02-18", "2013-05-27", "2013-07-04", "2013-09-02", 
                      "2013-10-14", "2013-11-11", "2013-11-28", "2013-12-25",
                      "2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26",
                      "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11",
                      "2014-11-27", "2014-12-25", "2015-01-01", "2015-01-09",
                      "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07"))


#####################
## FORMAT THE DATA ##
#####################

## SUMMARIZE THE DATA SET - COUNT OF TICKETS PER DAY
days <- as.data.frame(as.Date(ptix$Issue.Date.and.Time, format = "%m/%d/%Y"))
names(days) <- "DATE"
count_by_day <- ddply(days, .(DATE), summarize, count = length(DATE))

## CHANGE THE DATE FACTOR INTO A DATE VARIABLE
weather_data$DATE <- as.Date(as.POSIXct(strptime(as.character(weather_data$DATE), 
                                                 format = "%Y%m%d")), 
                             format = "%m/%d/%Y")

## JOIN THE WEATHER DATA TO THE TICKET DATA
count_by_day <- join(count_by_day, weather_data, by = "DATE")

## I DON'T CARE ABOUT THE STATION OR ITS NAME - GETTING RID OF IT
count_by_day$STATION <- NULL
count_by_day$STATION_NAME <- NULL

## A BUNCH OF VARIABLE ARE CODED WITH NEGATIVE VALUES IF THEY WEREN'T
## COLLECTED - CHANGING THEM TO 0s
count_by_day$MDPR[count_by_day$MDPR < 0] <- 0
count_by_day$DAPR[count_by_day$DAPR < 0] <- 0
count_by_day$PRCP[count_by_day$PRCP < 0] <- 0
count_by_day$SNWD[count_by_day$SNWD < 0] <- 0
count_by_day$SNOW[count_by_day$SNOW < 0] <- 0
count_by_day$WT01[count_by_day$WT01 < 0] <- 0
count_by_day$WT03[count_by_day$WT03 < 0] <- 0
count_by_day$WT04[count_by_day$WT04 < 0] <- 0

## REMOVING ANY ROWS WITH MISSING TEMP DATA
count_by_day <- count_by_day[count_by_day$TMAX > 0, ]
count_by_day <- count_by_day[count_by_day$TMIN > 0, ]

## GETTING RID OF SOME NA VALUES THAT POPPED UP
count_by_day <- count_by_day[!is.na(count_by_day$TMAX), ]


## REMOVING COLUMNS THAT HAVE LITTLE OR NO DATA IN THEM (ALL 0s)
count_by_day$TOBS <- NULL
count_by_day$WT01 <- NULL
count_by_day$WT04 <- NULL
count_by_day$WT03 <- NULL

## CHANGING THE DATA, UNNECESSARILY, FROM 10ths OF DEGREES CELCIUS TO 
## JUST DEGREES CELCIUS
count_by_day$TMAX <- count_by_day$TMAX / 10
count_by_day$TMIN <- count_by_day$TMIN / 10

## FEATURE CREATION - ADDING IN THE DAY OF WEEK
count_by_day$DOW <- as.factor(weekdays(count_by_day$DATE))

## FEATURE CREATION - ADDING IN IF THE DAY WAS A HOLIDAY
count_by_day$HOL <- 0
count_by_day$HOL[as.character(count_by_day$DATE) %in% 
                         as.character(holidays)] <- 1
count_by_day$HOL <- as.factor(count_by_day$HOL)

## FEATURE CREATION - ADDING IN THE MONTH
count_by_day$MON <- as.factor(months(count_by_day$DATE))

## SPLIT THE DATA SET INTO TRAIN AND TEST SEGMENTS
train <- count_by_day[count_by_day$DATE < "2014-08-01", ]
test <- count_by_day[count_by_day$DATE >= "2014-08-01", ]


######################
## EXPLORE THE DATA ##
######################

## IS THE DATA NORMALLY DISTRIBUTED?
hist(count_by_day$count, xlab = "Count of Tickets per Day", 
     main = "Histogram of Daily Violation Counts", 
     ylab = "Number of Days Issued the Amount")

## YOU CAN SEE THAT THE BOTTOM GROUP IS MADE UP MOSTLY OF SUNDAYS
plot(x = count_by_day$TMIN, y = count_by_day$count, 
     xlab = "Minimum Temperature (Celcius)", ylab = "Count of tickets", 
     main = "Minimum Temperature vs. Count of Tickets", 
     pch = ifelse(count_by_day$DOW == "Sunday", 1, 2))

############################
## FEATURE IDENTIFICATION ##
############################

## USING A RANDOM FOREST TO DETERMINE THE VARIABLE IMPORTANCE
featForest <- randomForest(count ~ MDPR + DAPR + PRCP + SNWD + SNOW + TMAX + 
                                   TMIN + DOW + HOL + MON, data = train, 
                           importance = TRUE, ntree = 10000)

## PLOT THE VARIABLE TO SEE THE IMPORTANCE
varImpPlot(featForest)

## SEE THE CORRELATION BETWEEN THE NUMERIC VARIABLES
cor(count_by_day[,c(3:9)])

######################
## BUILD THE MODELS ##
######################

## BUILD ANOTHER FOREST USING THE IMPORTANT VARIABLES
predForest <- randomForest(count ~ DOW + HOL + TMIN + MON, data = train, 
                           importance = TRUE, ntree = 10000)

## BUILD A LINEAR MODEL USING THE IMPORTANT VARIABLES
linmod_with_mon <- lm(count ~ TMIN + DOW + HOL + MON, data = train)

## THE SUMMARY AND ANOVA TABLE FOR THE MODEL WITH THE MONTH TERM
summary(linmod_with_mon)
anova(linmod_with_mon)

## BUILD A LINEAR MODEL WITHOUT THE MONTH TERM (BECAUSE MANY OF THE VALUES ARE 
## NOT SIGNIFICANT)
linmod_wo_mon <- lm(count ~ TMIN + DOW + HOL, data = train)

## THE SUMMARY AND ANOVA TABLE FOR THE MODEL WITHOUT THE MONTH TERM
summary(linmod_wo_mon)
anova(linmod_wo_mon)

###############################################################################
## IS THE MONTH TERM STATISTICALLY EQUIVALENT TO ZERO (& THUS UNNECCESSARY)? ##
###############################################################################

## Ho: B9 = B10 = B11 = B12 = B13 = B14 = B15 = B16 = B17 = B18 = B19 = 0
## Ha: At least one is not equal to 0

## F-Stat = MSdrop / MSE
f_stat <- ((241885490 - 221563026) / (759 - 748)) / 296207

## P_VALUE OF THE F_STAT CALCULATED ABOVE
p_value <- 1 - pf(f_stat, 11, 748)

## SINCE THE P-VALUE 6.8829e-10 IS MUCH LESS THAN 0.05, REJECT THE NULL 
## HYPOTHESIS AND CONCLUDE THE NULL, THAT AT LEAST ONE OF THE MONTH TERMS IS 
## NOT EQUAL TO ZERO. BECAUSE OF THIS, I'LL KEEP THE TERM IN THE MODEL.

###############################################################
## APPLY THE LINEAR AND RANDOM FOREST MODELS TO THE TEST SET ##
###############################################################

## PREDICT THE VALUES BASED ON THE MODELS
test$RF <- round(predict(predForest, test), 0)
test$LM <- round(predict.lm(linmod_with_mon, test), 0)

## SEE THE ABSOLUTE DIFFERENCE FROM THE ACTUAL
difOfRF <- sum(abs(test$RF - test$count))
difOfLM <- sum(abs(test$LM - test$count))

## SEEMS LIKE THE LM DOES BETTER OVERALL THAN THE RF