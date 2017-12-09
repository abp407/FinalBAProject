#Merging Airline Data with the Flights Data
names(airlines)[1]<-"Airline_Code"
names(flights)[5]<-"Airline_Code"
A1<-merge(airlines, flights, by = "Airline_Code")

#Removing columns after Cancellation_Reason
A1 <- subset( A1, select = -c( CANCELLATION_REASON : WEATHER_DELAY)) #Too many missing values
A1 <- subset( A1, select = -c( FLIGHT_NUMBER:TAIL_NUMBER)) #Not relevant
A1 <- subset( A1, select = -c( YEAR)) #Same Year
A1 <- subset( A1, select = -c( Airline_Code)) #We already have the airlines

#Checking the data quality of 2 columns
table(flights$DIVERTED)
table(flights$CANCELLED)
#The 0s and 1s in these 2 columns will not be useful as 1s are 1 or 2 percent of the 0s we  have.

A1 <- subset( A1, select = -c( DIVERTED:CANCELLED))  #Too many 0s compared to 1

# Using 10 percent of the final data for the project

set.seed(100)  # setting seed to reproduce results of random sampling
split<-(.1)
trainingRowIndex <- sample(1:nrow(A1),(split)*nrow(A1))  # row indices for training data
A1 <- A1[trainingRowIndex, ]  # model training data

View(A1)              

#Copying the 10 percent data that we are going to use into a CSV file.

write.csv(A1, file = "E:/Semester 4/BA/UncleanFinalData.csv")

#Checking the data quality of this unclean data to understand the number of missing values.

checkDataQuality(data = A1,
                 out.file.num ="C:/Users/abp40/A1_num.csv",
                 out.file.cat= "C:/Users/abp40/A1_cat.csv")

A1_num<-read.csv("C:/Users/abp40/A1_num.csv")
A1_cat<-read.csv("C:/Users/abp40/A1_cat.csv")
A1_num
head(A1_num)
head(A1_cat)

#Trying to fill the null values of Departure_Time

summary(A2$DEPARTURE_TIME) #Mean=1335. We can change it to whatever we want.

meanDepTime<-mean(A2$DEPARTURE_TIME, na.rm = TRUE)

for (i in 1:581907){
  A2$DEPARTURE_TIME[i] <- with(data=A2,
                               ifelse ((is.na(A2$DEPARTURE_TIME[i])),returnValue(meanDepTime),returnValue(A2$DEPARTURE_TIME[i]))) 
  }

#CHecking the outliers of Departure_Delay and if the number of outliers are significant enough.
DEP_DELAY_OUT<-subset(A1,A1$DEPARTURE_DELAY>150)

A2<-A1
#Working on A2 so that data in A1 remains the same. 

summary(A2$DEPARTURE_DELAY) #Median=1335

#Removing the outliers from Departure delay..>150 ad <-15
for (i in 1:581907){
A2$DEPARTURE_DELAY[i] <- with(data=A2,
                             ifelse (A2$DEPARTURE_DELAY[i]>150 | A2$DEPARTURE_DELAY[i]<(-15),returnValue(NA),returnValue(A2$DEPARTURE_DELAY[i])))  
}

meanDeptDelay<-mean(A2$DEPARTURE_DELAY, na.rm = TRUE)

#Setting those outlier values to mean value
for (i in 1:581907){
  A2$DEPARTURE_DELAY[i] <- with(data=A2,
                               ifelse ((is.na(A2$DEPARTURE_DELAY[i])),returnValue(meanDeptDelay),returnValue(A2$DEPARTURE_DELAY[i])))  
  
}

#Replacing the null values in the column ARRIVAL_TIME
summary(A2$ARRIVAL_TIME)
meanArriTime<-mean(A2$ARRIVAL_TIME, na.rm = TRUE)

for (i in 1:581907){
  A2$ARRIVAL_TIME[i] <- with(data=A2,
                                ifelse ((is.na(A2$ARRIVAL_TIME[i])),returnValue(meanArriTime),returnValue(A2$ARRIVAL_TIME[i])))  
  
}

#Removing the outliers for Arrival Delay
summary(A2$ARRIVAL_DELAY)

ArrivalDelayOutliers<-subset(A2, A2$ARRIVAL_DELAY>100) #Checking the number of values above 80
ArrivalDelayOutliers<-subset(A2, A2$ARRIVAL_DELAY<(-30))#Values below -30

#Replacing the outliers with NA and then finding the mean

for (i in 1:581907){
  A2$ARRIVAL_DELAY[i] <- with(data=A2,
                                ifelse (A2$ARRIVAL_DELAY[i]>100, returnValue(NA),returnValue(A2$ARRIVAL_DELAY[i])))  
}

#Replaing the null values with the mean

meanArriDelay<-mean(A2$ARRIVAL_DELAY, na.rm = TRUE)

for (i in 1:581907){
  A2$ARRIVAL_DELAY[i] <- with(data=A2,
                             ifelse ((is.na(A2$ARRIVAL_DELAY[i])),returnValue(meanArriDelay),returnValue(A2$ARRIVAL_DELAY[i])))  
}

#Replacing the null values for Elapsed Time

summary(A2$ELAPSED_TIME)

meanElapsedTime<-mean(A2$ELAPSED_TIME, na.rm = TRUE)

for(i in 1:581907){A2$ELAPSED_TIME[i]<- with(data=A2,
                                               ifelse((is.na(A2$ELAPSED_TIME[i])),
                                                      returnValue(meanElapsedTime),returnValue(A2$ELAPSED_TIME[i])))}

#Replacing the null values for AIR_TIME

summary(A2$AIR_TIME)
meanAirTime<-mean(A2$AIR_TIME, na.rm = TRUE)

for(i in 1:581907){A2$AIR_TIME[i]<- with(data=A2,
                                           ifelse((is.na(A2$AIR_TIME[i])),
                                                  returnValue(meanAirTime),returnValue(A2$AIR_TIME[i])))}
#Replacing the null values for Wheel_On

summary(A2$WHEELS_ON)

meanWheelOn<-mean(A2$WHEELS_ON, na.rm = TRUE)

for(i in 1:581907){A2$WHEELS_ON[i]<- with(data=A2,
                                            ifelse((is.na(A2$WHEELS_ON[i])),
                                                   returnValue(meanWheelOn),returnValue(A2$WHEELS_ON[i])))}

#Replacing the null values for Wheel_Off

summary(A2$WHEELS_OFF)

meanWheelOff<-mean(A2$WHEELS_OFF, na.rm = TRUE)

for(i in 1:581907){A2$WHEELS_OFF[i]<- with(data=A2,
                                             ifelse((is.na(A2$WHEELS_OFF[i])),
                                                    returnValue(meanWheelOff),returnValue(A2$WHEELS_OFF[i])))}

#Replacing the null values for Taxi_In

summary(A2$TAXI_IN)
meanTaxiIn<-mean(A2$TAXI_IN, na.rm = TRUE)

for(i in 1:581907){A2$TAXI_IN[i]<- with(data=A2,
                                          ifelse((is.na(A2$TAXI_IN[i])),
                                                 returnValue(meanTaxiIn),returnValue(A2$TAXI_IN[i])))}

#Replacing the null values for Taxi_Out
summary(A2$TAXI_OUT)

meanTaxiOut<-mean(A2$TAXI_OUT, na.rm = TRUE)

for(i in 1:581907){A2$TAXI_OUT[i]<- with(data=A2,
                                           ifelse((is.na(A2$TAXI_OUT[i])),
                                                  returnValue(meanTaxiOut),returnValue(A2$TAXI_OUT[i])))}

summary(A2$SCHEDULED_TIME)

scheduledtimeoutier<-subset(A2, A2$SCHEDULED_TIME>350)
for(i in 1:581907){A2$SCHEDULED_TIME[i]<- with(data=A2,
                                         ifelse((A2$SCHEDULED_TIME[i]>350),
                                                returnValue(NA),returnValue(A2$SCHEDULED_TIME[i])))}
meanScheduledTime<-mean(A2$SCHEDULED_TIME, na.rm = TRUE)
meanScheduledTime
for(i in 1:581907){A2$SCEDULED_TIME[i]<- with(data=A2,
                                         ifelse((is.na(A2$SCHEDULED_TIME[i])),
                                                returnValue(meanScheduledTime),returnValue(A2$SCHEDULED_TIME[i])))}


#Checking Data QUality
checkDataQuality(data = A2,
                 out.file.num ="C:/Users/abp40/A2_num.csv",
                 out.file.cat= "C:/Users/abp40/A2_cat.csv")


A2_num<-read.csv("C:/Users/abp40/A2_num.csv")
A2_cat<-read.csv("C:/Users/abp40/A2_cat.csv")
A2_num
A2_cat

#Writing the cleaned data to a csv file

write.csv(A2, file = "E:/Semester 4/BA/FinalData.csv")

checkDataQuality(data = FinalData,
                 out.file.num ="C:/Users/abp40/Final1_num.csv",
                 out.file.cat= "C:/Users/abp40/Final1_cat.csv")

FinalData_num<-read.csv("C:/Users/abp40/Final1_num.csv")
FinalData_cat<-read.csv("C:/Users/abp40/Final1_cat.csv")
FinalData_num
FInalData_cat
head(A1_num)
head(A1_cat)
FinalData_num

