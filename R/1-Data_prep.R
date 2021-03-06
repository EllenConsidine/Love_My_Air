library(stringr)
library(chron)
library(lubridate)
library(dplyr)

## Cleaning training set:
collo<- read.csv("Data/Raw_collocated_data.csv")
collo_names<- c("NJH", "I25.1", "I25.2", "I25.3", "LaCasa")
names(collo)<- c("DateTime", "PM25", "Temperature", "Humidity", "AirNow",
                 "PM25", "Temperature", "Humidity", "PM25", "Temperature", "Humidity", 
                 "PM25", "Temperature", "Humidity", "AirNow",
                 rep(c("PM25", "Temperature", "Humidity", "AirNow"),2) )
C1<- cbind(collo$DateTime, rbind(collo[,2:5], cbind(collo[,6:8], AirNow = collo[,15]), 
                                 cbind(collo[,9:11], AirNow = collo[,15]), collo[,12:15],
                                 collo[,16:19])) #Leave out Swansea
C1$ID<- rep(collo_names, each = dim(collo)[1])
C1$Sensor<- rep(c("NJH", "I25", "I25", "I25", "LaCasa"), each = dim(collo)[1])

missing<- which(is.na(C1$PM25) | is.na(C1$Temperature) | is.na(C1$Humidity) | is.na(C1$AirNow))
high<- which(C1$PM25 > 1500)
neg<- which((C1$PM25 < 0) | (C1$Temperature < 0) | (C1$Humidity < 0))

C2<- C1[-c(high, missing, neg),] 

C2$Date<- as.Date(sapply(C2[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                  format = "%m/%d/%Y")
C2$Time<- chron(times = sapply(C2[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2], ":00")}), 
                format = "h:m:s")
C2$Month<- sapply(C2$Date, function(x){month(as.POSIXct(x))})
C2$Weekend<- is.weekend(C2$Date)
roads<- read.csv("Data/Road_lengths.csv")
ColloR<- merge(C2, roads, by = "Sensor")
ColloR$Time<- as.numeric(sapply(as.character(ColloR$Time), function(s){strsplit(s, ":")[[1]][1]}))
ColloR$cos_Time<- cos(as.numeric(ColloR$Time)*2*pi/24)
ColloR$cos_Month<- cos(as.numeric(ColloR$Month)*2*pi/12)
ColloR$sin_Time<- sin(as.numeric(ColloR$Time)*2*pi/24)
ColloR$sin_Month<- sin(as.numeric(ColloR$Month)*2*pi/12)
write.csv(ColloR, "Data/Clean_collocated_hourly_data.csv", row.names = FALSE)


## Cleaning testing set:

test<- read.csv("Data/test-set.csv")
test2<- read.csv("Data/test-set_Nov-Dec.csv")

Test<- rbind(test, test2)

colnames(Test)<- c("DateTime", rep(c("PM2.5_A", "PM2.5_B", "Temperature", "Humidity"),2),
                   "AirNow", "AirNow")

test_data<- rbind(Test[,c(1,2:5,10)], Test[,c(1,6:9,11)])
test_data$ID<- c(rep("CAMP",dim(Test)[1]), rep("I25_Denver",dim(Test)[1]))

test_data[,2:6]<- apply(test_data[,2:6], MARGIN = 2,
                        function(y){as.numeric(as.character(y))})
test_data[,c(1,7)]<- apply(test_data[,c(1,7)], MARGIN = 2,
                           function(y){as.character(y)})

test_data[test_data < 0]<- NA
missing<- which(is.na(test_data$PM2.5_A) | is.na(test_data$Temperature)
                | is.na(test_data$Humidity) | is.na(test_data$AirNow))
high<- which(test_data$PM2.5_A > 1500)

Test<- test_data[-c(high, missing),]

Test$Date<- as.Date(sapply(Test$DateTime, function(x){strsplit(x, " ")[[1]][1]}),
                    format = "%Y-%m-%d")
Test$Time<- chron(times = sapply(Test$DateTime, function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}),
                format = "h:m:s")
Test$Month<- sapply(Test$Date, function(x){month(as.POSIXct(x))})

Test$Weekend<- is.weekend(Test$Date)

Test$Time<- as.numeric(sapply(as.character(Test$Time), function(s){strsplit(s, ":")[[1]][1]}))
Test$cos_Time<- cos(as.numeric(Test$Time)*2*pi/24)
Test$cos_Month<- cos(as.numeric(Test$Month)*2*pi/12)
Test$sin_Time<- sin(as.numeric(Test$Time)*2*pi/24)
Test$sin_Month<- sin(as.numeric(Test$Month)*2*pi/12)

with_roads<- merge(Test, roads, by.x = "ID", by.y = "Sensor")

write.csv(with_roads, "Data/Cleaned_test_data.csv", row.names = FALSE)

