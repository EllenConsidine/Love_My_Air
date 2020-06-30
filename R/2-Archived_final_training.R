library(dplyr)

#Read in and format data:
DATA<- read.csv("Data/Clean_collocated_hourly_data.csv")
DATA$Date<- as.Date(DATA$Date)

#Adding in other spatial and temporal variables:
DATA$Rush_hour<- DATA$Weekend & (DATA$Time %in% c(8:9, 16:18))
near_hwy<- data.frame(ID=c("NJH", "LaCasa", "I25.1", "I25.2", "I25.3"), 
                      Near_hwy=c(FALSE, FALSE, TRUE, TRUE, TRUE))
DATA<- inner_join(DATA, near_hwy, by = "ID")
DATA$Hwy_rush<- DATA$Near_hwy & DATA$Rush_hour

days<- as.numeric(sapply(DATA$Date, function(s){strsplit(as.character(s),"-")[[1]][3]}))
weeks<- seq.Date(min(DATA$Date), max(DATA$Date), by = 7)

dataset<- DATA[,-which(names(DATA)%in%c("Date", "ID"))]

# #Data summary:
# for(s in unique(DATA$ID)){
#   print(s)
#   data<- DATA[which(DATA$ID == s),]
#   print(quantile(data[,"AirNow"])) #Or PM25
#   print(sd(data[,"AirNow"])) #Or PM25
# }

#Linear models:
LM1<- lm(AirNow ~ PM25, DATA)
LM2<- lm(AirNow ~ PM25 + Temperature + Humidity, DATA)
LM3<- lm(AirNow ~ PM25 + Temperature + Humidity + Near_hwy, DATA)
LM4<- lm(AirNow ~ PM25 + Temperature + Humidity + Aroad_500, DATA)
LM5<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + cos_Time + Weekend, DATA)
LM6<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + cos_Time + Weekend + Near_hwy, DATA)
LM7<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + cos_Time + Weekend + Aroad_500, DATA)

#Mixed linear models:
library(nlme)
nRE1<- lme(AirNow ~ PM25 + Temperature+ Humidity, data = DATA, random = ~1 | ID)
nRE2<- lme(AirNow ~ PM25 + Temperature+ Humidity, data = DATA, 
           random = ~1 + PM25 | ID)
nRE3<- lme(AirNow ~ PM25 + Temperature+ Humidity + cos_Month + cos_Time + Weekend,
           data = DATA, random = ~1 + PM25 | ID)

performance<- function(model, DATA){
  vars<- names(model$coefficients$fixed[-1])
  vars[vars == "WeekendTRUE"]<- "Weekend"
  data<- DATA[,vars]
  preds<- apply(data, MARGIN = 1, function(x){
    sum(x*model$coefficients$fixed[2:(length(vars)+1)]) + model$coefficients$fixed[1] })
  obs<- DATA$AirNow
  
  print(paste("RMSE =", round(sqrt(mean((preds - obs)^2)),3)))
  print(paste("R^2 =", round((cor(preds, obs))^2,3)))
}

for(i in 1:3){
  model_name<- paste0("nRE", as.character(i))
  print(paste("Model =", model_name))
  model<- get(model_name, DATA)
  performance(model)
}

#Machine learning:
dataset<- DATA[,-which(names(DATA)%in%c("Date", "ID"))]

library(caret)
library(parallel)
library(doParallel)

#Leave one out (LOO):
source("LOO_functions.R")

run_LOO(type = "RF1", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "RF2", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "RF3", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "RF4", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "RF5", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "RF6", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "RF7", train_pos = c(1:dim(dataset)[1]), name = "None")

sensors<- c("NJH", "LaCasa", "I25.1", "I25.2", "I25.3")

sink("Training_LOO.txt")
for(s in sensors){
  LOO_linear(type = "LR1", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "LR2", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "LR3", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "LR4", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "LR5", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "LR6", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "LR7", test_pos = which(DATA$ID == s), name = s)
  
  LOO_hlm(type = "RE1", test_pos = which(DATA$ID == s), name = s)
  LOO_hlm(type = "RE2", test_pos = which(DATA$ID == s), name = s)
  LOO_hlm(type = "RE3", test_pos = which(DATA$ID == s), name = s)
  
  run_LOO(type = "RF1", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "RF2", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "RF3", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "RF4", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "RF5", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "RF6", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "RF7", train_pos = which(DATA$ID != s), name = s)
}
sink()
