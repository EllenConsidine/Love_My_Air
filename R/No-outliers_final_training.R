
#Read in and format data:
DATA<- read.csv("~/No-outliers_final_data.csv")
# DATA$Time<- cos(as.numeric(DATA$Time)*pi/24)
# DATA$Month<- cos(as.numeric(DATA$Month)*pi/12)
DATA$Date<- as.Date(DATA$Date)

months<- unique(DATA$Month)
month_names<- c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
days<- as.numeric(sapply(DATA$Date, function(s){strsplit(as.character(s),"-")[[1]][3]}))
weeks<- seq.Date(min(DATA$Date), max(DATA$Date), by = 7)

dataset<- DATA[,-which(names(DATA)%in%c("Date", "ID"))]

#Data summary:
for(s in unique(DATA$ID)){
  print(s)
  data<- DATA[which(DATA$ID == s),]
  print(quantile(data[,"AirNow"])) #Or PM25
  print(sd(data[,"AirNow"])) #Or PM25
}

#Linear models:
LM0<- lm(AirNow ~ PM25, DATA)
LM1<- lm(AirNow ~ PM25 + Temperature + Humidity, DATA)
LM2<- lm(AirNow ~ PM25 + Temperature + Humidity + Month + Time + Weekend, DATA)
LM3<- lm(AirNow ~ PM25 + Temperature + Humidity + Month + Time + Weekend + Aroad_500, DATA)

#Mixed linear models:
library(nlme)
nRE0<- lme(AirNow ~ PM25 + Temperature+ Humidity, data = DATA, random = ~1 | ID)
nRE1<- lme(AirNow ~ PM25 + Temperature+ Humidity, data = DATA, 
           random = ~1 + PM25 | ID)
nRE2<- lme(AirNow ~ PM25 + Temperature+ Humidity + Month + Time + Weekend,
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

for(i in 0:2){
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

run_LOO(type = "no-folds", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "no-roads", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "one-road", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "five-roads", train_pos = c(1:dim(dataset)[1]), name = "None")
run_LOO(type = "all-vars", train_pos = c(1:dim(dataset)[1]), name = "None")

#Leave one out (LOO):
source("~/LOO_functions.R")

sensors<- c("NJH", "LaCasa", "I25.1", "I25.2", "I25.3")

for(s in sensors){
  LOO_linear(type = "SLR_1", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "MLR_1", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "MLR_2", test_pos = which(DATA$ID == s), name = s)
  LOO_linear(type = "MLR_3", test_pos = which(DATA$ID == s), name = s)
  
  LOO_hlm(type = "RE_1", test_pos = which(DATA$ID == s), name = s)
  LOO_hlm(type = "RE_2", test_pos = which(DATA$ID == s), name = s)
  LOO_hlm(type = "RE_3", test_pos = which(DATA$ID == s), name = s)
  
  run_LOO(type = "no-folds", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "no-roads", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "one-road", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "five-roads", train_pos = which(DATA$ID != s), name = s)
  run_LOO(type = "all-vars", train_pos = which(DATA$ID != s), name = s)
}

