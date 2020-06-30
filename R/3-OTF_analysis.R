library(caret)
library(parallel)
library(doParallel)
library(nlme)

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

##Run on-the-fly optimizations / LOO training

LR_OTF<- function(type){
  print(type)
  #Set up data frame:
  lmDF<- as.data.frame(matrix(0,nrow=8*2,ncol=4))
  i<-1
  names(lmDF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                   "Testing RMSE", "Testing R2")
  #Specify covariates:
  if(type == "LR1"){
    vars<- c("PM25", "AirNow")
  }else if(type == "LR2"){
    vars<- c("PM25", "Temperature", "Humidity", "AirNow")
  }else if(type == "LR3"){
    vars<- c("PM25", "Temperature", "Humidity", "Near_hwy", "AirNow")
  }else if(type == "LR4"){
    vars<- c("PM25", "Temperature", "Humidity", "Aroad_500", "AirNow")
  }else if(type == "LR5"){
    vars<- c("PM25", "Temperature", "Humidity", "cos_Month", "cos_Time", "Weekend", "AirNow")
  }else if(type == "LR6"){
    vars<- c("PM25", "Temperature", "Humidity", "cos_Month", "cos_Time", "Weekend", "Near_hwy", "AirNow")
  }else if(type == "LR7"){
    vars<- c("PM25", "Temperature", "Humidity", "cos_Month", "cos_Time", "Weekend", "Aroad_500", "AirNow")
  }
  
  #Run optimization:
  for(t in c(1:8)){
    Preds1<- c()
    Preds2<- c()
    Obs1<- c()
    Obs2<- c()
    for(p in 1:length(weeks)){ 
      if(p > t){
        w<- weeks[p]
        training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")), 
                           which(names(dataset) %in% vars)]
        model<- lm(AirNow ~ ., training)
        for(T in 1:2){
          if(p+T <= length(weeks)){
            testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                              which(names(dataset) %in% vars)]
            if(T == 1){
              Preds1<- append(Preds1, predict.lm(model, testing))
              Obs1<- append(Obs1, testing$AirNow)
            }else if(T == 2){
              Preds2<- append(Preds2, predict.lm(model, testing))
              Obs2<- append(Obs2, testing$AirNow)
            }
          }
        }
      }
    }
    lmDF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
    lmDF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
    i<- i+2
  }
  # write.csv(lmDF, paste0("Results/OTF_", type, ".csv"), row.names = FALSE)
  
  print(lmDF[which(lmDF$`Testing RMSE` == min(lmDF$`Testing RMSE`)),])
  print(lmDF[which(lmDF$`Testing R2` == max(lmDF$`Testing R2`)),]) 
}

RE_OTF<- function(type){
  print(type)
  #Set up data frame:
  lmDF<- as.data.frame(matrix(0,nrow=8*2,ncol=4))
  i<-1
  names(lmDF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                  "Testing RMSE", "Testing R2")
  
  #Run optimization:
  for(t in c(1:8)){
    Preds1<- c()
    Preds2<- c()
    Obs1<- c()
    Obs2<- c()
    for(p in 1:length(weeks)){ 
      if(p > t){
        w<- weeks[p]
        training<- DATA[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")), 
                           which(names(DATA) %in% c("PM25", "Temperature", "Humidity", "cos_Month", "cos_Time", "Weekend", "AirNow", "Near_hwy", "ID"))]
        if(type == "RE1"){
          try(model<- lme(AirNow ~ PM25 + Temperature + Humidity + cos_Time + Weekend, 
                          data = training, random = ~1 + PM25 | ID))
        }else if(type == "RE2"){
          try(model<- lme(AirNow ~ PM25 + Temperature + Humidity + cos_Time + Weekend + Near_hwy, 
                          data = training, random = ~1 | ID))
        }
        
        if(exists("model")){
          vars<- names(model$coefficients$fixed[-1])
          vars[vars == "WeekendTRUE"]<- "Weekend"
          vars[vars == "Near_hwyTRUE"]<- "Near_hwy"
          for(T in 1:2){
            if(p+T <= length(weeks)){
              testing<- DATA[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                             which(names(DATA) %in% c("cos_Time","cos_Month","Weekend","PM25",
                                                      "Temperature","Humidity","AirNow",
                                                      "Near_hwy"))]
              data<- testing[,vars]
              preds<- apply(data, MARGIN = 1, function(x){
                sum(x*model$coefficients$fixed[2:(length(vars)+1)]) + model$coefficients$fixed[1] })
        
            if(T == 1){
              Preds1<- append(Preds1, preds)
              Obs1<- append(Obs1, testing$AirNow)
            }else if(T == 2){
              Preds2<- append(Preds2, preds)
              Obs2<- append(Obs2, testing$AirNow)
            }
          }
          }
        }
      }
    }
    lmDF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
    lmDF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
    i<- i+2
    }
  # write.csv(lmDF, paste0("Results/OTF_", type, ".csv"), row.names = FALSE)
  
  print(lmDF[which(lmDF$`Testing RMSE` == min(lmDF$`Testing RMSE`)),])
  print(lmDF[which(lmDF$`Testing R2` == max(lmDF$`Testing R2`)),]) 
}


RF_OTF<- function(type){
  print(type)
  #Set up data frame:
  lmDF<- as.data.frame(matrix(0,nrow=8*2,ncol=4))
  i<-1
  names(lmDF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                  "Testing RMSE", "Testing R2")
  #Specify model:
  myControl<- trainControl(number = 1, savePredictions = "final", 
                           verboseIter = FALSE)
  if(type == "RF1"){
    vars<- c("PM25", "Temperature", "Humidity", "cos_Month", "cos_Time", "Weekend", "AirNow")
    Mtry<- 4
  }else if(type == "RF2"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow", "Near_hwy")
    Mtry<- 4
  }else if(type == "RF3"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow", "Aroad_500")
    Mtry<- 4
  }else if(type == "RF4"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow", 
             "Near_hwy", "Hwy_rush")
    Mtry<- 6
  }
  
  #Run optimization:
  for(t in c(1:8)){
    Preds1<- c()
    Preds2<- c()
    Obs1<- c()
    Obs2<- c()
    
    for(p in 1:length(weeks)){ 
      if(p > t){
        w<- weeks[p]
        training<- DATA[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")), 
                        which(names(DATA) %in% vars)]
        
            model <- train(AirNow ~ ., 
                       data = training, trControl = myControl,
                       method = "ranger",
                       tuneGrid = expand.grid( .mtry = Mtry, 
                                               .splitrule = "extratrees",
                                               .min.node.size = 2 ),
                       metric = "RMSE")
          for(T in 1:2){
            if(p+T <= length(weeks)){
              testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                                which(names(dataset) %in% vars)]
              preds<- data.frame(predict(model, testing))[,1]
            
              if(T == 1){
                Preds1<- append(Preds1, preds)
                Obs1<- append(Obs1, testing$AirNow)
              }else if(T == 2){
                Preds2<- append(Preds2, preds)
                Obs2<- append(Obs2, testing$AirNow)
              }
            }
          }
        }
      }
    lmDF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
    lmDF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
    i<- i+2
  }
  # write.csv(lmDF, paste0("Results/OTF_", type, ".csv"), row.names = FALSE)
  
  print(lmDF[which(lmDF$`Testing RMSE` == min(lmDF$`Testing RMSE`)),])
  print(lmDF[which(lmDF$`Testing R2` == max(lmDF$`Testing R2`)),]) 
}


#### Now run the models:

for(i in 1:7){
  LR_OTF(paste0("LR",i))
}

for(i in 1:2){
  RE_OTF(paste0("RE",i))
}

for(i in 1:7){
  RF_OTF(paste0("RF",i))
}




