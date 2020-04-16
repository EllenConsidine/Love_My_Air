library(caret)
library(parallel)
library(doParallel)

DATA<- read.csv("~/Data/No-outliers_final_data.csv")
# DATA$Time<- cos(as.numeric(DATA$Time)*pi/24)
# DATA$Month<- cos(as.numeric(DATA$Month)*pi/12)
DATA$Date<- as.Date(DATA$Date)

dataset<- DATA[,-which(names(DATA)%in%c("Date", "ID"))]

months<- unique(dataset$Month)
month_names<- c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
days<- as.numeric(sapply(DATA$Date, function(s){strsplit(as.character(s),"-")[[1]][3]}))
DATA$Date<- as.Date(DATA$Date)
weeks<- seq.Date(min(DATA$Date), max(DATA$Date), by = 7)

#SLR optimization:
lm0DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(lm0DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")), 
                         which(names(dataset) %in% c("PM25","AirNow"))]
      model<- lm(AirNow ~ ., training)
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("PM25", "AirNow"))]
          if(T == 1){
            Preds1<- append(Preds1, predict.lm(model, testing))
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, predict.lm(model, testing))
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, predict.lm(model, testing))
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, predict.lm(model, testing))
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  lm0DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  lm0DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  lm0DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  lm0DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(lm0DF, "~/Data/OTF_not-avg_SLR_no-outliers.csv",
          row.names = FALSE)

lm0DF[which(lm0DF$`Testing RMSE` == min(lm0DF$`Testing RMSE`)),]
lm0DF[which(lm0DF$`Testing R2` == max(lm0DF$`Testing R2`)),] 


#MLR1 optimization:
lmDF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(lmDF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")), 
                         which(names(dataset) %in% c("PM25","Temperature",
                                                     "Humidity","AirNow"))]
      model<- lm(AirNow ~ ., training)
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow","Aroad_500"))]
          if(T == 1){
            Preds1<- append(Preds1, predict.lm(model, testing))
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, predict.lm(model, testing))
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, predict.lm(model, testing))
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, predict.lm(model, testing))
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  lmDF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  lmDF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  lmDF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  lmDF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(lmDF, "~/Data/OTF_not-avg_MLR1_no-outliers.csv",
          row.names = FALSE)

lmDF[which(lmDF$`Testing RMSE` == min(lmDF$`Testing RMSE`)),]
lmDF[which(lmDF$`Testing R2` == max(lmDF$`Testing R2`)),] 

#MLR2 optimization:
lm2DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(lm2DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                        "Temperature","Humidity","AirNow","Aroad_500"))]
      model<- lm(AirNow ~ ., training)
      for(T in 1){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow","Aroad_500"))]
          if(T == 1:4){
            Preds1<- append(Preds1, predict.lm(model, testing))
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, predict.lm(model, testing))
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, predict.lm(model, testing))
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, predict.lm(model, testing))
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  lm2DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  lm2DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  lm2DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  lm2DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

##In predict.lm(model, testing) :
# prediction from a rank-deficient fit may be misleading

write.csv(lm2DF, "~/Data/OTF_not-avg_MLR2_no-outliers.csv",
          row.names = FALSE)

lm2DF[which(lm2DF$`Testing RMSE` == min(lm2DF$`Testing RMSE`)),]
lm2DF[which(lm2DF$`Testing R2` == max(lm2DF$`Testing R2`)),] 

#MLR2.5 optimization:
lm2_5DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(lm2_5DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                 "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                     "Temperature","Humidity","AirNow"))]
      model<- lm(AirNow ~ ., training)
      for(T in 1){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow"))]
          if(T == 1:4){
            Preds1<- append(Preds1, predict.lm(model, testing))
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, predict.lm(model, testing))
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, predict.lm(model, testing))
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, predict.lm(model, testing))
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  lm2_5DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  lm2_5DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  lm2_5DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  lm2_5DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

##In predict.lm(model, testing) :
# prediction from a rank-deficient fit may be misleading

write.csv(lm2_5DF, "~/Data/OTF_not-avg_MLR2-5_no-outliers.csv",
          row.names = FALSE)

lm2_5DF[which(lm2_5DF$`Testing RMSE` == min(lm2_5DF$`Testing RMSE`)),]
lm2_5DF[which(lm2_5DF$`Testing R2` == max(lm2_5DF$`Testing R2`)),] 


#RE optimization:
reDF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(reDF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- DATA[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(DATA) %in% c("Time","Month","Weekend","PM25",
                                        "Temperature","Humidity","AirNow","Aroad_500", "ID"))]
      try(model<- lme(AirNow ~ PM25 + Temperature + Humidity + Time + Weekend, 
                      data = training, random = ~1 + PM25 | ID))
      if(exists("model")){
        vars<- names(model$coefficients$fixed[-1])
        vars[vars == "WeekendTRUE"]<- "Weekend"
        for(T in 1:4){
          if(p+T <= length(weeks)){
            testing<- DATA[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                              which(names(DATA) %in% c("Time","Month","Weekend","PM25",
                                                          "Temperature","Humidity","AirNow",
                                                          "Aroad_500", "ID"))]
            data<- testing[,vars]
            preds<- apply(data, MARGIN = 1, function(x){
              sum(x*model$coefficients$fixed[2:(length(vars)+1)]) + model$coefficients$fixed[1] })
            if(T == 1){
              Preds1<- append(Preds1, preds)
              Obs1<- append(Obs1, testing$AirNow)
            }else if(T == 2){
              Preds2<- append(Preds2, preds)
              Obs2<- append(Obs2, testing$AirNow)
            }else if(T == 3){
              Preds3<- append(Preds3, preds)
              Obs3<- append(Obs3, testing$AirNow)
            }else{
              Preds4<- append(Preds4, preds)
              Obs4<- append(Obs4, testing$AirNow)
            }
          }
        }
        rm(model)
      }
    }
  }
  reDF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  reDF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  reDF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  reDF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  i<- i+4
}

##Error in lme.formula(AirNow ~ PM25 + Temperature + Humidity + Time + Weekend,  : 
# nlminb problem, convergence error code = 1
# message = false convergence (8)

write.csv(reDF, "~/Data/OTF_not-avg_RE_no-outliers.csv",
          row.names = FALSE)

reDF[which(reDF$`Testing RMSE` == min(reDF$`Testing RMSE`)),]
reDF[which(reDF$`Testing R2` == max(reDF$`Testing R2`)),] 

#RF, no roads, no folds optimization:
rf1DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(rf1DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                 "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                     "Temperature","Humidity","AirNow"))]
      myControl<- trainControl(number = 1, savePredictions = "final", 
                               verboseIter = FALSE)
      registerDoSEQ()
      model <- train(AirNow ~ ., 
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 4, #10 for all variables...
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow"))]
          preds<- data.frame(predict(model, testing))[,1]
          if(T == 1){
            Preds1<- append(Preds1, preds)
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, preds)
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, preds)
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, preds)
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  rf1DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  rf1DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  rf1DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  rf1DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(rf1DF, "~/Data/OTF_not-avg_RF1_no-outliers.csv",
          row.names = FALSE)

rf1DF[which(rf1DF$`Testing RMSE` == min(rf1DF$`Testing RMSE`)),]
rf1DF[which(rf1DF$`Testing R2` == max(rf1DF$`Testing R2`)),] 

#RF, no roads, 10-fold CV optimization:
rf2DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(rf2DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                 "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                     "Temperature","Humidity","AirNow"))]
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      cluster <- makeCluster(detectCores() - 4)
      registerDoParallel(cluster)
      model <- train(AirNow ~ ., 
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 4, #10 for all variables...
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      stopCluster(cluster)
      
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow"))]
          preds<- data.frame(predict(model, testing))[,1]
          if(T == 1){
            Preds1<- append(Preds1, preds)
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, preds)
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, preds)
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, preds)
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  rf2DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  rf2DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  rf2DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  rf2DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(rf2DF, "~/Data/OTF_not-avg_RF2_no-outliers.csv",
          row.names = FALSE)

rf2DF[which(rf2DF$`Testing RMSE` == min(rf2DF$`Testing RMSE`)),]
rf2DF[which(rf2DF$`Testing R2` == max(rf2DF$`Testing R2`)),]

# #RF, four roads, 10-fold CV optimization:
# rf3DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
# i<-1
# names(rf3DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
#                  "Testing RMSE", "Testing R2")
# 
# for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
#   Preds1<- c()
#   Preds2<- c()
#   Preds3<- c()
#   Preds4<- c()
#   Obs1<- c()
#   Obs2<- c()
#   Obs3<- c()
#   Obs4<- c()
#   for(p in 1:length(weeks)){
#     if(p > t){
#       w<- weeks[p]
#       training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
#                          which(names(dataset) %in% c("Time","Month","Weekend","PM25",
#                                                      "Temperature","Humidity","AirNow",
#                                                      "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100"))]
#       set.seed(321)
#       IND<- createResample(training[,"AirNow"], 10)
# 
#       myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
#                                savePredictions = "final", index = IND, verboseIter = FALSE,
#                                allowParallel = TRUE)
#       cluster <- makeCluster(detectCores() - 4)
#       registerDoParallel(cluster)
#       model <- train(AirNow ~ .,
#                      data = training, trControl = myControl,
#                      method = "ranger",
#                      tuneGrid = expand.grid( .mtry = 10,
#                                              .splitrule = "extratrees",
#                                              .min.node.size = 2 ),
#                      metric = "RMSE")
#       stopCluster(cluster)
# 
#       for(T in 1:4){
#         if(p+T <= length(weeks)){
#           testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
#                             which(names(dataset) %in% c("Time","Month","Weekend","PM25",
#                                                         "Temperature","Humidity","AirNow",
#                                                         "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100"))]
#           preds<- data.frame(predict(model, testing))[,1]
#           if(T == 1){
#             Preds1<- append(Preds1, preds)
#             Obs1<- append(Obs1, testing$AirNow)
#           }else if(T == 2){
#             Preds2<- append(Preds2, preds)
#             Obs2<- append(Obs2, testing$AirNow)
#           }else if(T == 3){
#             Preds3<- append(Preds3, preds)
#             Obs3<- append(Obs3, testing$AirNow)
#           }else{
#             Preds4<- append(Preds4, preds)
#             Obs4<- append(Obs4, testing$AirNow)
#           }
#         }
#       }
#     }
#   }
# 
#   rf3DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
#   rf3DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
#   rf3DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
#   rf3DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
# 
#   i<- i+4
# }
# 
# write.csv(rf3DF, "~/Data/OTF_not-avg_RF3_no-outliers.csv",
#           row.names = FALSE)
# 
# rf3DF[which(rf3DF$`Testing RMSE` == min(rf3DF$`Testing RMSE`)),]
# rf3DF[which(rf3DF$`Testing R2` == max(rf3DF$`Testing R2`)),]

#RF, one road, 10-fold CV optimization:
rf3DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(rf3DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                 "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                     "Temperature","Humidity","AirNow",
                                                     "Aroad_500"))]
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      cluster <- makeCluster(detectCores() - 4)
      registerDoParallel(cluster)
      model <- train(AirNow ~ .,
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 4,
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      stopCluster(cluster)
      
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow",
                                                        "Aroad_500"))]
          preds<- data.frame(predict(model, testing))[,1]
          if(T == 1){
            Preds1<- append(Preds1, preds)
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, preds)
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, preds)
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, preds)
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  rf3DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  rf3DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  rf3DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  rf3DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(rf3DF, "~/Data/OTF_not-avg_RF3_no-outliers.csv",
          row.names = FALSE)

rf3DF[which(rf3DF$`Testing RMSE` == min(rf3DF$`Testing RMSE`)),]
rf3DF[which(rf3DF$`Testing R2` == max(rf3DF$`Testing R2`)),]

#RF, five roads, 10-fold CV optimization:
rf3_5DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(rf3_5DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                 "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){
    if(p > t){
      w<- weeks[p]
      training<- dataset[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                     "Temperature","Humidity","AirNow",
                                                     "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100", "Lroad_50"))]
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      cluster <- makeCluster(detectCores() - 4)
      registerDoParallel(cluster)
      model <- train(AirNow ~ .,
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 10,
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      stopCluster(cluster)
      
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- dataset[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            which(names(dataset) %in% c("Time","Month","Weekend","PM25",
                                                        "Temperature","Humidity","AirNow",
                                                        "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100", "Lroad_50"))]
          preds<- data.frame(predict(model, testing))[,1]
          if(T == 1){
            Preds1<- append(Preds1, preds)
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, preds)
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, preds)
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, preds)
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  rf3_5DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  rf3_5DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  rf3_5DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  rf3_5DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(rf3_5DF, "~/Data/OTF_not-avg_RF3-5_no-outliers.csv",
          row.names = FALSE)

rf3_5DF[which(rf3_5DF$`Testing RMSE` == min(rf3_5DF$`Testing RMSE`)),]
rf3_5DF[which(rf3_5DF$`Testing R2` == max(rf3_5DF$`Testing R2`)),]


#RF, all roads, 10-fold CV optimization:
rf4DF<- as.data.frame(matrix(0,nrow=7*4,ncol=4))
i<-1
names(rf4DF)<- c("Training set size (weeks)", "Testing set size (weeks)",
                 "Testing RMSE", "Testing R2")

for(t in c(1:4,8,12,16)){ #c(1:4,8,12,16)
  Preds1<- c()
  Preds2<- c()
  Preds3<- c()
  Preds4<- c()
  Obs1<- c()
  Obs2<- c()
  Obs3<- c()
  Obs4<- c()
  for(p in 1:length(weeks)){ 
    if(p > t){
      w<- weeks[p]
      training<- DATA[which((DATA$Date %in% seq.Date(w-(7*t), w-1, by = 1))&(DATA$ID != "LaCasa")),
                         -which(names(DATA) %in% c("ID", "Date"))]
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      cluster <- makeCluster(detectCores() - 4)
      registerDoParallel(cluster)
      model <- train(AirNow ~ ., 
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 10,
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      stopCluster(cluster)
      
      for(T in 1:4){
        if(p+T <= length(weeks)){
          testing<- DATA[which((DATA$Date %in% seq.Date(w, w+(7*T)-1, by = 1))&(DATA$ID == "LaCasa")),
                            -which(names(DATA) %in% c("ID", "Date"))]
          preds<- data.frame(predict(model, testing))[,1]
          if(T == 1){
            Preds1<- append(Preds1, preds)
            Obs1<- append(Obs1, testing$AirNow)
          }else if(T == 2){
            Preds2<- append(Preds2, preds)
            Obs2<- append(Obs2, testing$AirNow)
          }else if(T == 3){
            Preds3<- append(Preds3, preds)
            Obs3<- append(Obs3, testing$AirNow)
          }else{
            Preds4<- append(Preds4, preds)
            Obs4<- append(Obs4, testing$AirNow)
          }
        }
      }
    }
  }
  
  rf4DF[i,]<- c(t, 1, round(sqrt(mean((Preds1 - Obs1)^2)),4), round((cor(Preds1, Obs1))^2,4))
  rf4DF[i+1,]<- c(t, 2, round(sqrt(mean((Preds2 - Obs2)^2)),4), round((cor(Preds2, Obs2))^2,4))
  rf4DF[i+2,]<- c(t, 3, round(sqrt(mean((Preds3 - Obs3)^2)),4), round((cor(Preds3, Obs3))^2,4))
  rf4DF[i+3,]<- c(t, 4, round(sqrt(mean((Preds4 - Obs4)^2)),4), round((cor(Preds4, Obs4))^2,4))
  
  i<- i+4
}

write.csv(rf4DF, "~/Data/OTF_not-avg_RF4_no-outliers.csv",
          row.names = FALSE)

rf4DF[which(rf4DF$`Testing RMSE` == min(rf4DF$`Testing RMSE`)),]
rf4DF[which(rf4DF$`Testing R2` == max(rf4DF$`Testing R2`)),]

#Plot:

data<- read.csv("~/Data/OTF_not-avg_MLR2_no-outliers.csv")

library(raster)
library(RColorBrewer)

r<- c(1:4,8,12,16)
c<-c(1:4)
windows()
par(mfrow=c(1,2))
RMSE_mat<- matrix(0,nrow = length(r), ncol = length(c))
for(i in 1:dim(data)[1]){
  RMSE_mat[which(r == data[i, "Training.set.size..weeks."]),
           which(c == data[i, "Testing.set.size..weeks."])]<- data[i,"Testing.RMSE"]
}
plot(raster(RMSE_mat), axes = FALSE, main = "RMSE", col = rev(brewer.pal(9, "Blues")))

R2_mat<- matrix(0,length(r),length(c))
for(i in 1:dim(data)[1]){
  R2_mat[which(r == data[i, "Training.set.size..weeks."]),
         which(c == data[i, "Testing.set.size..weeks."])]<- data[i,"Testing.R2"]
}
plot(raster(R2_mat), axes = FALSE, main = "R squared", col = brewer.pal(9, "Blues"))

