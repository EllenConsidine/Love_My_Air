#Read in data:
test<- read.csv("~/Cleaned_test_data.csv")

names(test)<- c("ID", "DateTime", "PM25", names(test)[4:25])

test$Time<- sapply(test$Time, function(s){strsplit(as.character(s), ":")[[1]][1]})
test[,-which(names(test) %in% c("ID", "DateTime", "Date", "Weekend"))]<- 
  apply(test[,-which(names(test) %in% c("ID", "DateTime", "Date", "Weekend"))], 
        MARGIN = 2, function(y){as.numeric(as.character(y))})

test$Date<- as.Date(test$Date)

test$Time<- cos(as.numeric(test$Time)*pi/24)
test$Month<- cos(as.numeric(test$Month)*pi/12)

test<- test[-which(test$AirNow == 0),] #Removed 22 points

test<- test[-which(((test$Date == "2019-10-10")|(test$Date == "2019-10-11")|
                      (test$Date == "2019-10-27")|(test$Date == "2019-10-29"))
                   &(test$ID == "I25_Denver")),]
#This ^^^ got rid of 41 observations

Test<- as.data.frame(test[,c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", 
              "Aroad_50", "Croad_50", "Lroad_50",
              "Aroad_100", "Croad_100", "Lroad_100", 
              "Aroad_250", "Croad_250", "Lroad_250", 
              "Aroad_500", "Croad_500", "Lroad_500",
              "AirNow")])

#Read in models:
all_roads_model<- readRDS("~/No-outliers_all-vars_None.rds")
five_roads_model<- readRDS("~/No-outliers_five-roads_None.rds")
one_road_model<- readRDS("~/No-outliers_one-road_None.rds")
no_roads_model<- readRDS("~/No-outliers_no-roadsNone.rds")
no_folds_model<- readRDS("~/No-outliers_no-folds_None.rds")

preds1<- data.frame(predict(all_roads_model, Test))
preds2<- data.frame(predict(five_roads_model, Test))
preds3<- data.frame(predict(one_road_model, Test))
preds4<- data.frame(predict(no_roads_model, Test))
preds5<- data.frame(predict(no_folds_model, Test))

# compare1<- cbind(preds1[,1], Test$AirNow)
# compare2<- cbind(preds2[,1], Test$AirNow)
# compare3<- cbind(preds3[,1], Test$AirNow)
# compare4<- cbind(preds4[,1], Test$AirNow)
# compare5<- cbind(preds5[,1], Test$AirNow)

#Change out these depending on the sensor:
CAMP_pos<- which(test$ID == "CAMP")
compare1<- cbind(preds1[CAMP_pos,1], Test$AirNow[CAMP_pos])
compare2<- cbind(preds2[CAMP_pos,1], Test$AirNow[CAMP_pos])
compare3<- cbind(preds3[CAMP_pos,1], Test$AirNow[CAMP_pos])
compare4<- cbind(preds4[CAMP_pos,1], Test$AirNow[CAMP_pos])
compare5<- cbind(preds5[CAMP_pos,1], Test$AirNow[CAMP_pos])
I25D_pos<- which(test$ID == "I25_Denver")
compare1<- cbind(preds1[I25D_pos,1], Test$AirNow[I25D_pos])
compare2<- cbind(preds2[I25D_pos,1], Test$AirNow[I25D_pos])
compare3<- cbind(preds3[I25D_pos,1], Test$AirNow[I25D_pos])
compare4<- cbind(preds4[I25D_pos,1], Test$AirNow[I25D_pos])
compare5<- cbind(preds5[I25D_pos,1], Test$AirNow[I25D_pos])

#RMSE:
sqrt(mean((compare1[,1] - compare1[,2])^2)) #All vars, 5 roads, 1 road, no roads, no folds
sqrt(mean((compare2[,1] - compare2[,2])^2))
sqrt(mean((compare3[,1] - compare3[,2])^2))
sqrt(mean((compare4[,1] - compare4[,2])^2))
sqrt(mean((compare5[,1] - compare5[,2])^2))

#R^2:
(cor(compare1[,1], compare1[,2]))^2
(cor(compare2[,1], compare2[,2]))^2
(cor(compare3[,1], compare3[,2]))^2
(cor(compare4[,1], compare4[,2]))^2
(cor(compare5[,1], compare5[,2]))^2

#Inspect:
plot(compare1[,2], compare1[,1], xlab = "Observed", ylab = "Predicted")
test[which(compare1[,2] - compare1[,1] > 15),]

plot(compare2[,2], compare2[,1], xlab = "Observed", ylab = "Predicted")
test[which(compare2[,2] - compare2[,1] > 15), 1:7] #Same

test[which(compare1[,1] > 40), 1:7]

###Other models: get from Final_sensor_calibration.R
these_results<- function(preds){
  CAMP<- preds[which(test$ID == "CAMP")]
  Denv<- preds[which(test$ID == "I25_Denver")]
  print("CAMP:")
  print(sqrt(mean((CAMP - Test$AirNow[which(test$ID == "CAMP")])^2)))
  print((cor(CAMP, Test$AirNow[which(test$ID == "CAMP")]))^2)
  print("I25 Denver:")
  print(sqrt(mean((Denv - Test$AirNow[which(test$ID == "I25_Denver")])^2)))
  print((cor(Denv, Test$AirNow[which(test$ID == "I25_Denver")]))^2)
}

LM0_preds<- predict(LM0, Test)
LM1_preds<- predict(LM1, Test)
LM2_preds<- predict(LM2, Test)
LM3_preds<- predict(LM3, Test)

these_results(LM0_preds)
these_results(LM1_preds)
these_results(LM2_preds)
these_results(LM3_preds)

#Get "performance" from Final_sensor_calibration.R
performance(nRE0, Test[which(test$ID == "CAMP"),])
performance(nRE0, Test[which(test$ID == "I25_Denver"),])
performance(nRE1, Test[which(test$ID == "CAMP"),])
performance(nRE1, Test[which(test$ID == "I25_Denver"),])
performance(nRE2, Test[which(test$ID == "CAMP"),])
performance(nRE2, Test[which(test$ID == "I25_Denver"),])


### Make on-the-fly predictions:
library(nlme)
library(caret)
library(parallel)
library(doParallel)

test$Date<- as.Date(test$Date)
weeks<- seq.Date(min(test$Date), max(test$Date), by = 7)

Test_set<- function(tr_opt, ts_opt, model_type, sensor){
  Preds<- c()
  Obs<- c()
  for(p in 1:length(weeks)){ #start at 1
    go<- FALSE
    if(p > tr_opt){
      w<- weeks[p]
      training<- test[which((test$Date %in% seq.Date(w-(7*tr_opt), w-1, by = 1))
                    &(test$ID == sensor)),]
      if(p+ts_opt <= length(weeks)){
        testing<- test[which((test$Date %in% seq.Date(w, w+(7*ts_opt)-1, by = 1))
                     &(test$ID == sensor)),]
        if((dim(training)[1] > 0)&(dim(testing)[1] > 0)){
          go<- TRUE
        }
      }
    }
    
    if((model_type == "MLR1")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "SLR")&(go)){
      model<- lm(AirNow ~ PM25, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "MLR1-5")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + Month + Time + Weekend,
                 data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "MLR2")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + Month + Time + Weekend + Aroad_500,
                 data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "RE")&(go)){
      try(model<- lme(AirNow ~ PM25 + Temperature + Humidity + Time + Weekend, 
                      data = training, random = ~1 + PM25 | ID))
      if(exists("model")){
        vars<- names(model$coefficients$fixed[-1])
        vars[vars == "WeekendTRUE"]<- "Weekend"
        data<- testing[,vars]
        preds<- apply(data, MARGIN = 1, function(x){
          sum(x*model$coefficients$fixed[2:(length(vars)+1)]) + model$coefficients$fixed[1] })
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
        rm("model")
      }
    }else if((model_type == "RF_no-folds")&(go)){
      vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow")
      training<- training[, vars]
      
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
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }else if((model_type == "RF_no-roads")&(go)){
      vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow")
      training<- training[, vars]
      
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      
      cluster <- makeCluster(detectCores() - 3)
      registerDoParallel(cluster)
      
      model<- train(AirNow ~ ., 
                    data = training, trControl = myControl,
                    method = "ranger",
                    tuneGrid = expand.grid( .mtry = 4,
                                            .splitrule = "extratrees",
                                            .min.node.size = 2 ),
                    metric = "RMSE", importance = "permutation")
      
      stopCluster(cluster)
      
      preds<- data.frame(predict(model, testing))[,1]
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "RF_1-road")&(go)){
      vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
               "Aroad_500")
      training<- training[, vars]
      
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      
      cluster <- makeCluster(detectCores() - 3)
      registerDoParallel(cluster)
      
      model<- train(AirNow ~ ., 
                    data = training, trControl = myControl,
                    method = "ranger",
                    tuneGrid = expand.grid( .mtry = 4,
                                            .splitrule = "extratrees",
                                            .min.node.size = 2 ),
                    metric = "RMSE", importance = "permutation")
      
      stopCluster(cluster)
      
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }else if((model_type == "RF_5-roads")&(go)){
      vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
               "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100", "Lroad_50")
      training<- training[, vars]
      
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      
      cluster <- makeCluster(detectCores() - 3)
      registerDoParallel(cluster)
      
      model<- train(AirNow ~ ., 
                    data = training, trControl = myControl,
                    method = "ranger",
                    tuneGrid = expand.grid( .mtry = 10,
                                            .splitrule = "extratrees",
                                            .min.node.size = 2 ),
                    metric = "RMSE", importance = "permutation")
      
      stopCluster(cluster)
      
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }else if((model_type == "RF_all-roads")&(go)){
      vars<- names(Test)
      training<- training[, vars]
      
      set.seed(321)
      IND<- createResample(training[,"AirNow"], 10)
      
      myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                               savePredictions = "final", index = IND, verboseIter = FALSE,
                               allowParallel = TRUE)
      
      cluster <- makeCluster(detectCores() - 3)
      registerDoParallel(cluster)
      
      model<- train(AirNow ~ ., 
                    data = training, trControl = myControl,
                    method = "ranger",
                    tuneGrid = expand.grid( .mtry = 10,
                                            .splitrule = "extratrees",
                                            .min.node.size = 2 ),
                    metric = "RMSE", importance = "permutation")
      
      stopCluster(cluster)
      
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }
  }
  DF<- cbind(Preds, Obs)
  return(DF)
}

results<- function(df){
  print("RMSE:")
  print(sqrt(mean((df[,1] - df[,2])^2)))
  print("R^2:")
  print((cor(df[,1], df[,2]))^2)
}

LM0_camp<- Test_set(3,1,"SLR", "CAMP")
results(LM0_camp)
LM0_denv<- Test_set(3,1,"SLR", "I25_Denver")
results(LM0_denv)

LM1_camp<- Test_set(3,1,"MLR1", "CAMP")
results(LM1_camp)
LM1_denv<- Test_set(3,1,"MLR1", "I25_Denver")
results(LM1_denv)

LM1_5_camp<- Test_set(3,1,"MLR1-5", "CAMP")
results(LM1_5_camp)
LM1_5_denv<- Test_set(3,1,"MLR1-5", "I25_Denver")
results(LM1_5_denv)

LM2_camp<- Test_set(3,1,"MLR2", "CAMP") 
results(LM2_camp)
LM2_denv<- Test_set(3,1,"MLR2", "I25_Denver")
results(LM2_denv)

RE_camp<- Test_set(3,1,"RE", "CAMP")
results(RE_camp)
RE_denv<- Test_set(3,1,"RE", "I25_Denver")
results(RE_denv)

RF1_camp<- Test_set(3,2,"RF_no-folds", "CAMP")
results(RF1_camp)
RF1_denv<- Test_set(3,2,"RF_no-folds", "I25_Denver")
results(RF1_denv)

RF2_camp<- Test_set(3,2,"RF_no-roads", "CAMP")
results(RF2_camp)
RF2_denv<- Test_set(3,2,"RF_no-roads", "I25_Denver")
results(RF2_denv)

RF3_camp<- Test_set(12,1,"RF_1-road", "CAMP")
results(RF3_camp)
RF3_denv<- Test_set(12,1,"RF_1-road", "I25_Denver")
results(RF3_denv)

RF4_camp<- Test_set(8,1,"RF_5-roads", "CAMP")
results(RF4_camp)
RF4_denv<- Test_set(8,1,"RF_5-roads", "I25_Denver")
results(RF4_denv)

RF5_camp<- Test_set(8,2,"RF_all-roads", "CAMP")
results(RF5_camp)
RF5_denv<- Test_set(8,2,"RF_all-roads", "I25_Denver")
results(RF5_denv)


