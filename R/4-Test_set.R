library(dplyr)

#Read in data:
test<- read.csv("Data/Cleaned_test_data.csv")
names(test)<- c("ID", "DateTime", "PM25", names(test)[4:29])

test$Date<- as.Date(test$Date)

test<- test[-which(test$AirNow == 0),] #Removed 22 points

#Adding in other spatial and temporal variables:
near_hwy<- data.frame(ID=c("CAMP", "I25_Denver"), 
                      Near_hwy=c(FALSE, TRUE))

test<- inner_join(test, near_hwy, by = "ID")

Test<- as.data.frame(test[,c("cos_Time", "sin_Time", "cos_Month", "sin_Month", 
                             "Weekend", "PM25", "Temperature", "Humidity", 
              "Aroad_50", "Croad_50", "Lroad_50",
              "Aroad_100", "Croad_100", "Lroad_100", 
              "Aroad_250", "Croad_250", "Lroad_250", 
              "Aroad_500", "Croad_500", "Lroad_500",
              "AirNow", "Near_hwy")])

#Read in models:
RF1<- readRDS("Models/Archived_RF1_None.rds")
RF2<- readRDS("Models/Archived_RF2_None.rds")
RF3<- readRDS("Models/Archived_RF3_None.rds")
RF4<- readRDS("Models/Archived_RF4_None.rds")
RF5<- readRDS("Models/Archived_RF5_None.rds")

preds1<- data.frame(predict(RF1, Test))
preds2<- data.frame(predict(RF2, Test))
preds3<- data.frame(predict(RF3, Test))
preds4<- data.frame(predict(RF4, Test))
preds5<- data.frame(predict(RF5, Test))

##Both test set sensors:
# compare1<- cbind(preds1[,1], Test$AirNow)
# compare2<- cbind(preds2[,1], Test$AirNow)
# compare3<- cbind(preds3[,1], Test$AirNow)
compare4<- cbind(preds4[,1], Test$AirNow)
# compare5<- cbind(preds5[,1], Test$AirNow)

##Create plots:
my_max<- max(c(DATA$AirNow, preds4[,1]))
tr_results<- RF4$pred

windows()
par(mfrow=c(1,2))
plot(tr_results$obs, tr_results$pred, 
     xlab = "Observed PM2.5", ylab = "Predicted PM2.5",
     main = "Training Set", xlim=c(0,my_max), ylim=c(0,my_max))
abline(0,1)
plot(compare4[,2], compare4[,1], 
     xlab = "Observed PM2.5", ylab = "Predicted PM2.5",
     main = "Testing Set", xlim=c(0,my_max), ylim=c(0,my_max))
abline(0,1)
dev.off()

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
sqrt(mean((compare1[,1] - compare1[,2])^2))
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

###Other models: get from 2-Archived_final_training.R
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

LM1_preds<- predict(LM1, Test)
LM2_preds<- predict(LM2, Test)
LM3_preds<- predict(LM3, Test)
LM4_preds<- predict(LM4, Test)
LM5_preds<- predict(LM5, Test)
LM6_preds<- predict(LM6, Test)
LM7_preds<- predict(LM7, Test)

these_results(LM1_preds)
these_results(LM2_preds)
these_results(LM3_preds)
these_results(LM4_preds)
these_results(LM5_preds)
these_results(LM6_preds)
these_results(LM7_preds)

#Get "performance" function from 2-Archived_final_training.R
performance(nRE1, Test[which(test$ID == "CAMP"),])
performance(nRE1, Test[which(test$ID == "I25_Denver"),])
performance(nRE2, Test[which(test$ID == "CAMP"),])
performance(nRE2, Test[which(test$ID == "I25_Denver"),])
performance(nRE3, Test[which(test$ID == "CAMP"),])
performance(nRE3, Test[which(test$ID == "I25_Denver"),])


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
    
    if((model_type == "LR1")&(go)){
      model<- lm(AirNow ~ PM25, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "LR2")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "LR3")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + Near_hwy, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "LR4")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + Aroad_500, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "LR5")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + sin_Month + cos_Time + Weekend, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "LR6")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + sin_Month + cos_Time + Weekend + Near_hwy, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "LR7")&(go)){
      model<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + sin_Month + cos_Time + Weekend + Aroad_500, data = training)
      preds<- predict(model, testing)
      Preds<- append(Preds, preds)
      Obs<- append(Obs, testing$AirNow)
    }else if((model_type == "RE1")&(go)){
      try(model<- lme(AirNow ~ PM25 + Temperature + Humidity + cos_Month + cos_Time + Weekend, 
                      data = training, random = ~1 + PM25| ID))
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
    }else if((model_type == "RE2")&(go)){
      try(model<- lme(AirNow ~ PM25 + Temperature + Humidity + cos_Month + cos_Time + Weekend + Near_hwy, 
                      data = training, random = ~1 | ID))
      if(exists("model")){
        vars<- names(model$coefficients$fixed[-1])
        vars[vars == "WeekendTRUE"]<- "Weekend"
        vars[vars == "Near_hwyTRUE"]<- "Near_hwy"
        data<- testing[,vars]
        preds<- apply(data, MARGIN = 1, function(x){
          sum(x*model$coefficients$fixed[2:(length(vars)+1)]) + model$coefficients$fixed[1] })
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
        rm("model")
      }
    }else if((model_type == "RF1")&(go)){
      vars<- c("PM25", "Temperature", "Humidity", "AirNow")
      training<- training[, vars]
      
      myControl<- trainControl(number = 1, savePredictions = "final", 
                               verboseIter = FALSE)
      
      model <- train(AirNow ~ ., 
                        data = training, trControl = myControl,
                        method = "ranger",
                        tuneGrid = expand.grid( .mtry = 3,
                                                .splitrule = "extratrees",
                                                .min.node.size = 2 ),
                        metric = "RMSE")
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }else if((model_type == "RF2")&(go)){
      vars<- c("cos_Time", "sin_Time", "sin_Month", "cos_Month", "Weekend", 
               "PM25", "Temperature", "Humidity", "AirNow")
      training<- training[, vars]
      
      myControl<- trainControl(number = 1, savePredictions = "final", 
                               verboseIter = FALSE)
      
      model <- train(AirNow ~ ., 
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 5,
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }else if((model_type == "RF3")&(go)){
      vars<- c("cos_Time", "sin_Time", "sin_Month", "cos_Month", "Weekend", 
               "PM25", "Temperature", "Humidity", "AirNow", "Near_hwy")
      training<- training[, vars]
      
      myControl<- trainControl(number = 1, savePredictions = "final", 
                               verboseIter = FALSE)
      
      model <- train(AirNow ~ ., 
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 7,
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
      if(exists("model")){
        preds<- data.frame(predict(model, testing))[,1]
        Preds<- append(Preds, preds)
        Obs<- append(Obs, testing$AirNow)
      }
      
      rm("model")
    }else if((model_type == "RF4")&(go)){
      vars<- c("cos_Time", "sin_Time", "sin_Month", "cos_Month", "Weekend", 
               "PM25", "Temperature", "Humidity", "AirNow", "Aroad_500")
      training<- training[, vars]
      
      myControl<- trainControl(number = 1, savePredictions = "final", 
                               verboseIter = FALSE)
      
      model <- train(AirNow ~ ., 
                     data = training, trControl = myControl,
                     method = "ranger",
                     tuneGrid = expand.grid( .mtry = 7,
                                             .splitrule = "extratrees",
                                             .min.node.size = 2 ),
                     metric = "RMSE")
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

#Linear model results:
LM1_camp<- Test_set(3,1,"LR1", "CAMP")
results(LM1_camp)
LM1_denv<- Test_set(3,1,"LR1", "I25_Denver")
results(LM1_denv)

LM2_camp<- Test_set(3,1,"LR2", "CAMP")
results(LM2_camp)
LM2_denv<- Test_set(3,1,"LR2", "I25_Denver")
results(LM2_denv)

LM3_camp<- Test_set(8,1,"LR3", "CAMP")
results(LM3_camp)
LM3_denv<- Test_set(8,1,"LR3", "I25_Denver")
results(LM3_denv)

LM4_camp<- Test_set(8,1,"LR4", "CAMP")
results(LM4_camp)
LM4_denv<- Test_set(8,1,"LR4", "I25_Denver")
results(LM4_denv)

LM5_camp<- Test_set(3,1,"LR5", "CAMP")
results(LM5_camp)
LM5_denv<- Test_set(3,1,"LR5", "I25_Denver")
results(LM5_denv)

LM6_camp<- Test_set(8,1,"LR6", "CAMP")
results(LM6_camp)
LM6_denv<- Test_set(8,1,"LR6", "I25_Denver")
results(LM6_denv)

LM7_camp<- Test_set(8,1,"LR7", "CAMP")
results(LM7_camp)
LM7_denv<- Test_set(8,1,"LR7", "I25_Denver")
results(LM7_denv)

#Mixed linear model results:
RE1_camp<- Test_set(3,1,"RE1", "CAMP")
results(RE1_camp)
RE1_denv<- Test_set(3,1,"RE1", "I25_Denver")
results(RE1_denv)

RE2_camp<- Test_set(8,1,"RE2", "CAMP")
results(RE2_camp)
RE2_denv<- Test_set(8,1,"RE2", "I25_Denver")
results(RE2_denv)

#Random forest results:
RF1_camp<- Test_set(3,1,"RF1", "CAMP")
results(RF1_camp)
RF1_denv<- Test_set(3,1,"RF1", "I25_Denver")
results(RF1_denv)

RF2_camp<- Test_set(3,2,"RF2", "CAMP")
results(RF2_camp)
RF2_denv<- Test_set(3,2,"RF2", "I25_Denver")
results(RF2_denv)

RF3_camp<- Test_set(7,2,"RF3", "CAMP")
results(RF3_camp)
RF3_denv<- Test_set(7,2,"RF3", "I25_Denver")
results(RF3_denv)

RF4_camp<- Test_set(7,2,"RF4", "CAMP")
results(RF4_camp)
RF4_denv<- Test_set(7,2,"RF4", "I25_Denver")
results(RF4_denv)



