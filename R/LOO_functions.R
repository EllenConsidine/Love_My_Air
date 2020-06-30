#Leave out different sensors for testing:

LOO_linear<- function(type, test_pos, name){
  print(type)
  if(type == "LR1"){
    LM<- lm(AirNow ~ PM25, training)
  }else if(type == "LR2"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity, training)
  }else if(type == "LR3"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + Near_hwy, training)
  }else if(type == "LR4"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + Aroad_500, training)
  }else if(type == "LR5"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + Weekend + cos_Time, training)
  }else if(type == "LR6"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + Weekend + cos_Time
            + Near_hwy, training)
  }else if(type == "LR7"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + cos_Month + Weekend + cos_Time
            + Aroad_500, training)
  }
  
  #Testing:
  preds<- predict(LM, testing)
  obs<- testing$AirNow
  
  print(round(sqrt(mean((preds-obs)^2)),3))
  print(round(cor(preds, obs)^2,3))
}

LOO_hlm<- function(test_pos, name){
  print(type)
  print(paste("Leaving out", name))
  training<- DATA[-test_pos,]
  testing<- DATA[test_pos,]
  
  if(type == "RE1"){
    RE<- lme(AirNow ~ PM25 + Temperature + Humidity, 
             data = DATA, random = ~1| ID)
  }else if(type == "RE2"){
    RE<- lme(AirNow ~ PM25 + Temperature + Humidity, 
             data = DATA, random = ~1 + PM25| ID)
  }else if(type == "RE3"){
    RE<- lme(AirNow ~ PM25 + Temperature + Humidity + cos_Month + cos_Time + Weekend, 
             data = DATA, random = ~1 + PM25| ID)
  }
  
  performance(RE, testing) # function from 2-Archived_final_training.R
}

run_LOO<- function(type, train_pos, name){
  print(type)
  print(paste("Leaving out", name))
  
  if(type == "RF1"){
    vars<- c("PM25", "Temperature", "Humidity", "AirNow")
    Mtry<- 3
  }else if(type == "RF2"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow")
    Mtry<- 4
  }else if(type == "RF3"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Near_hwy")
    Mtry<- 4
  }else if(type == "RF4"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Aroad_500")
    Mtry<- 4
  }else if(type == "RF5"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Near_hwy", "Hwy_rush")
    Mtry<- 7
  }else if(type == "RF6"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100", "Lroad_50")
    Mtry<- 7
  }else if(type == "RF7"){
    vars<- c("cos_Time", "cos_Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100", "Lroad_50",
             "Hwy_rush", "Rush_hour")
    Mtry<- 10
  }
  
  if(name != "None"){
    training<- dataset[train_pos,vars]
    testing<- dataset[-train_pos,vars]
  }else{
    training<- dataset[train_pos,vars]
  }
  
  
  myControl<- trainControl(number = 1, savePredictions = "final", 
                           verboseIter = FALSE)
  my_model <- train(AirNow ~ .,
                    data = training, trControl = myControl,
                    method = "ranger",
                    tuneGrid = expand.grid( .mtry = Mtry,
                                            .splitrule = "extratrees",
                                            .min.node.size = 5 ),
                    metric = "RMSE", importance = "permutation")

  saveRDS(my_model, paste0("Models/Archived_", type, "_", name, ".rds"))

  print(my_model$results)
  
  if(name != "None"){
    test_preds <- data.frame(predict(my_model, testing[,-(which(names(testing)== "AirNow"))])) #sanity check
    compare<- cbind(test_preds, testing[,"AirNow"])
    resids<- (compare[,1] - compare[,2])
    print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
    print(paste("Testing set RMSE =", round(sqrt(mean(resids^2)), digits = 4)))
  }
  
}

