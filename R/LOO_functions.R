#Leave out different sensors for testing:

LOO_linear<- function(type, test_pos, name){
  print(type)
  print(paste("Leaving out", name))
  training<- DATA[-test_pos,]
  testing<- DATA[test_pos,]
  
  if(type == "SLR_1"){
    LM<- lm(AirNow ~ PM25, training)
  }else if(type = "MLR_1"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity, training)
  }else if(type == "MLR_2"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + Month + Weekend + Time, training)
  }else if(type == "MLR_3"){
    LM<- lm(AirNow ~ PM25 + Temperature + Humidity + Month + Weekend + Time
            + Aroad_500, training)
  }
  
  # #Training:
  # preds<- LM$fitted.values
  # obs<- training$AirNow
  
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
  
  if(type == "RE_1"){
    RE<- lme(AirNow ~ PM25 + Temperature + Humidity, 
             data = DATA, random = ~1| ID)
  }else if(type == "RE_2"){
    RE<- lme(AirNow ~ PM25 + Temperature + Humidity, 
             data = DATA, random = ~1 + PM25| ID)
  }else if(type == "RE_3"){
    RE<- lme(AirNow ~ PM25 + Temperature + Humidity + Month + Time + Weekend, 
             data = DATA, random = ~1 + PM25| ID)
  }
  
  performance(RE, testing) # function from No-outliers_final_training.R
}

run_LOO<- function(type, train_pos, name){
  print(type)
  print(paste("Leaving out", name))
  if(type == "all-vars"){
    vars<- names(dataset)
    Mtry<- 10
  }else if(type == "five-roads"){
    vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Aroad_500", "Aroad_50", "Lroad_250", "Lroad_100", "Lroad_50")
    Mtry<- 10
  }else if(type == "one-road"){
    vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow",
             "Aroad_500")
    Mtry<- 4
  }else{ # type == no-roads OR no-folds
    vars<- c("Time", "Month", "Weekend", "PM25", "Temperature", "Humidity", "AirNow")
    Mtry<- 4
  }

  training<- dataset[train_pos,vars]
  testing<- dataset[-train_pos,vars]
  
  cluster <- makeCluster(detectCores() - 2)
  registerDoParallel(cluster)
  
  if(type == "no-folds"){
    myControl<- trainControl(number = 1, savePredictions = "final", 
                             verboseIter = FALSE)
    registerDoSEQ()
    
  }else{
    #Create resampling folds
    IND<- createResample(training[,"AirNow"], 10)
    
    #Set up control object
    myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                             savePredictions = "final", index = IND, verboseIter = TRUE,
                             allowParallel = TRUE)
  }

  my_model <- train(AirNow ~ .,
                    data = training, trControl = myControl,
                    method = "ranger",
                    tuneGrid = expand.grid( .mtry = Mtry,
                                            .splitrule = "extratrees",
                                            .min.node.size = 5 ),
                    metric = "RMSE", importance = "permutation")

  saveRDS(my_model, paste0("~/No-outliers_", type, "_", name, ".rds"))

  stopCluster(cluster)

  print(my_model$results)
  
  test_preds <- data.frame(predict(my_model, testing[,-(which(names(testing)== "AirNow"))])) #sanity check
  compare<- cbind(test_preds, testing[,"AirNow"])
  resids<- (compare[,1] - compare[,2])
  print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
  print(paste("Testing set RMSE =", round(sqrt(mean(resids^2)), digits = 4)))
}

