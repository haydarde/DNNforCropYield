library(randomForest)
library(e1071)
library(Metrics)
library(dLagM)
set.seed(2523)

data(grainProduction)
grainProduction
head(grainProduction)

fitRF <- function(train, test, dep, formula, print = TRUE, ntree=200, mtry=7, nodesize = 1){
  set.seed(2523)
  RFmodel <- randomForest(formula = formula, data = train, ntree=ntree, mtry=mtry, nodesize = nodesize, importance = TRUE)
  plot(RFmodel)
  
  preds <- predict(RFmodel, test)
  
  if (print){
    print(data.frame(Predictions = preds, Observations = test[,dep]))
  }
  
  MAE <- mean(abs(preds-test[,dep])) 
  MSE <- mean((preds-test[,dep])^2) 
  RMSE <- sqrt(MSE)
  MASE <- mase(test[,dep],preds)
  
  return(list(preds = preds, MAE = MAE, MSE = MSE, RMSE = RMSE, MASE= MASE ))

}

# Oats model
dataTrainOats <- grainProduction[1:50, c(1:5)]
dataTestOats <- grainProduction[51:58, c(1:5)]

# Run for tune up
set.seed(2523)
tuneOatsRF <- tune(randomForest, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainOats[,1:4], train.y = dataTrainOats[,5],
                 validation.x = dataTestOats[,1:4], validation.y = dataTestOats[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneOatsRF$best.parameters

RFOats <- fitRF(train = dataTrainOats, test = dataTestOats, dep = 5, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=tuneOatsRF$best.parameters$ntree, mtry=tuneOatsRF$best.parameters$mtry, nodesize = tuneOatsRF$best.parameters$nodesize)  
RFOats$MAE

set.seed(2523)
RFOats <- fitRF(train = dataTrainOats, test = dataTestOats, dep = 5, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=5, mtry=27, nodesize = 1)  

# Corn model
dataTrainCorn <- grainProduction[1:50, c(1:4,6)]
dataTestCorn <- grainProduction[51:58, c(1:4,6)]

# Run for tune up
set.seed(2523)
tuneCornRF <- tune(randomForest, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainCorn[,1:4], train.y = dataTrainCorn[,5],
                 validation.x = dataTestCorn[,1:4], validation.y = dataTestCorn[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneCornRF$best.parameters

RFCorn <- fitRF(train = dataTrainCorn, test = dataTestCorn, dep = 5, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=tuneCornRF$best.parameters$ntree, mtry=tuneCornRF$best.parameters$mtry, nodesize = tuneCornRF$best.parameters$nodesize)  
RFCorn$MAE

set.seed(2523)
RFCorn <- fitRF(train = dataTrainCorn, test = dataTestCorn, dep = 5, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=5, mtry=66, nodesize = 1)  

# Rice model
dataTrainRice <- grainProduction[1:50, c(1:4,7)]
dataTestRice <- grainProduction[51:58, c(1:4,7)]

# Run for tune up
set.seed(2523)
tuneRiceRF <- tune(randomForest, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainRice[,1:4], train.y = dataTrainRice[,5],
                 validation.x = dataTestRice[,1:4], validation.y = dataTestRice[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneRiceRF$best.parameters

RFRice <- fitRF(train = dataTrainRice, test = dataTestRice, dep = 5, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=tuneRiceRF$best.parameters$ntree, mtry=tuneRiceRF$best.parameters$mtry, nodesize = tuneRiceRF$best.parameters$nodesize) 
RFRice$MAE

set.seed(2523)
RFRice <- fitRF(train = dataTrainRice, test = dataTestRice, dep = 5, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=5, mtry=28, nodesize = 1)

# Wheat model
dataTrainWheat <- grainProduction[1:50, c(1:4,8)]
dataTestWheat <- grainProduction[51:58, c(1:4,8)]

# Run for tune up
set.seed(2523)
tuneWheatRF <- tune(randomForest, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainWheat[,1:4], train.y = dataTrainWheat[,5],
                 validation.x = dataTestWheat[,1:4], validation.y = dataTestWheat[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneWheatRF$best.parameters

set.seed(2523)
RFWheat <- fitRF(train = dataTrainWheat, test = dataTestWheat, dep = 5, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                ntree=tuneWheatRF$best.parameters$ntree, mtry=tuneWheatRF$best.parameters$mtry, nodesize = tuneWheatRF$best.parameters$nodesize)  
RFWheat$MAE

set.seed(2523)
RFWheat<- fitRF(train = dataTrainWheat, test = dataTestWheat, dep = 5, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                ntree = 5, mtry = 2, nodesize = 2)
RFWheat$MAE

minMAE <- c(RFOats$MAE, RFCorn$MAE, RFRice$MAE, RFWheat$MAE)
dfResultsRF <- data.frame(MAE_RF = minMAE)  
rownames(dfResultsRF) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsRF

dfResults2 <- data.frame(dfResultsIndv,MAE_RF = minMAE)
dfResults2 <- dfResultsRF

minRMSE <- c(RFOats$RMSE, RFCorn$RMSE, RFRice$RMSE, RFWheat$RMSE)
dfResultsRMSERF <- data.frame(RMSE_RF = minRMSE)  
rownames(dfResultsRMSERF) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsRMSERF

dfResults2RMSE <- data.frame(dfResultsIndvRMSE,RMSE_RF = minRMSE)
dfResults2RMSE <- dfResultsRMSERF

minMASE <- c(RFOats$MASE, RFCorn$MASE, RFRice$MASE, RFWheat$MASE)
dfResultsMASERF <- data.frame(MASE_RF = minMASE)  
rownames(dfResultsMASERF) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsMASERF

dfResults2MASE <- data.frame(dfResultsIndvMASE,MASE_RF = minMASE)
dfResults2MASE <- dfResultsMASERF




