library(randomForest)
library(e1071)
library(Metrics)
library(dLagM)
set.seed(2523)

grainYield <- read.csv("cropYieldData.csv", header = TRUE)

head(grainYield)

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
dataTrainOats <- grainYield[1:35, c(1, 3, 7, 10:13)]
dataTestOats <- grainYield[36:42, c(1, 3, 7, 10:13)]

# Run for tune up
set.seed(2523)
tuneOatsRF <- tune(randomForest, formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainOats[,-2], train.y = dataTrainOats[,2],
                 validation.x = dataTrainOats[,-2], validation.y = dataTestOats[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneOatsRF$best.parameters

RFOats <- fitRF(train = dataTrainOats, test = dataTestOats, dep = 2, formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"),
                ntree=tuneOatsRF$best.parameters$ntree, mtry=tuneOatsRF$best.parameters$mtry, nodesize = tuneOatsRF$best.parameters$nodesize)  
RFOats$MAE

set.seed(2523)
RFOats <- fitRF(train = dataTrainOats, test = dataTestOats, dep = 2, formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                ntree=5, mtry=101, nodesize = 10)  
RFOats$MAE

# Corn model
dataTrainCorn <- grainYield[1:35, c(1, 2, 6, 10:13)]
dataTestCorn <- grainYield[36:42, c(1, 2, 6, 10:13)]

# Run for tune up
set.seed(2523)
tuneCornRF <- tune(randomForest, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainCorn[,-2], train.y = dataTrainCorn[,2],
                 validation.x = dataTestCorn[,-2], validation.y = dataTestCorn[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneCornRF$best.parameters

RFCorn <- fitRF(train = dataTrainCorn, test = dataTestCorn, dep = 2, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"),
                ntree=tuneCornRF$best.parameters$ntree, mtry=tuneCornRF$best.parameters$mtry, nodesize = tuneCornRF$best.parameters$nodesize)  
RFCorn$MAE

set.seed(2523)
RFCorn <- fitRF(train = dataTrainCorn, test = dataTestCorn, dep = 2, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                ntree=10, mtry=41, nodesize = 2)  
RFCorn$MAE

# Rice model
dataTrainRice <- grainYield[1:35, c(1, 4, 8, 10:13)]
dataTestRice <- grainYield[36:42, c(1, 4, 8, 10:13)]

# Run for tune up
set.seed(2523)
tuneRiceRF <- tune(randomForest, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainRice[,-2], train.y = dataTrainRice[,2],
                 validation.x = dataTestRice[,-2], validation.y = dataTestRice[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneRiceRF$best.parameters

RFRice <- fitRF(train = dataTrainRice, test = dataTestRice, dep = 2, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"),
                ntree=tuneRiceRF$best.parameters$ntree, mtry=tuneRiceRF$best.parameters$mtry, nodesize = tuneRiceRF$best.parameters$nodesize) 
RFRice$MAE

set.seed(2523)
RFRice <- fitRF(train = dataTrainRice, test = dataTestRice, dep = 2, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                ntree=5, mtry=118, nodesize = 5)
RFRice$MAE

# Wheat model
dataTrainWheat <- grainYield[1:35, c(1, 5, 9, 10:13)]
dataTestWheat <- grainYield[36:42, c(1, 5, 9, 10:13)]

# Run for tune up
set.seed(2523)
tuneWheatRF <- tune(randomForest, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainWheat[,-2], train.y = dataTrainWheat[,2],
                 validation.x = dataTestWheat[,-2], validation.y = dataTestWheat[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(ntree=seq(5,200,5), mtry = seq(1,120,1), nodesize = seq(1,10,1)),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneWheatRF$best.parameters

set.seed(2523)
RFWheat <- fitRF(train = dataTrainWheat, test = dataTestWheat, dep = 2, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"),
                ntree=tuneWheatRF$best.parameters$ntree, mtry=tuneWheatRF$best.parameters$mtry, nodesize = tuneWheatRF$best.parameters$nodesize)  
RFWheat$MAE

set.seed(2523)
RFWheat<- fitRF(train = dataTrainWheat, test = dataTestWheat, dep = 2, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                ntree = 5, mtry = 19, nodesize = 10)
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




