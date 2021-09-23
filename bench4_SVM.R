library(e1071)
library(caret)
library(dLagM)
set.seed(2523)

grainYield <- read.csv("cropYieldData.csv", header = TRUE)
head(grainYield)

fitSVM <- function(train, test, formula, dep, print = TRUE, tune = FALSE, kernel = "radial", epsilon, cost, gamma){

  set.seed(2523)
  SVMmodel <- svm(formula = formula, data = train,  kernel = kernel , epsilon = epsilon, cost = cost, gamma = gamma, coef.0 = 0)

  preds <- predict(SVMmodel, test)
  preds
  
  MAE <- mean(abs(preds-test[,dep])) 
  MAE
  MSE <- mean((preds-test[,dep])^2) 
  RMSE <- sqrt(MSE)
  MASE <- mase(test[,dep],preds)

  if (print){
    print(data.frame(Predictions = preds, Observations = test[,dep]))
  }
  
  return(list(preds = preds, MAE = MAE, MSE = MSE, RMSE = RMSE, MASE= MASE ))
}

# Oats model
dataTrainOats <- grainYield[1:35, c(1, 3, 7, 10:13)]
dataTestOats <- grainYield[36:42, c(1, 3, 7, 10:13)]

# Run for tune up
set.seed(2523)
tuneOatsSVM <- tune(svm, formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainOats[,-2], train.y = dataTrainOats[,2],
                 validation.x = dataTestOats[,-2], validation.y = dataTestOats[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,1,0.01), 
                             kernel = c('linear','polynomial','radial','sigmoid')),
                 error.fun = function(x,y){mean(abs(x-y))})
tuneOatsSVM$best.model

set.seed(2523)
SVMOats <- fitSVM(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                  dep = 2, print = TRUE, kernel = "radial", epsilon = tuneOatsSVM$best.model$epsilon, cost = tuneOatsSVM$best.model$cost, 
                  gamma = tuneOatsSVM$best.model$gamma)
SVMOats$MAE

SVMOats <- fitSVM(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = 2, print = TRUE, kernel = "sigmoid", epsilon = 0.3, cost = 54, 
                  gamma = 0.05) 
SVMOats$MAE

# Corn model
dataTrainCorn <- grainYield[1:35, c(1, 2, 6, 10:13)]
dataTestCorn <- grainYield[36:42, c(1, 2, 6, 10:13)]

# Run for tune up
set.seed(2523)
tuneCornSVM <- tune(svm, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                train.x = dataTrainCorn[,-2], train.y = dataTrainCorn[,2],
                validation.x = dataTestCorn[,-2], validation.y = dataTestCorn[,2], 
                tunecontrol = tune.control(sampling = "fix"),
                ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,1,0.01), 
                            kernel = c('linear','polynomial','radial','sigmoid')),
                error.fun = function(x,y){mean(abs(x-y))})
tuneCornSVM$best.model

set.seed(2523)
SVMCorn <- fitSVM(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                  dep = 2, print = TRUE, kernel = "radial", epsilon = tuneCornSVM$best.model$epsilon, cost = tuneCornSVM$best.model$cost, 
                  gamma = tuneCornSVM$best.model$gamma)
SVMCorn$MAE

set.seed(2523)
SVMCorn <- fitSVM(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = 2, print = TRUE, kernel = "radial", epsilon = 0.5, cost = 16, 
                  gamma = 0.02)
SVMCorn$MAE

# Rice model
dataTrainRice <- grainYield[1:35, c(1, 4, 8, 10:13)]
dataTestRice <- grainYield[36:42, c(1, 4, 8, 10:13)]

# Run for tune up
set.seed(2523)
tuneRiceSVM <- tune(svm, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainRice[,-2], train.y = dataTrainRice[,2],
                 validation.x = dataTestRice[,-2], validation.y = dataTestRice[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,1,0.01), 
                             kernel = c('linear','polynomial','radial','sigmoid')),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneRiceSVM$best.model

set.seed(2523)
SVMRice <- fitSVM(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                  dep = 2, print = TRUE, kernel = "sigmoid", epsilon = tuneRiceSVM$best.model$epsilon, cost = tuneRiceSVM$best.model$cost, 
                  gamma = tuneRiceSVM$best.model$gamma)
SVMRice$MAE

set.seed(2523)
SVMRice <- fitSVM(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = 2, print = TRUE, kernel = "sigmoid", epsilon = 0.9, cost = 14, 
                  gamma = 0.08)
SVMRice$MAE

# Wheat model
dataTrainWheat <- grainYield[1:35, c(1, 5, 9, 10:13)]
dataTestWheat <- grainYield[36:42, c(1, 5, 9, 10:13)]

# Run for tune up
set.seed(2523)
tuneWheatSVM <- tune(svm, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                 train.x = dataTrainWheat[,-2], train.y = dataTrainWheat[,2],
                 validation.x = dataTestWheat[,-2], validation.y = dataTestWheat[,2], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,0.01,1), 
                             kernel = c('linear','polynomial','radial','sigmoid')),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneWheatSVM$best.model

set.seed(2523)
SVMWheat <- fitSVM(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + CO2"), 
                  dep = 2, print = TRUE, kernel = "sigmoid", epsilon = tuneWheatSVM$best.model$epsilon, cost = tuneWheatSVM$best.model$cost, 
                  gamma = tuneWheatSVM$best.model$gamma)
SVMWheat$MAE

set.seed(2523)
SVMWheat <- fitSVM(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                   dep = 2, print = TRUE, kernel = "linear", epsilon = 0.9, cost = 34, 
                   gamma = 0.01)
SVMWheat$MAE

minMAE_SVM <- c(SVMOats$MAE, SVMCorn$MAE, SVMRice$MAE, SVMWheat$MAE)
dfResultsSVM <- data.frame(MAE_SVM = minMAE_SVM)  
rownames(dfResultsSVM) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsSVM

dfResults5 <- data.frame(dfResults4,MAE_SVM = minMAE_SVM)
dfResults5

minRMSE_SVM <- c(SVMOats$RMSE, SVMCorn$RMSE, SVMRice$RMSE, SVMWheat$RMSE)
dfResultsRMSESVM <- data.frame(RMSE_SVM = minRMSE_SVM)  
rownames(dfResultsRMSESVM) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsRMSESVM

dfResults5RMSE <- data.frame(dfResults4RMSE,RMSE_SVM = minRMSE_SVM)
dfResults5RMSE

minMASE_SVM <- c(SVMOats$MASE, SVMCorn$MASE, SVMRice$MASE, SVMWheat$MASE)
dfResultsMASESVM <- data.frame(MASE_SVM = minMASE_SVM)  
rownames(dfResultsMASESVM) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsMASESVM

dfResults5MASE <- data.frame(dfResults4MASE,MASE_SVM = minMASE_SVM)
dfResults5MASE

hyperParameterTuningSVM <- rbind(tuneOatsSVM$best.parameters, tuneCornSVM$best.parameters, tuneRiceSVM$best.parameters, tuneWheatSVM$best.parameters)
rownames(hyperParameterTuningSVM) <- c("Oats","Corn","Rice","Wheat")

