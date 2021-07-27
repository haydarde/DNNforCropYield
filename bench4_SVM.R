library(e1071)
library(caret)
library(dLagM)
set.seed(2523)

data(grainProduction)
grainProduction
head(grainProduction)

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
dataTrainOats <- grainProduction[1:50, c(1:5)]
dataTestOats <- grainProduction[51:58, c(1:5)]

# Run for tune up
set.seed(2523)
tuneOatsSVM <- tune(svm, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainOats[,1:4], train.y = dataTrainOats[,5],
                 validation.x = dataTestOats[,1:4], validation.y = dataTestOats[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,1,0.01), 
                             kernel = c('linear','polynomial','radial','sigmoid')),
                 error.fun = function(x,y){mean(abs(x-y))})
tuneOatsSVM$best.model

set.seed(2523)
SVMOats <- fitSVM(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "radial", epsilon = tuneOatsSVM$best.model$epsilon, cost = tuneOatsSVM$best.model$cost, 
                  gamma = tuneOatsSVM$best.model$gamma)
SVMOats$MAE

SVMOats <- fitSVM(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "radial", epsilon = 0.3, cost = 15, 
                  gamma = 0.04) 
SVMOats$MAE

# Corn model
dataTrainCorn <- grainProduction[1:50, c(1:4,6)]
dataTestCorn <- grainProduction[51:58, c(1:4,6)]

# Run for tune up
set.seed(2523)
tuneCornSVM <- tune(svm, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                train.x = dataTrainCorn[,1:4], train.y = dataTrainCorn[,5],
                validation.x = dataTestCorn[,1:4], validation.y = dataTestCorn[,5], 
                tunecontrol = tune.control(sampling = "fix"),
                ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,1,0.01), 
                            kernel = c('linear','polynomial','radial','sigmoid')),
                error.fun = function(x,y){mean(abs(x-y))})
tuneCornSVM$best.model

set.seed(2523)
SVMCorn <- fitSVM(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "radial", epsilon = tuneCornSVM$best.model$epsilon, cost = tuneCornSVM$best.model$cost, 
                  gamma = tuneCornSVM$best.model$gamma)
SVMCorn$MAE

set.seed(2523)
SVMCorn <- fitSVM(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "radial", epsilon = 0, cost = 105, 
                  gamma = 0.07)
SVMCorn$MAE

# Rice model
dataTrainRice <- grainProduction[1:50, c(1:4,7)]
dataTestRice <- grainProduction[51:58, c(1:4,7)]

# Run for tune up
set.seed(2523)
tuneRiceSVM <- tune(svm, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainRice[,1:4], train.y = dataTrainRice[,5],
                 validation.x = dataTestRice[,1:4], validation.y = dataTestRice[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,1,0.01), 
                             kernel = c('linear','polynomial','radial','sigmoid')),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneRiceSVM$best.model

set.seed(2523)
SVMRice <- fitSVM(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "sigmoid", epsilon = tuneRiceSVM$best.model$epsilon, cost = tuneRiceSVM$best.model$cost, 
                  gamma = tuneRiceSVM$best.model$gamma)
SVMRice$MAE

set.seed(2523)
SVMRice <- fitSVM(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "sigmoid", epsilon = 0.3, cost = 10, 
                  gamma = 0.06)
SVMRice$MAE

# Wheat model
dataTrainWheat <- grainProduction[1:50, c(1:4,8)]
dataTestWheat <- grainProduction[51:58, c(1:4,8)]

# Run for tune up
set.seed(2523)
tuneWheatSVM <- tune(svm, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                 train.x = dataTrainWheat[,1:4], train.y = dataTrainWheat[,5],
                 validation.x = dataTestWheat[,1:4], validation.y = dataTestWheat[,5], 
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges=list(epsilon=seq(0,1,0.1), cost=10:105, gamma = seq(0.01,0.01,1), 
                             kernel = c('linear','polynomial','radial','sigmoid')),
                 error.fun = function(x,y){mean(abs(x-y))})

tuneWheatSVM$best.model

set.seed(2523)
SVMWheat <- fitSVM(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, kernel = "sigmoid", epsilon = tuneWheatSVM$best.model$epsilon, cost = tuneWheatSVM$best.model$cost, 
                  gamma = tuneWheatSVM$best.model$gamma)
SVMWheat$MAE

set.seed(2523)
SVMWheat <- fitSVM(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                   dep = 5, print = TRUE, kernel = "sigmoid", epsilon = 0.4, cost = 10, 
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

