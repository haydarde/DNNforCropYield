library(dLagM)
set.seed(2523)

data(grainProduction)
grainProduction
head(grainProduction)

fitARDL <- function(train, test, formula, dep, print = TRUE, p, q, remove = NULL){
  
  
  ARDLmodel <- ardlDlm(formula = formula, data = train , p = p , q = q, remove = remove )
  summary(ARDLmodel)
  
  preds <- dLagM::forecast(model = ARDLmodel , x = t(as.matrix(test[,(2:(dep-1))])) , h = 8 , interval = FALSE, level = 0.95 , nSim = 500)$forecasts
  
  if (print){
    summary(ARDLmodel)
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

set.seed(2523)
rem.p = list(Land.OceanTempIndex.C. = c(1) , CO2.Tons. = c(1))
remove = list(p = rem.p)
remove

ARDLOats <- fitARDL(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.1Mt. ~ CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                    dep = 5, print = TRUE, p = 1, q = 1 ,  remove = remove )

ARDLOats$MAE

# Corn model
dataTrainCorn <- grainProduction[1:50, c(1:4,6)]
dataTestCorn <- grainProduction[51:58, c(1:4,6)]

set.seed(2523)
rem.p = list(Land.OceanTempIndex.C. = c(1) , CO2.Tons. = c(1))
remove = list(p = rem.p)
remove

ARDLCorn <- fitARDL(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.1Mt. ~ CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                    dep = 5, print = TRUE, p = 1, q = 2,  remove = remove )

ARDLCorn$MAE

# Rice model
dataTrainRice <- grainProduction[1:50, c(1:4,7)]
dataTestRice <- grainProduction[51:58, c(1:4,7)]

set.seed(2523)
rem.p = list(Land.OceanTempIndex.C. = c(1) , CO2.Tons. = c(0))#, Cropland.1Mha. = c(1))
remove = list(p = rem.p)
remove

ARDLRice <- fitARDL(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.1Mt. ~ CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                    dep = 5, print = TRUE, p = 1, q = 3,  remove = remove )

ARDLRice$MAE

# Wheat model
dataTrainWheat <- grainProduction[1:50, c(1:4,8)]
dataTestWheat <- grainProduction[51:58, c(1:4,8)]

set.seed(2523)
rem.p = list(Land.OceanTempIndex.C. = c(0,2,3) , CO2.Tons. = c(0,2,3), Cropland.1Mha. = c(0,2))
remove = list(p = rem.p)
remove

ARDLWheat <- fitARDL(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.1Mt. ~ CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                    dep = 5, print = TRUE, p = 3, q = 2,  remove = remove )

ARDLWheat$MAE

minMAE_ARDL <- c(ARDLOats$MAE, ARDLCorn$MAE, ARDLRice$MAE, ARDLWheat$MAE)
dfResultsARDL <- data.frame(MAE_ARDL = minMAE_ARDL)  
rownames(dfResultsARDL) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsARDL

dfResults4 <- data.frame(dfResults3,MAE_ARDL = minMAE_ARDL)
dfResults4

minRMSE_ARDL <- c(ARDLOats$RMSE, ARDLCorn$RMSE, ARDLRice$RMSE, ARDLWheat$RMSE)
dfResultsRMSEARDL <- data.frame(MAE_ARDL = minRMSE_ARDL)  
rownames(dfResultsRMSEARDL) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsRMSEARDL

dfResults4RMSE <- data.frame(dfResults3RMSE, RMSE_ARDL = minRMSE_ARDL)
dfResults4RMSE

minMASE_ARDL <- c(ARDLOats$MASE, ARDLCorn$MASE, ARDLRice$MASE, ARDLWheat$MASE)
dfResultsMASEARDL <- data.frame(MAE_ARDL = minMASE_ARDL)  
rownames(dfResultsMASEARDL) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsMASEARDL

dfResults4MASE <- data.frame(dfResults3MASE, MASE_ARDL = minMASE_ARDL)
dfResults4MASE
