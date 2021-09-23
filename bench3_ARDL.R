library(dLagM)
set.seed(2523)

grainYield <- read.csv("cropYieldData.csv", header = TRUE)
head(grainYield)

fitARDL <- function(train, test, formula, print = TRUE, p, q, remove = NULL){
  
  
  ARDLmodel <- ardlDlm(formula = formula, data = train , p = p , q = q, remove = remove )
  summary(ARDLmodel)
  
  preds <- dLagM::forecast(model = ARDLmodel , x = t(as.matrix(test[,3:7])) , h = 7 , interval = FALSE, level = 0.95 , nSim = 500)$forecasts
  
  if (print){
    summary(ARDLmodel)
    print(data.frame(Predictions = preds, Observations = test[,2]))
  }
  
  MAE <- mean(abs(preds-test[,2])) 
  MSE <- mean((preds-test[,2])^2) 
  RMSE <- sqrt(MSE)
  MASE <- mase(test[,2],preds)
  
  return(list(preds = preds, MAE = MAE, MSE = MSE, RMSE = RMSE, MASE= MASE ))
  
}

# Oats model
dataTrainOats <- grainYield[1:35, c(1, 3, 7, 10:13)]
dataTestOats <- grainYield[36:42, c(1, 3, 7, 10:13)]

set.seed(2523)
rem.p = list(Oats.area = c(0) , Fertilizer = c(1), Temp.anomaly = c(1), Rainfall.anomaly = c(1), C02 = c(1))
remove = list(p = rem.p,q=c(1))
remove

ARDLOats <- fitARDL(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.yield ~ Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                    print = TRUE, p = 1, q = 1 ,  remove = remove )

ARDLOats$MAE

# Corn model
dataTrainCorn <- grainYield[1:35, c(1, 2, 6, 10:13)]
dataTestCorn <- grainYield[36:42, c(1, 2, 6, 10:13)]

set.seed(2523)
rem.p = list(Corn.area = c(1) , Fertilizer = c(1), Temp.anomaly = c(1), Rainfall.anomaly = c(1), C02 = c(1))
remove = list(p = rem.p)#, q=c(1))
remove

ARDLCorn <- fitARDL(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.yield ~ Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                    print = TRUE, p = 1, q = 1,  remove = remove )

ARDLCorn$MAE

# Rice model
dataTrainRice <- grainYield[1:35, c(1, 4, 8, 10:13)]
dataTestRice <- grainYield[36:42, c(1, 4, 8, 10:13)]

set.seed(2523)
rem.p = list(Rice.area = c(0) , Fertilizer = c(1), Temp.anomaly = c(1), Rainfall.anomaly = c(1), C02 = c(0))
remove = list(p = rem.p, q=c(1))
remove

ARDLRice <- fitARDL(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.yield ~ Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                    print = TRUE, p = 1, q = 1,  remove = remove )

ARDLRice$MAE

# Wheat model
dataTrainWheat <- grainYield[1:35, c(1, 5, 9, 10:13)]
dataTestWheat <- grainYield[36:42, c(1, 5, 9, 10:13)]

set.seed(2523)
rem.p = list(Wheat.area = c(0) , Fertilizer = c(1), Temp.anomaly = c(1), Rainfall.anomaly = c(1), C02 = c(0))
remove = list(p = rem.p)#, q=c(1))
remove

ARDLWheat <- fitARDL(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.yield ~ Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                    print = TRUE, p = 1, q = 1,  remove = remove )

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
