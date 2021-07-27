library(TSA)
library(tseries)
library(forecast)
library(dLagM)

set.seed(2523)

data(grainProduction)
grainProduction
head(grainProduction)

fitARIMAX <- function(train, test, data, dep, print = TRUE, order = c(0,0,0)){
  
  xSeries = ts.union(diff(ts(train[,2])), diff(ts(train[,3])), diff(ts(train[,4])))
  colnames(xSeries) <- c("CO2.Tons.", "Land.OceanTempIndex.C.", "Cropland.1Mha.")
  ySeries <- diff(ts(train[,5]))
  ARIMAXmodel = Arima(ySeries, order=order, xreg=xSeries)

  xTestSeries = ts.union(diff(ts(data[50:58,2])), diff(ts(data[50:58,3])), diff(ts(data[50:58,4])))
  colnames(xTestSeries) <- c("CO2.Tons.", "Land.OceanTempIndex.C.", "Cropland.1Mha.")
  # preds <- predict(object = ARIMAXmodel , newxreg = xTestSeries , h = 8)
  
  frc <- forecast(object = ARIMAXmodel , xreg = xTestSeries , h = 8)
  preds <-  diffinv(frc[[4]], differences = 1, xi = train[50,5])[2:9]

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

ySeries <- ts(grainProduction[,5])
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:5){
  for ( j in 0:5){
    ARIMAXOats <- fitARIMAX(train = dataTrainOats, test = dataTestOats, data = grainProduction, dep = 5, print = FALSE, order = c(i,0,j))
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXOats$MAE,"\n")
  }
}

ARIMAXOats <- fitARIMAX(train = dataTrainOats, test = dataTestOats, data = grainProduction, dep = 5, print = TRUE, order = c(3,0,5))
ARIMAXOats$MAE

# Corn model
dataTrainCorn <- grainProduction[1:50, c(1:4,6)]
dataTestCorn <- grainProduction[51:58, c(1:4,6)]

ySeries <- ts(grainProduction[,6])
plot(ySeries)
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:5){
  for ( j in 0:5){
    ARIMAXCorn <- fitARIMAX(train = dataTrainCorn, test = dataTestCorn, data = grainProduction, dep = 5, print = FALSE, order = c(i,0,j))
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXCorn$MAE,"\n")
  }
}

ARIMAXCorn <- fitARIMAX(train = dataTrainCorn, test = dataTestCorn, data = grainProduction, dep = 5, print = TRUE, order = c(1,0,5))
ARIMAXCorn$MAE    

# Rice model
dataTrainRice <- grainProduction[1:50, c(1:4,7)]
dataTestRice <- grainProduction[51:58, c(1:4,7)]

ySeries <- ts(grainProduction[,7])
plot(ySeries)
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:5){
  for ( j in 0:5){
    ARIMAXRice <- fitARIMAX(train = dataTrainRice, test = dataTestRice, data = grainProduction, dep = 5, print = FALSE, order = c(i,0,j))
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXRice$MAE,"\n")
  }
}

ARIMAXRice <- fitARIMAX(train = dataTrainRice, test = dataTestRice, data = grainProduction, dep = 5, print = TRUE, order = c(0,0,0))
ARIMAXRice$MAE

# Wheat model
dataTrainWheat <- grainProduction[1:50, c(1:4,8)]
dataTestWheat <- grainProduction[51:58, c(1:4,8)]

ySeries <- ts(grainProduction[,8])
plot(ySeries)
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:5){
  for ( j in 0:5){
    ARIMAXWheat <- fitARIMAX(train = dataTrainWheat, test = dataTestWheat, data = grainProduction, dep = 5, print = FALSE, order = c(i,0,j))
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXWheat$MAE,"\n")
  }
}

ARIMAXWheat <- fitARIMAX(train = dataTrainWheat, test = dataTestWheat, data = grainProduction, dep = 5, print = TRUE, order = c(1,0,1))
ARIMAXWheat$MAE

minMAE_ARIMAX <- c(ARIMAXOats$MAE, ARIMAXCorn$MAE, ARIMAXRice$MAE, ARIMAXWheat$MAE)
dfResultsARIMAX <- data.frame(MAE_ARIMAX = minMAE_ARIMAX)  
rownames(dfResultsARIMAX) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsARIMAX

dfResults6 <- data.frame(dfResults5,MAE_ARIMAX = minMAE_ARIMAX)
dfResults6
# write.csv(dfResults6,"dfResults6.csv")  

minRMSE_ARIMAX <- c(ARIMAXOats$RMSE, ARIMAXCorn$RMSE, ARIMAXRice$RMSE, ARIMAXWheat$RMSE)
dfResultsARIMAX <- data.frame(RMSE_ARIMAX = minRMSE_ARIMAX)  
rownames(dfResultsARIMAX) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsARIMAX

dfResults6RMSE <- data.frame(dfResults5RMSE,RMSE_ARIMAX = minRMSE_ARIMAX)
dfResults6RMSE
# write.csv(dfResults6RMSE,"dfResults6RMSE.csv") 

minMASE_ARIMAX <- c(ARIMAXOats$MASE, ARIMAXCorn$MASE, ARIMAXRice$MASE, ARIMAXWheat$MASE)
dfResultsARIMAX <- data.frame(MASE_ARIMAX = minMASE_ARIMAX)  
rownames(dfResultsARIMAX) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsARIMAX

dfResults6MASE <- data.frame(dfResults5MASE,MASE_ARIMAX = minMASE_ARIMAX)
dfResults6MASE

# write.csv(dfResults6MASE,"dfResults6MASE.csv")  
