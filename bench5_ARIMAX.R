library(TSA)
library(tseries)
library(forecast)
library(dLagM)

set.seed(2523)

grainYield <- read.csv("cropYieldData.csv", header = TRUE)
head(grainYield)

fitARIMAX <- function(data, order = c(0,0,0), colNames, print = TRUE){
  
  xSeriesAll = ts.union(diff(ts(data[,2])), diff(ts(data[,3])), diff(ts(data[,4])), diff(ts(data[,5])), diff(ts(data[,6])))
  
  xSeries = xSeriesAll[1:34,] #
  colnames(xSeries) <- colNames
  ySeries <- diff(ts(data[,1]))[1:34]
  out <- tryCatch(
  {
    ARIMAXmodel = Arima(ySeries, order=order, xreg=xSeries)
  
    xTestSeries = xSeriesAll[35:41,] #ts.union(diff(ts(data[36:42,2])), diff(ts(data[36:42,3])), diff(ts(data[36:42,4])), diff(ts(data[36:42,5])), diff(ts(data[36:42,6])))
    colnames(xTestSeries) <- colNames
    # preds <- predict(object = ARIMAXmodel , newxreg = xTestSeries , h = 8)
  
    frc <- forecast(object = ARIMAXmodel , xreg = xTestSeries )
    preds <-  diffinv(frc[[4]], differences = 1, xi = data[35,1])[2:8]
  }, error=function(e){print("Error")})
  
  test <- data[36:42,]
  
  if (print){
    print(data.frame(Predictions = preds, Observations = test[,1]))
  }
  
  MAE <- mean(abs(preds-test[,1])) 
  MSE <- mean((preds-test[,1])^2) 
  RMSE <- sqrt(MSE)
  MASE <- mase(test[,1],preds)
  
  return(list(preds = preds, MAE = MAE, MSE = MSE, RMSE = RMSE, MASE= MASE ))

}

# Oats model
dataOats <- grainYield[, c(3, 7, 10:13)]

ySeries <- ts(grainYield[,3])
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries),5,5)

# Run the search for orders
for ( i in 0:10){
  for ( j in 0:10){
    ARIMAXOats <- fitARIMAX(data = dataOats,order = c(i,0,j), colNames = c("Oats.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"),
                            print = FALSE)
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXOats$MAE,"\n")
  }
}

ARIMAXOats <- fitARIMAX(data = dataOats,  order = c(0,0,0), colNames = c("Oats.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), print = TRUE)
ARIMAXOats$MAE

# Corn model
dataCorn <- grainYield[, c(2, 6, 10:13)]

ySeries <- ts(grainYield[,6])
plot(ySeries)
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:10){
  for ( j in 0:10){
    ARIMAXCorn <- fitARIMAX(data = dataCorn, order = c(i,0,j), colNames = c("Corn.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), print = FALSE)
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXCorn$MAE,"\n")
  }
}

ARIMAXCorn <- fitARIMAX(data = dataCorn, colNames = c("Corn.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), order = c(10,0,8), print = TRUE)
ARIMAXCorn$MAE    

# Rice model
dataRice <- grainYield[, c(4, 8, 10:13)]

ySeries <- ts(grainYield[,7])
plot(ySeries)
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:10){
  for ( j in 0:10){
    ARIMAXRice <- fitARIMAX(data = dataRice, order = c(i,0,j), colNames = c("Rice.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), print = FALSE)
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXRice$MAE,"\n")
  }
}

ARIMAXRice <- fitARIMAX(data = dataRice, order = c(1,0,1), colNames = c("Rice.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), print = TRUE)
ARIMAXRice$MAE

# Wheat model
dataWheat <- grainYield[, c(5, 9, 10:13)]

ySeries <- ts(grainYield[,8])
plot(ySeries)
adf.test(diff(ySeries))
acf(diff(ySeries))
pacf(diff(ySeries))
eacf(diff(ySeries))

# Run the search for orders
for ( i in 0:10){
  for ( j in 0:10){
    ARIMAXWheat <- fitARIMAX(data = dataWheat, order = c(i,0,j),colNames = c("Wheat.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), print = FALSE)
    cat("i:", i, "; j:",j, "--> MAE:", ARIMAXWheat$MAE,"\n")
  }
}

ARIMAXWheat <- fitARIMAX(data = dataWheat, order = c(7,0,0),colNames = c("Wheat.area", "Fertilizer", "Temp.anomaly", "Rainfall.anomaly", "C02"), print = TRUE)
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
