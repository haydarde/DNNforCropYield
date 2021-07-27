library(tidyverse)
library(neuralnet)
library(GGally)
library(forecast)
library(tictoc)
library(dLagM)

set.seed(2523)

data(grainProduction)
grainProduction
head(grainProduction)

tuneANN <- function(train, test, formula, min, max, dep){
  flags = list(
    hidden1 = c(1, 2, 3, 4, 5), # Small dataset - big number of hidden nodes wouldn't be suitable
    hidden2 = c(1, 2, 3, 4, 5),
    hidden3 = c(1, 2, 3, 4, 5),
    activation = c(1, 2, 3, 4),
    rep = c(5,10,20,40)
  )
  
  functions <- list()
  functions[[1]] <- function(x){log(exp(x) + 1)}  # softplus
  functions[[2]] <- function(x){x / (abs(x) + 1)} # softsign
  functions[[3]] <- "tanh" #function(x){((exp(x) - exp(-x))/(exp(x) + exp(-x)))} # tanh
  functions[[4]] <- "logistic"
  
  
  dfOverall <- data.frame()
 
  tic()
  for (hidden1 in flags$hidden1){
    for (hidden2 in flags$hidden2){
      for (hidden3 in flags$hidden3){
        for (activation in flags$activation){
          for (rep in flags$rep){
            print(c(hidden1, hidden2, hidden3, activation, rep))
            out <- tryCatch(
              {
            ANNmodel <- neuralnet(formula = formula, data = train, hidden = c(hidden1, hidden2, hidden3), act.fct = functions[[activation]], 
                                  rep = rep, linear.output = TRUE)
              }, error=function(e){})
            preds <- neuralnet::compute(ANNmodel, test)$net.result
            if (activation == 3){
              preds <- ((preds+1)/2)*(max-min)+min
              MAE <- mean(abs(preds-((test[,dep]+1)/2)*(max-min)+min)) 
            }else {
              preds <- preds*(max-min)+min
              MAE <- mean(abs(preds-test[,dep])) 
            }
            df <- data.frame(Hidden1 = hidden1, Hidden2 = hidden2, Hidden3 = hidden3,
                             Activation = activation, Rep = rep, MAE = MAE)
            
            dfOverall <- rbind(dfOverall, df)
          }
        }
      }
    }
  }
  toc()
  return(dfOverall)
}


fitANN <- function(train, test, formula, dep, print = TRUE, hidden = c(4, 1), act.fct = "tanh", rep = 10, min, max){
  
  ANNmodel <- neuralnet(formula = formula, data = train, hidden = hidden, act.fct = act.fct, rep = rep, err.fct = "sse", linear.output = TRUE)
  
  ANNmodel$net.result
  plot(ANNmodel, rep = 'best')
  
  preds <- neuralnet::compute(ANNmodel, test)$net.result
  if (act.fct == "tanh"){
    preds <- ((preds+1)/2)*(max-min)+min
    if (print){
      print(data.frame(Predictions = preds, Observations = ((test[,dep]+1)/2)*(max-min)+min))
    }
    MAE <- mean(abs(preds-((test[,dep]+1)/2)*(max-min)+min)) 
    MSE <- mean((preds-((test[,dep]+1)/2)*(max-min)+min)^2) 
    MASE <- mase(((test[,dep]+1)/2)*(max-min)+min,preds)
  }else {
    preds <- preds*(max-min)+min
    if (print){
      print(data.frame(Predictions = preds, Observations = test[,dep]*(max-min)+min))
    }
    MAE <- mean(abs(preds-test[,dep]*(max-min)+min)) 
    MSE <- mean((preds-test[,dep]*(max-min)+min)^2) 
    MASE <- mase(test[,dep]*(max-min)+min,preds)
  }
  return(list(preds = preds, MAE = MAE, MSE = MSE, RMSE = sqrt(MSE), MASE= MASE ))

}

custom_activation <- function(x) {
  # activation_linear(x) 
  # log(exp(x) + 1) # softplus 
  x / (abs(x) + 1) # softsign 0 
  # ((exp(x) - exp(-x))/(exp(x) + exp(-x))) #tanh(x) 
  # 2*activation_elu(x, alpha =.2) #selu
}

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

# Rescale for tanh activation function
scale11 <- function(x) {
  (2 * ((x - min(x))/(max(x) - min(x)))) - 1
}

# Oats model
grainProductionDataS <- grainProduction %>%
  mutate_all(scale01)
dep <- 5
dataTrainOats <- grainProductionDataS[1:50, c(1:5)]
dataTestOats <- grainProductionDataS[51:58, c(1:5)]

# Run for tune up
set.seed(2523)
TuneOats <- tuneANN(train = dataTrainOats, test = dataTestOats, 
                    formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                    min = min(grainProduction[,dep]), max = max(grainProduction[,dep]), dep = dep)
 # write.csv(TuneOats,"ANNtuneUpResultsOatsV2.csv")

ANNOats <- fitANN(train = dataTrainOats, test = dataTestOats, formula = as.formula("Oats.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, 
                  hidden = c(4, 2, 1), act.fct = "logistic", rep = 20,
                  min = min(grainProduction[,5]), max = max(grainProduction[,5]))
ANNOats$MAE

# Corn model
grainProductionDataS <- grainProduction %>%
  mutate_all(scale11)

dep <- 5 
dataTrainCorn <- grainProductionDataS[1:50, c(1:4,6)]
dataTestCorn <- grainProductionDataS[51:58, c(1:4,6)]

# Run for tune up
set.seed(2523)
TuneCorn <- tuneANN(train = dataTrainCorn, test = dataTestCorn, 
                    formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                    min = min(grainProduction[,6]), max = max(grainProduction[,6]), dep = dep)
# write.csv(TuneCorn,"ANNtuneUpResultsCornV4.csv")

ANNCorn <- fitANN(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, 
                  hidden = c(4, 2, 1), act.fct = "tanh", rep = 20,
                  min = min(grainProduction[,6]), max = max(grainProduction[,6]))
ANNCorn$MAE

# Rice model
grainProductionDataS <- grainProduction %>%
  mutate_all(scale11)
dep <- 5 
dataTrainRice <- grainProductionDataS[1:50, c(1:4,7)]
dataTestRice <- grainProductionDataS[51:58, c(1:4,7)]

# Run for tune up
set.seed(2523)
TuneRice <- tuneANN(train = dataTrainRice, test = dataTestRice, 
                    formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                    min = min(grainProduction[,7]), max = max(grainProduction[,7]), dep = dep)
# write.csv(TuneRice,"ANNtuneUpResultsRiceV3.csv")

ANNRice <- fitANN(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, 
                  hidden = c(4, 2, 1), act.fct = "tanh", rep = 20,
                  min = min(grainProduction[,5]), max = max(grainProduction[,5]))
ANNRice$MAE

# Wheat model
grainProductionDataS <- grainProduction %>%
  mutate_all(scale11)

dataTrainWheat <- grainProductionDataS[1:50, c(1:4,8)]
dataTestWheat <- grainProductionDataS[51:58, c(1:4,8)]

# Run for tune up
set.seed(2523)
TuneWheat <- tuneANN(train = dataTrainWheat, test = dataTestWheat, 
                     formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."),
                    min = min(grainProduction[,8]), max = max(grainProduction[,8]), dep = dep)
# write.csv(TuneWheat,"ANNtuneUpResultsWheatV3.csv")

ANNWheat <- fitANN(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.1Mt. ~ Year + CO2.Tons. + Land.OceanTempIndex.C. + Cropland.1Mha."), 
                  dep = 5, print = TRUE, 
                  hidden = c(4, 2, 1), act.fct = "tanh", rep = 20,
                  min = min(grainProduction[,5]), max = max(grainProduction[,5]))
ANNWheat$MAE

minMAE_ANN <- c(ANNOats$MAE, ANNCorn$MAE, ANNRice$MAE, ANNWheat$MAE)
dfResultsANN <- data.frame(MAE_ANN = minMAE_ANN)  
rownames(dfResultsANN) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsANN

dfResults3 <- data.frame(dfResults2,MAE_ANN = minMAE_ANN)
dfResults3

minRMSE_ANN <- c(ANNOats$RMSE, ANNCorn$RMSE, ANNRice$RMSE, ANNWheat$RMSE)
dfResultsRMSEANN <- data.frame(RMSE_ANN = minRMSE_ANN)  
rownames(dfResultsRMSEANN) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsRMSEANN

dfResults3RMSE <- data.frame(dfResults2RMSE,RMSE_ANN = minRMSE_ANN)
dfResults3RMSE

minMASE_ANN <- c(ANNOats$MASE, ANNCorn$MASE, ANNRice$MASE, ANNWheat$MASE)
dfResultsMASEANN <- data.frame(MASE_ANN = minMASE_ANN)  
rownames(dfResultsMASEANN) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsMASEANN

dfResults3MASE <- data.frame(dfResults2MASE,MASE_ANN = minMASE_ANN)
dfResults3MASE


