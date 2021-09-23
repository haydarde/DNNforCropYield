library(tidyverse)
library(neuralnet)
library(GGally)
library(forecast)
library(tictoc)
library(dLagM)

set.seed(2523)

grainYield <- read.csv("cropYieldData.csv", header = TRUE)
head(grainYield)

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
            # hidden1 = 2; hidden2 = 5; hidden3 = 4; activation = 3; rep = 5
            print(c(hidden1, hidden2, hidden3, activation, rep))
            ANNmodel <- NULL
            out <- tryCatch(
              {
                set.seed(2523)
                ANNmodel <- neuralnet(formula = formula, data = train, hidden = c(hidden1, hidden2, hidden3), act.fct = functions[[activation]], 
                                  rep = rep, linear.output = TRUE)
              }, error=function(e){print("Error")})
            if (!is.null(ANNmodel)){
              out <- tryCatch(
                {
              preds <- neuralnet::compute(ANNmodel, test)$net.result
                }, error=function(e){print("Error2")})
              if (activation == 3){
                preds <- ((preds+1)/2)*(max-min)+min
                MAE <- mean(abs(preds-((test[,dep]+1)/2)*(max-min)+min)) 
              }else {
                preds <- preds*(max-min)+min
                MAE <- mean(abs(preds-(test[,dep]*(max-min)+min))) 
              }
              df <- data.frame(Hidden1 = hidden1, Hidden2 = hidden2, Hidden3 = hidden3,
                               Activation = activation, Rep = rep, MAE = MAE)
              
              dfOverall <- rbind(dfOverall, df)
            }
          }
        }
      }
    }
  }
  toc()
  return(dfOverall)
}


fitANN <- function(train, test, formula, dep, print = TRUE, hidden = c(4, 1), act.fct = 1, rep = 10, min, max){
  
  functions <- list()
  functions[[1]] <- function(x){log(exp(x) + 1)}  # softplus
  functions[[2]] <- function(x){x / (abs(x) + 1)} # softsign
  functions[[3]] <- "tanh" #function(x){((exp(x) - exp(-x))/(exp(x) + exp(-x)))} # tanh
  functions[[4]] <- "logistic"
  
  set.seed(2523)
  ANNmodel <- neuralnet(formula = formula, data = train, hidden = hidden, act.fct = functions[[act.fct]], rep = rep, linear.output = TRUE)

  ANNmodel$net.result
  plot(ANNmodel, rep = 'best')
  
  preds <- neuralnet::compute(ANNmodel, test)$net.result
  
  if (activation == 3){
    if (print){
      print(data.frame(Predictions = preds, Observations = ((test[,dep]+1)/2)*(max-min)+min))
    }
    preds <- ((preds+1)/2)*(max-min)+min
    MAE <- mean(abs(preds-((test[,dep]+1)/2)*(max-min)+min)) 
    MSE <- mean((preds-((test[,dep]+1)/2)*(max-min)+min)^2) 
    MASE <- mase(((test[,dep]+1)/2)*(max-min)+min,preds)
  }else {
    preds <- preds*(max-min)+min
    if (print){
      print(data.frame(Predictions = preds, Observations = test[,dep]*(max-min)+min))
    }
    MAE <- mean(abs(preds-(test[,dep]*(max-min)+min))) 
    MSE <- mean((preds-(test[,dep]*(max-min)+min))^2) 
    MASE <- mase((test[,dep]*(max-min)+min),preds)
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
grainYieldDataS <- grainYield %>%
  mutate_all(scale01)
dep <- 2
dataTrainOats <- grainYieldDataS[1:35, c(1, 3, 7, 10:13)]
dataTestOats <- grainYieldDataS[36:42, c(1, 3, 7, 10:13)]

# Run for tune up
set.seed(2523)
TuneOats <- tuneANN(train = dataTrainOats, test = dataTestOats, 
                    formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                    min = min(grainYield[,3]), max = max(grainYield[,3]), dep = dep)
# write.csv(TuneOats,"ANNtuneUpResultsOats.csv")

ANNOats <- fitANN(train = dataTrainOats, test = dataTestOats, 
                  formula = as.formula("Oats.yield ~ Year + Oats.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = dep, print = TRUE, 
                  hidden = c(3, 5, 1), act.fct = 4, rep = 5,
                  min = min(grainYield[,3]), max = max(grainYield[,3]))
ANNOats$MAE

# Corn model
grainYieldDataS <- grainYield %>%
  mutate_all(scale11)

dep <- 2 
dataTrainCorn <- grainYieldDataS[1:35, c(1, 2, 6, 10:13)]
dataTestCorn <- grainYieldDataS[36:42, c(1, 2, 6, 10:13)]

# Run for tune up
set.seed(2523)
TuneCorn <- tuneANN(train = dataTrainCorn, test = dataTestCorn, 
                    formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                    min = min(grainYield[,2]), max = max(grainYield[,2]), dep = dep)
# write.csv(TuneCorn,"ANNtuneUpResultsCorn.csv")

ANNCorn <- fitANN(train = dataTrainCorn, test = dataTestCorn, formula = as.formula("Corn.yield ~ Year + Corn.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = 2, print = TRUE, 
                  hidden = c(2, 5, 4), act.fct = 3, rep = 5,
                  min = min(grainYield[,2]), max = max(grainYield[,2]))
ANNCorn$MAE

# Rice model
grainYieldDataS <- grainYield %>%
  mutate_all(scale11)
dep <- 2 
dataTrainRice <- grainYieldDataS[1:35, c(1, 4, 8, 10:13)]
dataTestRice <- grainYieldDataS[36:42, c(1, 4, 8, 10:13)]

# Run for tune up
set.seed(2523)
TuneRice <- tuneANN(train = dataTrainRice, test = dataTestRice, 
                    formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                    min = min(grainYield[,4]), max = max(grainYield[,4]), dep = dep)
# write.csv(TuneRice,"ANNtuneUpResultsRice.csv")

ANNRice <- fitANN(train = dataTrainRice, test = dataTestRice, formula = as.formula("Rice.yield ~ Year + Rice.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = 2, print = TRUE, 
                  hidden = c(5, 3, 1), act.fct = 4, rep = 5,
                  min = min(grainYield[,4]), max = max(grainYield[,4]))
ANNRice$MAE

# Wheat model
grainYieldDataS <- grainYield %>%
  mutate_all(scale11)

dataTrainWheat <- grainYieldDataS[1:35, c(1, 5, 9, 10:13)]
dataTestWheat <- grainYieldDataS[36:42, c(1, 5, 9, 10:13)]

# Run for tune up
set.seed(2523)
TuneWheat <- tuneANN(train = dataTrainWheat, test = dataTestWheat, 
                     formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"),
                    min = min(grainYield[,5]), max = max(grainYield[,5]), dep = dep)
# write.csv(TuneWheat,"ANNtuneUpResultsWheat.csv")

ANNWheat <- fitANN(train = dataTrainWheat, test = dataTestWheat, formula = as.formula("Wheat.yield ~ Year + Wheat.area + Fertilizer + Temp.anomaly + Rainfall.anomaly + C02"), 
                  dep = 2, print = TRUE, 
                  hidden = c(2, 4, 2), act.fct = 4, rep = 5,
                  min = min(grainYield[,5]), max = max(grainYield[,5]))
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


