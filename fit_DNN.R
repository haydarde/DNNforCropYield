library(plotly)
library(keras, quietly = T)
library(tensorflow)
library(tfruns)
library(tictoc)
library(Metrics)
library(dLagM)

set_random_seed(2021)
set.seed(2021)

data(grainProduction)
grainProduction
head(grainProduction)

grainProduction <- grainProduction[,-1]

data.set <- as.matrix(grainProduction)

dimnames(data.set) = NULL

x_train <- data.set[1:50, c(1:3)]
x_test <- data.set[51:58, c(1:3)]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test,
                center = mean.train,
                scale = sd.train)
x_train <- scale(x_train)

fitDNN <- function(y_train, y_test, x_train, x_test, dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                   alpha = 0.1, batchSize = 5){
  functions <- list()
  functions[[1]] <- function(x){log(exp(x) + 1)}  # softplus
  functions[[2]] <- function(x){x / (abs(x) + 1)} # softsign
  functions[[3]] <- function(x){((exp(x) - exp(-x))/(exp(x) + exp(-x)))} # tanh
  functions[[4]] <- function(x){activation_relu(x)} # relu
              
  set_random_seed(2523)
  set.seed(2523)
  model <- keras_model_sequential() %>% 
  layer_dense(units = units,
              activation = functions[[activation]],
              input_shape = c(3),
              kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(dropout1) %>% 
  layer_activation_elu(alpha = alpha) %>%
  layer_dense(units = units, kernel_initializer = "he_normal") %>% 
  layer_dropout(dropout2) %>% 
  layer_activation_elu(alpha = alpha) %>%
  layer_dense(units = units, kernel_initializer = "he_normal") %>% 
  layer_dropout(dropout3) %>% 
  layer_dense(units = 1)
              
              
  model %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = c("mean_absolute_error"))
              
  set_random_seed(2523)
  set.seed(2523)
  history <- model %>% fit(
      x_train,
      y_train,
      epoch = 600,
      batch_size = batchSize,
      validation_data = list(x_test, y_test),
      callbacks = c(
        callback_early_stopping(monitor = "val_mean_absolute_error", restore_best_weights = TRUE, patience = 5),
        callback_reduce_lr_on_plateau(factor = .00001)
      ),
      verbose = 2, view_metrics = 0)
              
      c(loss, mae) %<-% (model %>% evaluate(x_test, y_test, verbose = 0))
      
      preds <- predict_on_batch(object = model, x = x_test)
      
      MAE2 <- mean(abs(preds - y_test))
      MSE2 <- mean((preds - y_test)^2)
      
      MASE <- mase(y_test, preds)
      
      return(list(RMSE = sqrt(MSE2), MSE = MSE2, MAE = mae, MASE = MASE))#, MSE2 = MSE2, MAE2 = MAE2))
}

# Oats model
dependent = 4 # 4: Oats, 5: Corn, 6: Rice, 7: Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

DNNoats <- fitDNN(y_train, y_test, x_train, x_test, dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.05, units = 10, activation = 1, 
                  alpha = 0.025, batchSize = 5)
DNNoats

# Corn model
dependent = 5 # 4: Oats, 5: Corn, 6: Rice, 7: Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

DNNcorn <- fitDNN(y_train, y_test, x_train, x_test, dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.05, units = 15, activation = 4, 
                  alpha = 2, batchSize = 50)
DNNcorn

# Rice model
dependent = 6 # 4: Oats, 5: Corn, 6: Rice, 7: Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

DNNrice <- fitDNN(y_train, y_test, x_train, x_test, dropout1 = 0.1, dropout2 = 0.15, dropout3 = 0.1, units = 40, activation = 3, 
                  alpha = 0.1, batchSize = 50)
DNNrice

# Wheat model
dependent = 7 # 4: Oats, 5: Corn, 6: Rice, 7: Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

DNNwheat <- fitDNN(y_train, y_test, x_train, x_test, dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                  alpha = 0.1, batchSize = 5)
DNNwheat

minMAE <- c(DNNoats$MAE, DNNcorn$MAE, DNNrice$MAE, DNNwheat$MAE)
dfResultsDNN <- data.frame(MAE_DNN = minMAE)  
rownames(dfResultsDNN) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsDNN
dfResultsIndv <- dfResultsDNN

minRMSE <- c(DNNoats$RMSE, DNNcorn$RMSE, DNNrice$RMSE, DNNwheat$RMSE)
dfResultsRMSEDNN <- data.frame(RMSE_DNN = minRMSE)  
rownames(dfResultsRMSEDNN) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsRMSEDNN
dfResultsIndvRMSE <- dfResultsRMSEDNN


minMASE <- c(DNNoats$MASE, DNNcorn$MASE, DNNrice$MASE, DNNwheat$MASE)
dfResultsMASEDNN <- data.frame(MASE_DNN = minMASE)  
rownames(dfResultsMASEDNN) <- c("Oat", "Corn", "Rice", "Wheat")
dfResultsMASEDNN
dfResultsIndvMASE <- dfResultsMASEDNN


