library(plotly)
library(keras, quietly = T)
library(tensorflow)
library(tfruns)
library(tictoc)
library(dLagM)

set_random_seed(2021)
set.seed(2021)

# Run for each crop type
dependent <- 4 # 2: Oats, 1: Corn, 3: Rice, 4: Wheat
# independents <- c(6, 9:12) # Oats
# independents <- c(5, 9:12) # Corn
# independents <- c(7, 9:12) # Rice
independents <- c(8, 9:12) # Wheat

numInputs = length(independents)

grainYield <- read.csv("cropYieldData.csv", header = TRUE)
head(grainYield)

grainYield <- grainYield[,-1] # Remove year

data.set <- as.matrix(grainYield)

dimnames(data.set) = NULL

x_train <- data.set[1:35, independents]
x_test <- data.set[36:42, independents]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test, center = mean.train, scale = sd.train)
x_train <- scale(x_train)

flags = list(
  dropout1 = c(0.05, 0.1, 0.15, 0.20, 0.25), # Small dataset - big dropout wouldn't be suitable
  dropout2 = c(0.05, 0.1, 0.15, 0.20, 0.25),
  dropout3 = c(0.05, 0.1, 0.15, 0.20, 0.25),
  units = c(10, 15, 20, 25, 40),
  dependent = dependent,
  activation = c(1, 2, 3, 4),
  alpha = c(0.025, 0.1, 0.25, 1, 2),
  batchSize = c(5, 10, 25, 30, 50)
)

functions <- list()
functions[[1]] <- function(x){log(exp(x) + 1)}  # softplus
functions[[2]] <- function(x){x / (abs(x) + 1)} # softsign
functions[[3]] <- function(x){((exp(x) - exp(-x))/(exp(x) + exp(-x)))} # tanh
functions[[4]] <- function(x){activation_relu(x)} # relu

dfOverall <- data.frame()

y_train <- data.set[1:35, flags$dependent]
y_test <- data.set[36:42, flags$dependent]

tic()
for (dropout1 in flags$dropout1){
  for (dropout2 in flags$dropout2){
    for (dropout3 in flags$dropout3){
      for (units in flags$units){
        for (activation in flags$activation){
          for (alpha in flags$alpha){
            for (batchSize in flags$batchSize){
              set_random_seed(2523)
              set.seed(2523)
              model <- keras_model_sequential() %>% 
                layer_dense(units = units,
                            activation = functions[[activation]],
                            input_shape = c(numInputs),
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
                verbose = 0, view_metrics = 0)
              
              c(loss, mae) %<-% (model %>% evaluate(x_test, y_test, verbose = 0))
              
              df <- data.frame(Dropout1 = dropout1, Dropout2 = dropout2, Dropout3 = dropout3,
                               Units = units, Activation = activation, alpha = alpha, batchSize = batchSize, MAE = mae)
              
              dfOverall <- rbind(dfOverall, df)
            }
          }
        }
      }
    }
  }
}
toc()

dfOverall

