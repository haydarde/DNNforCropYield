library(dLagM)
library(pastecs)
library(moments)
library(ggfortify)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(ggrepel)

# Uncomment the following and set the working directory to your folder
# setwd("PATH_TO_YOUR_FOLDER")

# Load data
data(grainProduction)
data <- grainProduction[,-1]
head(data)

# Display descriptive statistics
stat.desc(data)
skewness(data)
kurtosis(data)

# Time series plots
cornRiceWheat <- read.csv("cornRiceWheat.csv") # Corn, rice and wheat series in long format.
cornRiceWheat <- gather(grainProduction, Crop, Production, Corn.1Mt.:Wheat.1Mt., factor_key=TRUE)
p1 <- ggplot(cornRiceWheat, aes(x = Year, y = Production)) + 
      geom_line(aes(color = Crop), size = 1)+
      ylab("Production (Mt)")+ 
      scale_color_discrete(name = "Crop", labels = c("Corn", "Rice", "Wheat"))+
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p2 <- ggplot(grainProduction, aes(x = Year, y = Oats.1Mt.)) + 
      geom_line(aes(), size = 1, color = "darkgoldenrod3")+
      ylab("Oats production (Mt)")+ 
      theme_bw()+
  theme(text = element_text(size=16))

p3 <- ggplot(grainProduction, aes(x = Year, y = CO2.Tons.)) + 
      geom_line(aes(), size = 1, color = "darkred")+
      ylab("CO2 (Tons)")+ 
      theme_bw()+
  theme(text = element_text(size=16))

p4 <- ggplot(grainProduction, aes(x = Year, y = Land.OceanTempIndex.C.)) + 
      geom_line(aes(), size = 1, color = "darkorange1")+
      ylab("TempIndex (Degrees-C)")+ 
      theme_bw()+
  theme(text = element_text(size=16))

p5 <- ggplot(grainProduction, aes(x = Year, y = Cropland.1Mha.)) + 
      geom_line(aes(), size = 1, color = "darkgreen")+
      ylab("Cropland area (Mha)")+ 
      theme_bw()+
      theme(text = element_text(size=16))

figure <- ggarrange(p1, p2, p3, p4, p5, nrow = 3, ncol = 2);figure


# Histograms
panel.hist = function(x,cex.cor, ...){
  usr = par("usr"); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h = hist(x, plot=FALSE)
  breaks = h$breaks
  nB = length(breaks)
  y = h$counts
  y = y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="red", ...)
}

plotWheat <- wheat
colnames(plotWheat) <- c("CO2", "Temp" , "Cropland"  , "Oats",   "Corn"  , "Rice" , "Wheat")
pairs(plotWheat , diag.panel=panel.hist, cex.labels=1.65)


# Significance of signal with oats
CO2TS <- ts(data[, 1], start = 1961)
TempTS <- ts(data[, 2], start = 1961)
LandTS <- ts(data[, 3], start = 1961)
oatsTS <- ts(data[, 4], start = 1961)
cornTS <- ts(data[, 5], start = 1961)
riceTS <- ts(data[, 6], start = 1961)
wheatTS <- ts(data[, 7], start = 1961)

genres <- list()
CO2Oats <- rolCorPlot(x = CO2TS, y = oatsTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
TempOats <- rolCorPlot(x = TempTS, y = oatsTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
LandOats <- rolCorPlot(x = LandTS, y = oatsTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

CO2Corn <- rolCorPlot(x = CO2TS, y = cornTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
TempCorn <- rolCorPlot(x = TempTS, y = cornTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
LandCorn <- rolCorPlot(x = LandTS, y = cornTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

CO2Rice <- rolCorPlot(x = CO2TS, y = riceTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
TempRice <- rolCorPlot(x = TempTS, y = riceTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
LandRice <- rolCorPlot(x = LandTS, y = riceTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

CO2Wheat <- rolCorPlot(x = CO2TS, y = wheatTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
TempWheat <- rolCorPlot(x = TempTS, y = wheatTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
LandWheat <- rolCorPlot(x = LandTS, y = wheatTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

genres <- list()
genres[[1]] <- rolCorPlot(x = CO2TS, y = oatsTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[2]] <- rolCorPlot(x = TempTS, y = oatsTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[3]] <- rolCorPlot(x = LandTS, y = oatsTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

genres[[4]] <- rolCorPlot(x = CO2TS, y = cornTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[5]] <- rolCorPlot(x = TempTS, y = cornTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[6]] <- rolCorPlot(x = LandTS, y = cornTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

genres[[7]] <- rolCorPlot(x = CO2TS, y = riceTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[8]] <- rolCorPlot(x = TempTS, y = riceTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[9]] <- rolCorPlot(x = LandTS, y = riceTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)

genres[[10]] <- rolCorPlot(x = CO2TS, y = wheatTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[11]] <- rolCorPlot(x = TempTS, y = wheatTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)
genres[[12]] <- rolCorPlot(x = LandTS, y = wheatTS , width = c(7, 11, 15, 21), level = 0.95, N = 1000)


for (j in 1:12){
  for ( i in 1:4){
    # res <- matrix(NA,4,1)
    print(paste0(round(genres[[j]]$test[i,2],2),"(",round(genres[[j]]$test[i,3],2),", ",round(genres[[j]]$test[i,4],2),")"))
  }
}

# Analysis of benchmarking results
benchRes <- read.csv("benchmarkResults.csv")

MAE <- benchRes[which(benchRes$Measure == 'MAE'),]

p1 <- ggplot(MAE, aes(x = Value, y = Crop, group = Method)) +
  geom_point(size = 4, aes(color = Method, shape = Method)) +
  theme_bw() +
  xlab("rMAE value") + ylab("Crop type") +
  theme(text = element_text(size=16,color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p1

p2 <- ggplot(MAE, aes(x = Value, y = Crop, group = Method)) +
      geom_point(size = 5, aes(color = Method, shape = Method)) +
      xlim(0, 0.165) +
      # theme_bw() +
      theme(text = element_text(size=20),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"))
p2

RMSE <- benchRes[which(benchRes$Measure == 'RMSE'),]

p1 <- ggplot(RMSE, aes(x = Value, y = Crop, group = Method)) +
  geom_point(size = 4, aes(color = Method, shape = Method)) +
  theme_bw() +
  xlab("rRMSE value") + ylab("Crop type") +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p1

p2 <- ggplot(RMSE, aes(x = Value, y = Crop, group = Method)) +
  geom_point(size = 5, aes(color = Method, shape = Method)) +
  xlim(0, 0.2) +
  # theme_bw() +
  theme(text = element_text(size=20, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p2

MASE <- benchRes[which(benchRes$Measure == 'MASE'),]

p1 <- ggplot(MASE, aes(x = Value, y = Crop, group = Method)) +
  geom_point(size = 4, aes(color = Method, shape = Method)) +
  theme_bw() +
  xlab("MASE value") + ylab("Crop type") +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p1


# Yield forecasting for possible scenarios

# Create the data for each scenario
scenarioI <- matrix(c( 2019	,	36441387580	,	0.98	,	1556059.24	,
                       2020	,	36623594518	,	1.02	,	1633862.202	,
                       2021	,	36806712490	,	1.0251	,	1715555.312	,
                       2022	,	36990746053	,	1.0302255	,	1801333.078	,
                       2023	,	37175699783	,	1.035376628	,	1891399.732	,
                       2024	,	37361578282	,	1.040553511	,	1985969.718	,
                       2025	,	37548386174	,	1.045756278	,	2085268.204	), 
             nrow = 7, ncol = 4, byrow = TRUE)
scenarioI[,4] <- scenarioI[,4]/1000
# scenarioI <- as.data.frame(scenarioI)
# colnames(scenarioI) <- c("CO2.Tons.", "Land.OceanTempIndex.C.", "Cropland.1Mha.")

scenarioII <- matrix(c( 2019	,	36441387580	,	0.98	,	1556059.24	,
                        2020	,	36805801456	,	1.02	,	1571619.832	,
                        2021	,	37173859470	,	1.0302	,	1587336.031	,
                        2022	,	37545598065	,	1.040502	,	1603209.391	,
                        2023	,	37921054046	,	1.05090702	,	1619241.485	,
                        2024	,	38300264586	,	1.06141609	,	1635433.9	,
                        2025	,	38683267232	,	1.072030251	,	1651788.239	), 
                    nrow = 7, ncol = 4, byrow = TRUE)
scenarioII[,4] <- scenarioII[,4]/1000
# scenarioII <- as.data.frame(scenarioII)
# colnames(scenarioII) <- c("CO2.Tons.", "Land.OceanTempIndex.C.", "Cropland.1Mha.")

scenarioIII <- matrix(c(2019	,	36441387580	,	0.98	,	1556059.24	,
                        2020	,	36076973704	,	1.02	,	1571619.832	,
                        2021	,	3.572E+10	,	1.0098	,	1587336.031	,
                        2022	,	3.536E+10	,	0.999702	,	1603209.391	,
                        2023	,	3.501E+10	,	0.98970498	,	1619241.485	,
                        2024	,	3.466E+10	,	0.97980793	,	1635433.9	,
                        2025	,	3.431E+10	,	0.970009851	,	1651788.239	), 
                     nrow = 7, ncol = 4, byrow = TRUE)
scenarioIII[,4] <- scenarioIII[,4]/1000
# scenarioIII <- as.data.frame(scenarioII)
# colnames(scenarioIII) <- c("CO2.Tons.", "Land.OceanTempIndex.C.", "Cropland.1Mha.")

# Scenario IV assumes that each explanatory series follows the same trend as in the observation period 
# for the forecast horizon. So, we fit a trend model to each of the explanatory series.
data("grainProduction")
trendData <- grainProduction[,1:4]
trendData <- rbind(trendData,c(2019	,	36441387580	,	0.98	,	1556.05924))

CO2Model <- lm(CO2.Tons. ~ Year, trendData)
CO2Preds <- c(36441387580,
              predict(CO2Model, data.frame(Year = c(2020:2025))))

TempModel <- lm(Land.OceanTempIndex.C. ~ Year, trendData)
TempPreds <- c(0.98, 
               predict(TempModel, data.frame(Year = c(2020:2025))))
TempPreds[2] <- 1.02

CroplandModel <- lm(Cropland.1Mha. ~ Year, trendData)
CroplandPreds <- c(1556.05924,
                   predict(CroplandModel, data.frame(Year = c(2020:2025))))

scenarioIV <- matrix(cbind(2019:2025,CO2Preds,TempPreds,CroplandPreds), 
                     nrow = 7, ncol = 4)

# Put together all the scenarios
scenariosAll <- rbind(data.frame(scenarioI, Scenario = "I" ), data.frame(scenarioII, Scenario = "II"),
                                    data.frame(scenarioIII, Scenario = "III"),
                                    data.frame(scenarioIV, Scenario = "IV"))
colnames(scenariosAll) <-c("Year", "CO2", "Temp-Index", "Cropland", "Scenario")

scenariosAll[,'CO2'] <- scenariosAll[,'CO2'] / mean(scenariosAll[,'CO2'])
scenariosAll[,'Temp-Index'] <- scenariosAll[,'Temp-Index'] / mean(scenariosAll[,'Temp-Index'])
scenariosAll[,'Cropland'] <- scenariosAll[,'Cropland'] / mean(scenariosAll[,'Cropland'])

# Create long format for plotting.
scenarionsAll_long <- gather(scenariosAll, Series, Value, CO2:Cropland, factor_key=TRUE)


p0 <- ggplot(scenarionsAll_long[which(scenarionsAll_long$Series == "CO2"),], aes(x = Year, y = Value, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + #geom_point(aes(color = Scenario)) +
  #geom_point(data = labelOats, aes(label = label)) +
  #geom_label_repel(data = labelOats, aes(label = label), box.padding = 0.55, point.padding = 0.5) +
  theme_bw() +
  xlab("Year") + ylab("CO2 emissions") +
  scale_x_continuous(breaks = c(2019:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p0

library(dLagM)
data(grainProduction)
grainProduction <- grainProduction[,-1]

data.set <- as.matrix(grainProduction)

dimnames(data.set) = NULL

x_train <- data.set[1:50, c(1:3)] # Here we use all the observations for scaling

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_train <- data.set[1:50, c(1:3)]
x_test <- data.set[51:58, c(1:3)]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test, center = mean.train, scale = sd.train)
x_train <- scale(x_train)

scenarioI <- scenarioI[,-1]
scenarioI <- scale(scenarioI, center = mean.train, scale = sd.train)

scenarioII <- scenarioII[,-1]
scenarioII <- scale(scenarioII, center = mean.train, scale = sd.train)

scenarioIII <- scenarioIII[,-1]
scenarioIII <- scale(scenarioIII, center = mean.train, scale = sd.train)

scenarioIV <- scenarioIV[,-1]
scenarioIV <- scale(scenarioIV, center = mean.train, scale = sd.train)

fitDNN <- function(y_train, y_test, x_train, x_test, Out_x_test = NULL, dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                   alpha = 0.1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE){
  histVerbose <- 0
  histMetrics <- 0
  if (printIter){ histVerbose <- 2 }
  if (plotMetrics){ histMetrics <- 2 }
  
  
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
    verbose = histVerbose, view_metrics = histMetrics)
  
  c(loss, mae) %<-% (model %>% evaluate(x_test, y_test, verbose = histVerbose))
  
  preds <- predict_on_batch(object = model, x = x_test)
  
  MAE2 <- mean(abs(preds - y_test))
  MSE2 <- mean((preds - y_test)^2)
  
  MASE <- mase(y_test, preds)
  
  if (!is.null(Out_x_test)){
    
    predsOut <- predict(object = model, x = Out_x_test, verbose = 2)
    
    return(list(RMSE = sqrt(MSE2), MSE = MSE2, MAE = mae, MASE = MASE, fits = preds, forecasts = predsOut))
  } else {
    return(list(RMSE = sqrt(MSE2), MSE = MSE2, MAE = mae, MASE = MASE, fits = preds))
  }
}

# Oats forecasting
dependent = 4 # 4: oats, 5 : Corn, 6: Rice, 7 Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

oatsFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioI, 
                  dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.05, units = 10, activation = 1, 
                  alpha = 0.025, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

oatsFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioII, 
                    dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.05, units = 10, activation = 1, 
                    alpha = 0.025, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)
oatsFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIII, 
                    dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.05, units = 10, activation = 1, 
                    alpha = 0.025, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

oatsFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIV, 
                      dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.05, units = 10, activation = 1, 
                      alpha = 0.025, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

# Corn forecasting
dependent = 5 # 4: oats, 5 : Corn, 6: Rice, 7 Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

cornFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioI,
                  dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.05, units = 15, activation = 4, 
                  alpha = 2, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

cornFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioII,
                    dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.05, units = 15, activation = 4, 
                    alpha = 2, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

cornFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIII,
                    dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.05, units = 15, activation = 4, 
                    alpha = 2, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

cornFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIV,
                      dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.05, units = 15, activation = 4, 
                      alpha = 2, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

# Rice forecasting
dependent = 6 # oats, 5 : Corn, 6: Rice, 7 Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

riceFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioI,
                  dropout1 = 0.1, dropout2 = 0.15, dropout3 = 0.1, units = 40, activation = 3, 
                  alpha = 0.1, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

riceFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioII,
                    dropout1 = 0.1, dropout2 = 0.15, dropout3 = 0.1, units = 40, activation = 3, 
                    alpha = 0.1, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

riceFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIII,
                    dropout1 = 0.1, dropout2 = 0.15, dropout3 = 0.1, units = 40, activation = 3, 
                    alpha = 0.1, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

riceFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIV,
                      dropout1 = 0.1, dropout2 = 0.15, dropout3 = 0.1, units = 40, activation = 3, 
                      alpha = 0.1, batchSize = 50, printIter = FALSE, plotMetrics = FALSE)

# Wheat forecasting
dependent = 7 # oats, 5 : Corn, 6: Rice, 7 Wheat
y_train <- data.set[1:50, dependent]
y_test <- data.set[51:58, dependent]

wheatFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioI, 
                   dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                   alpha = 0.1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioII, 
                   dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                   alpha = 0.1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIII, 
                   dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                   alpha = 0.1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIV, 
                       dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
                       alpha = 0.1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)


frcScenarioI <- data.frame(Scenario = "I", Year = c(2019:2025), Oats = oatsFrcSI$forecasts, Corn = cornFrcSI$forecasts, 
                           Rice = riceFrcSI$forecasts, Wheat = wheatFrcSI$forecasts)

frcScenarioII <- data.frame(Scenario = "II",Year = c(2019:2025), Oats = oatsFrcSII$forecasts, Corn = cornFrcSII$forecasts, 
                           Rice = riceFrcSII$forecasts, Wheat = wheatFrcSII$forecasts)

frcScenarioIII <- data.frame(Scenario = "III",Year = c(2019:2025), Oats = oatsFrcSIII$forecasts, Corn = cornFrcSIII$forecasts, 
                            Rice = riceFrcSIII$forecasts, Wheat = wheatFrcSIII$forecasts)

frcScenarioIV <- data.frame(Scenario = "IV",Year = c(2019:2025), Oats = oatsFrcSIV$forecasts, Corn = cornFrcSIV$forecasts, 
                             Rice = riceFrcSIV$forecasts, Wheat = wheatFrcSIV$forecasts)

frcAll <- rbind(frcScenarioI, frcScenarioII, frcScenarioIII, frcScenarioIV)

# Change format for plotting
# frcScenarioI_long <- gather(frcScenarioI, Crop, Forecast, Oats:Wheat, factor_key=TRUE)
# frcScenarioII_long <- gather(frcScenarioII, Crop, Forecast, Oats:Wheat, factor_key=TRUE)
# frcScenarioIII_long <- gather(frcScenarioIII, Crop, Forecast, Oats:Wheat, factor_key=TRUE)
# frcScenarioIV_long <- gather(frcScenarioIV, Crop, Forecast, Oats:Wheat, factor_key=TRUE)

frcScenarioAll_long <- gather(frcAll, Crop, Forecast, Oats:Wheat, factor_key=TRUE)

labelOats <- data.frame(Year = c(2019, 2020), Forecast = c(22.949, 25.53),  
                        label = c("22.95", "25.53"), Scenario = c("I", "I"))

labelCorn <- data.frame(Year = c(2019, 2020), Forecast = c(1117.164, 1128.463),  
                        label = c("1117.16", "1128.46"), Scenario = c("I", "I"))

labelRice <- data.frame(Year = c(2019, 2020), Forecast = c(497.571, 503.534),  
                        label = c("497.57", "503.53"), Scenario = c("I", "I"))

labelWheat <- data.frame(Year = c(2019, 2020), Forecast = c(764.156, 776.097),  
                        label = c("764.16", "776.10"), Scenario = c("I", "I"))

p1 <- ggplot(frcScenarioAll_long[1:28,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) + 
  geom_point(data = labelOats, aes(label = label)) +
  geom_label_repel(data = labelOats, aes(label = label), box.padding = 0.55, point.padding = 0.5) +
  theme_bw() +
  xlab("Year") + ylab("Oats yield") + 
  scale_x_continuous(breaks = c(2019:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p1


p2 <- ggplot(frcScenarioAll_long[29:56,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) +
  geom_point(data = labelCorn, aes(label = label)) +
  geom_label_repel(data = labelCorn, aes(label = label), box.padding = unit(1, "lines"), point.padding = 0.5) +
  theme_bw() +
  xlab("Year") + ylab("Corn yield") +
  scale_x_continuous(breaks = c(2019:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p2

p3 <- ggplot(frcScenarioAll_long[57:84,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) +
  geom_point(data = labelRice, aes(label = label)) +
  geom_label_repel(data = labelRice, aes(label = label), box.padding = unit(1, "lines"), point.padding = 0.5) +
  theme_bw() +
  xlab("Year") + ylab("Rice yield") +
  scale_x_continuous(breaks = c(2019:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p3

p4 <- ggplot(frcScenarioAll_long[85:112,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) +
  geom_point(data = labelWheat, aes(label = label)) +
  geom_label_repel(data = labelWheat, aes(label = label), box.padding = 2.5, point.padding = 0.5) +
  geom_point(aes(color = Scenario)) +
  theme_bw() +
  xlab("Year") + ylab("Wheat yield") +
  scale_x_continuous(breaks = c(2019:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p4

library(ggpubr)
figure <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2); figure 







