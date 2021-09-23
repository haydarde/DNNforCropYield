library(dLagM); library(pastecs); library(moments)
library(ggfortify); library(tidyr); library(ggpubr)
library(ggplot2); library(ggrepel); library(plotly)
library(keras, quietly = T); library(tensorflow)
library(tfruns); library(tictoc); library(Metrics)
library(ggpubr)

fitDNN <- function(y_train, y_test, x_train, x_test, Out_x_test = NULL, numInputs = 5, dropout1 = 0.1, dropout2 = 0.1, dropout3 = 0.1, units = 40, activation = 2, 
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


# Uncomment the following and set the working directory to your folder
# setwd("PATH_TO_YOUR_FOLDER")

# Load data
grainYield <- read.csv("cropYieldData.csv", header = TRUE)
head(grainYield)
data <- grainYield[,-1]

# Display descriptive statistics
stat.desc(data)
skewness(data)
kurtosis(data)

# Time series plots
OatsWheat <- read.csv("OatsWheatYieldLongFormat.csv") # Oats and wheat series in long format.
CornRice <- read.csv("CornRiceYieldLongFormat.csv") # Corn and rice series in long format.
OatsWheatArea <- read.csv("OatsWheatAreaLongFormat.csv") # Oats and wheat series in long format.
CornRiceArea <- read.csv("RiceCornAreaLongFromat.csv")
cornOatsRiceWheat <- gather(grainProduction, Crop, Production, Corn.1Mt.:Wheat.1Mt., factor_key=TRUE)
p1 <- ggplot(OatsWheat, aes(x = Year, y = Yield)) + 
      geom_line(aes(color = Crop), size = 1)+
      ylab("Yield (t/ha)")+ 
      scale_colour_manual(name = "Crop", labels = c("Oats", "Wheat"),values = c("blue", "darkgoldenrod3"))+
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p2 <- ggplot(CornRice, aes(x = Year, y = Yield)) + 
      geom_line(aes(color = Crop), size = 1)+
      ylab("Yield (t/ha)")+ 
      scale_colour_manual(name = "Crop", labels = c("Corn", "Rice"),values = c("cyan", "darkorange"))+
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p3 <- ggplot(CornRiceArea, aes(x = Year, y = Area)) + 
      geom_line(aes(color = Crop), size = 1)+
      ylab("Area (1000 ha)")+ 
      scale_colour_manual(name = "Crop", labels = c("Corn", "Rice"),values = c("purple", "green"))+
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p4 <- ggplot(grainYield, aes(x = Year, y = Oats.area)) + 
      geom_line(size = 1, color ="purple")+
      ylab("Oats Area (1000 ha)")+ 
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p5 <- ggplot(grainYield, aes(x = Year, y = Wheat.area)) + 
      geom_line(size = 1, color ="darkblue")+
      ylab("Wheat Area (1000 ha)")+ 
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p6 <- ggplot(grainYield, aes(x = Year, y = Fertilizer)) + 
      geom_line(size = 1, color ="darkgreen")+
      ylab("Fert. (kg/ha)")+ 
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p7 <- ggplot(grainYield, aes(x = Year, y = C02)) + 
      geom_line(size = 1, color ="red")+
      ylab("CO2 (ton)")+ 
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p8 <- ggplot(grainYield, aes(x = Year, y = Rainfall.anomaly)) + 
      geom_line(size = 1, color ="darkred")+
      ylab("Rainf.Anom.(mm)")+ 
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

p9 <- ggplot(grainYield, aes(x = Year, y = Temp.anomaly)) + 
      geom_line(size = 1, color ="darkorange1")+
      ylab("Temp.Anom.(Deg-C)")+ 
      theme_bw()+
      theme(text = element_text(size=16), legend.text=element_text(size=16))

figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 5, ncol = 2);figure


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

plotGrainYield <- grainYield[,-1]
colnames(plotGrainYield) <- c("CornY", "OatsY", "Rice.Y", "WheatY", "CornA", "OatsA", 
                         "RiceA", "WheatA", "Fertilizer", "TempAnm", "RainfAnm", "CO2")
pairs(plotGrainYield , diag.panel=panel.hist, cex.labels=1.65)

cor(plotGrainYield)

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

# Zoom in
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

# Zoom in
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

p2 <- ggplot(MASE, aes(x = Value, y = Crop, group = Method)) +
  geom_point(size = 5, aes(color = Method, shape = Method)) +
  xlim(0, 1) +
  # theme_bw() +
  theme(text = element_text(size=20, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p2

# Yield forecasting for possible scenarios

# Create scenarios
SI.CO2inc <- 0.04; SI.Tempinc <- 0.00; SI.Raininc <- 0.00
SII.CO2inc <- 0.00; SII.Tempinc <- 0.05; SII.Raininc <- 0.00
SIII.CO2inc <- 0.00; SIII.Tempinc <- 0.00; SIII.Raininc <- 0.2
SIV.CO2inc <- 0.04; SIV.Tempinc <- 0.05; SIV.Raininc <- -0.05
SV.CO2inc <- -0.04; SV.Tempinc <- 0.01; SV.Raininc <- -0.05

scenarioI <- scenarioII <- scenarioIII <- scenarioIV <- scenarioV <- scenarioVI <- grainYield[42,c(1,6:13)]
for (i in 1:5){
  scenarioI[nrow(scenarioI)+1, ] <- c(scenarioI[i,1] + 1, scenarioI[i,2:6],
                     scenarioI[i,7]*SI.Tempinc + scenarioI[i,7],
                     scenarioI[i,8]*SI.Raininc + scenarioI[i,8],
                     scenarioI[i,9]*SI.CO2inc + scenarioI[i,9])
  scenarioII[nrow(scenarioII)+1, ] <- c(scenarioII[i,1] + 1, scenarioII[i,2:6],
                                      scenarioII[i,7]*SII.Tempinc + scenarioII[i,7],
                                      scenarioII[i,8]*SII.Raininc + scenarioII[i,8],
                                      scenarioII[i,9]*SII.CO2inc + scenarioII[i,9])
  scenarioIII[nrow(scenarioIII)+1, ] <- c(scenarioIII[i,1] + 1, scenarioIII[i,2:6],
                                       scenarioIII[i,7]*SIII.Tempinc + scenarioIII[i,7],
                                       scenarioIII[i,8]*SIII.Raininc + scenarioIII[i,8],
                                       scenarioIII[i,9]*SIII.CO2inc + scenarioIII[i,9])
  scenarioIV[nrow(scenarioIV)+1, ] <- c(scenarioIV[i,1] + 1, scenarioIV[i,2:6],
                                        scenarioIV[i,7]*SIV.Tempinc + scenarioIV[i,7],
                                        scenarioIV[i,8]*SIV.Raininc + scenarioIV[i,8],
                                        scenarioIV[i,9]*SIV.CO2inc + scenarioIV[i,9])
  scenarioV[nrow(scenarioV)+1, ] <- c(scenarioV[i,1] + 1, scenarioV[i,2:6],
                                      scenarioV[i,7]*SV.Tempinc + scenarioV[i,7],
                                      scenarioV[i,8]*SV.Raininc + scenarioV[i,8],
                                      scenarioV[i,9]*SV.CO2inc + scenarioV[i,9])
}


# Scenario IV assumes that each explanatory series follows the same trend as in the observation period 
# for the forecast horizon. So, we fit a trend model to each of the explanatory series.
CO2Model <- lm(C02 ~ Year, grainYield)
CO2Preds <- predict(CO2Model, data.frame(Year = c(2021:2025)))

TempModel <- lm(Temp.anomaly ~ Year, grainYield)
TempPreds <- predict(TempModel, data.frame(Year = c(2021:2025)))

RainfModel <- lm(Rainfall.anomaly ~ Year, grainYield)
RainfPreds <- predict(RainfModel, data.frame(Year = c(2021:2025)))

FertModel <- lm(Fertilizer ~ Year, grainYield)
FertPreds <- predict(FertModel, data.frame(Year = c(2021:2025)))

OatsCropAreaModel <- lm(Oats.area  ~ Year, grainYield)
OatsCropAreaPreds <- predict(OatsCropAreaModel, data.frame(Year = c(2021:2025)))

CornCropAreaModel <- lm(Corn.area  ~ Year, grainYield)
CornCropAreaPreds <- predict(CornCropAreaModel, data.frame(Year = c(2021:2025)))

RiceCropAreaModel <- lm(Rice.area  ~ Year, grainYield)
RiceCropAreaPreds <- predict(RiceCropAreaModel, data.frame(Year = c(2021:2025)))

WheatCropAreaModel <- lm(Wheat.area  ~ Year, grainYield)
WheatCropAreaPreds <- predict(WheatCropAreaModel, data.frame(Year = c(2021:2025)))

scenarioVI <- rbind(scenarioVI, data.frame(Year = c(2021:2025), Corn.area = CornCropAreaPreds, Oats.area = OatsCropAreaPreds, Rice.area = RiceCropAreaPreds, 
                         Wheat.area = WheatCropAreaPreds, Fertilizer = FertPreds, Temp.anomaly = TempPreds, Rainfall.anomaly = RainfPreds, C02 = CO2Preds) )

# Put together all the scenarios
scenarioI.2 <- cbind(rep("I",5), scenarioI[2:6,])
colnames(scenarioI.2) <- c("Scenario", colnames(scenarioI))
scenarioII.2 <- cbind(rep("II",5), scenarioII[2:6,])
colnames(scenarioII.2) <- c("Scenario", colnames(scenarioII))
scenarioIII.2 <- cbind(rep("III",5), scenarioIII[2:6,])
colnames(scenarioIII.2) <- c("Scenario", colnames(scenarioIII))
scenarioIV.2 <- cbind(rep("IV",5), scenarioIV[2:6,])
colnames(scenarioIV.2) <- c("Scenario", colnames(scenarioIV))
scenarioV.2 <- cbind(rep("V",5), scenarioV[2:6,])
colnames(scenarioV.2) <- c("Scenario", colnames(scenarioV))
scenarioVI.2 <- cbind(rep("VI",5), scenarioVI[2:6,])
colnames(scenarioVI.2) <- c("Scenario", colnames(scenarioVI))
scenariosAll <- rbind(scenarioI.2, scenarioII.2, scenarioII.2, scenarioIV.2, scenarioV.2, scenarioVI.2)

# Create long format for plotting.
scenarionsAll_long <- gather(scenariosAll, Series, Value, C02:Wheat.area, factor_key=TRUE)
#  write.csv(scenariosAll, "scenarionsAll_long.csv")

library(dLagM)
data.set <- as.matrix(data)

dimnames(data.set) = NULL
# Uncomment the crop type to get the results for
independents <- c(6, 9:12) # Oats
# independents <- c(5, 9:12) # Corn
# independents <- c(7, 9:12) # Rice
# independents <- c(8, 9:12) # Wheat

independentsScn <- c(3,6:9) # Oats
# independentsScn <- c(2,6:9) # Corn
# independentsScn <- c(4,6:9) # Rice
# independentsScn <- c(5,6:9) # Wheat

x_train <- data.set[1:42, independents]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_train <- data.set[1:35, independents]
x_test <- data.set[36:42, independents]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test, center = mean.train, scale = sd.train)
x_train <- scale(x_train)

# Oats forecasting
scenarioIo <- scenarioI[2:6,independentsScn]
scenarioIo <- scale(scenarioIo, center = mean.train, scale = sd.train)

scenarioIIo <- scenarioII[2:6,independentsScn]
scenarioIIo <- scale(scenarioIIo, center = mean.train, scale = sd.train)

scenarioIIIo <- scenarioIII[2:6,independentsScn]
scenarioIIIo <- scale(scenarioIIIo, center = mean.train, scale = sd.train)

scenarioIVo <- scenarioIV[2:6,independentsScn]
scenarioIVo <- scale(scenarioIVo, center = mean.train, scale = sd.train)

scenarioVo <- scenarioV[2:6,independentsScn]
scenarioVo <- scale(scenarioVo, center = mean.train, scale = sd.train)

scenarioVIo <- scenarioVI[2:6,independentsScn]
scenarioVIo <- scale(scenarioVIo, center = mean.train, scale = sd.train)

dependent <- 2 # 2: Oats, 1: Corn, 3: Rice, 4: Wheat
y_train <- data[1:35, dependent]
y_test <- data[36:42, dependent]

oatsFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIo, 
                    dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.1, units = 15, activation = 4, 
                    alpha = 2, batchSize = 25, printIter = FALSE, plotMetrics = FALSE)

oatsFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIo, 
                     dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.1, units = 15, activation = 4, 
                     alpha = 2, batchSize = 25, printIter = FALSE, plotMetrics = FALSE)
oatsFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIIo, 
                      dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.1, units = 15, activation = 4, 
                      alpha = 2, batchSize = 25, printIter = FALSE, plotMetrics = FALSE)

oatsFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIVo, 
                     dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.1, units = 15, activation = 4, 
                     alpha = 2, batchSize = 25, printIter = FALSE, plotMetrics = FALSE)

oatsFrcSV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVo, 
                     dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.1, units = 15, activation = 4, 
                     alpha = 2, batchSize = 25, printIter = FALSE, plotMetrics = FALSE)

oatsFrcSVI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVIo, 
                    dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.1, units = 15, activation = 4, 
                    alpha = 2, batchSize = 25, printIter = FALSE, plotMetrics = FALSE)

# Corn forecasting
# independents <- c(6, 9:12) # Oats
independents <- c(5, 9:12) # Corn
# independents <- c(7, 9:12) # Rice
# independents <- c(8, 9:12) # Wheat

# independentsScn <- c(3,6:9) # Oats
independentsScn <- c(2,6:9) # Corn
# independentsScn <- c(4,6:9) # Rice
# independentsScn <- c(5,6:9) # Wheat

x_train <- data.set[1:42, independents] # Here we use all the observations for scaling

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_train <- data.set[1:35, independents]
x_test <- data.set[36:42, independents]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test, center = mean.train, scale = sd.train)
x_train <- scale(x_train)

scenarioIc <- scenarioI[2:6,independentsScn]
scenarioIc <- scale(scenarioIc, center = mean.train, scale = sd.train)

scenarioIIc <- scenarioII[2:6,independentsScn]
scenarioIIc <- scale(scenarioIIc, center = mean.train, scale = sd.train)

scenarioIIIc <- scenarioIII[2:6,independentsScn]
scenarioIIIc <- scale(scenarioIIIc, center = mean.train, scale = sd.train)

scenarioIVc <- scenarioIV[2:6,independentsScn]
scenarioIVc <- scale(scenarioIVc, center = mean.train, scale = sd.train)

scenarioVc <- scenarioV[2:6,independentsScn]
scenarioVc <- scale(scenarioVc, center = mean.train, scale = sd.train)

scenarioVIc <- scenarioVI[2:6,independentsScn]
scenarioVIc <- scale(scenarioVIc, center = mean.train, scale = sd.train)

dependent <- 1 # 2: Oats, 1: Corn, 3: Rice, 4: Wheat
y_train <- data[1:35, dependent]
y_test <- data[36:42, dependent]

cornFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIc,
                    dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.15, units = 40, activation = 2, 
                    alpha = 1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

cornFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIc,
                     dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.15, units = 40, activation = 2, 
                     alpha = 1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

cornFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIIc,
                      dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.15, units = 40, activation = 2, 
                      alpha = 1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

cornFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIVc,
                     dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.15, units = 40, activation = 2, 
                     alpha = 1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

cornFrcSV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVc,
                     dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.15, units = 40, activation = 2, 
                     alpha = 1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

cornFrcSVI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVIc,
                    dropout1 = 0.05, dropout2 = 0.05, dropout3 = 0.15, units = 40, activation = 2, 
                    alpha = 1, batchSize = 5, printIter = FALSE, plotMetrics = FALSE)

# Rice forecasting
# independents <- c(6, 9:12) # Oats
# independents <- c(5, 9:12) # Corn
independents <- c(7, 9:12) # Rice
# independents <- c(8, 9:12) # Wheat

# independentsScn <- c(3,6:9) # Oats
# independentsScn <- c(2,6:9) # Corn
independentsScn <- c(4,6:9) # Rice
# independentsScn <- c(5,6:9) # Wheat

x_train <- data.set[1:42, independents] # Here we use all the observations for scaling

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_train <- data.set[1:35, independents]
x_test <- data.set[36:42, independents]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test, center = mean.train, scale = sd.train)
x_train <- scale(x_train)

scenarioIr <- scenarioI[2:6,independentsScn]
scenarioIr <- scale(scenarioIr, center = mean.train, scale = sd.train)

scenarioIIr <- scenarioII[2:6,independentsScn]
scenarioIIr <- scale(scenarioIIr, center = mean.train, scale = sd.train)

scenarioIIIr <- scenarioIII[2:6,independentsScn]
scenarioIIIr <- scale(scenarioIIIr, center = mean.train, scale = sd.train)

scenarioIVr <- scenarioIV[2:6,independentsScn]
scenarioIVr <- scale(scenarioIVr, center = mean.train, scale = sd.train)

scenarioVr <- scenarioV[2:6,independentsScn]
scenarioVr <- scale(scenarioVr, center = mean.train, scale = sd.train)

scenarioVIr <- scenarioVI[2:6,independentsScn]
scenarioVIr <- scale(scenarioVIr, center = mean.train, scale = sd.train)

dependent <- 3 # 2: Oats, 1: Corn, 3: Rice, 4: Wheat
y_train <- data[1:35, dependent]
y_test <- data[36:42, dependent]

riceFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIr,
                    dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.15, units = 40, activation = 2, 
                    alpha = 2, batchSize = 10, printIter = FALSE, plotMetrics = FALSE)

riceFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIr,
                     dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.15, units = 40, activation = 2, 
                     alpha = 2, batchSize = 10, printIter = FALSE, plotMetrics = FALSE)

riceFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIIr,
                      dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.15, units = 40, activation = 2, 
                      alpha = 2, batchSize = 10, printIter = FALSE, plotMetrics = FALSE)

riceFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIVr,
                     dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.15, units = 40, activation = 2, 
                     alpha = 2, batchSize = 10, printIter = FALSE, plotMetrics = FALSE)

riceFrcSV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVr,
                     dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.15, units = 40, activation = 2, 
                     alpha = 2, batchSize = 10, printIter = FALSE, plotMetrics = FALSE)

riceFrcSVI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVIr,
                    dropout1 = 0.05, dropout2 = 0.15, dropout3 = 0.15, units = 40, activation = 2, 
                    alpha = 2, batchSize = 10, printIter = FALSE, plotMetrics = FALSE)

# Wheat forecasting
# independents <- c(6, 9:12) # Oats
# independents <- c(5, 9:12) # Corn
# independents <- c(7, 9:12) # Rice
independents <- c(8, 9:12) # Wheat

# independentsScn <- c(3,6:9) # Oats
# independentsScn <- c(2,6:9) # Corn
# independentsScn <- c(4,6:9) # Rice
independentsScn <- c(5,6:9) # Wheat

x_train <- data.set[1:42, independents] # Here we use all the observations for scaling

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_train <- data.set[1:35, independents]
x_test <- data.set[36:42, independents]

mean.train <- apply(x_train, 2, mean)
sd.train <- apply(x_train, 2, sd)

x_test <- scale(x_test, center = mean.train, scale = sd.train)
x_train <- scale(x_train)

scenarioIw <- scenarioI[2:6,independentsScn]
scenarioIw <- scale(scenarioIw, center = mean.train, scale = sd.train)

scenarioIIw <- scenarioII[2:6,independentsScn]
scenarioIIw <- scale(scenarioIIw, center = mean.train, scale = sd.train)

scenarioIIIw <- scenarioIII[2:6,independentsScn]
scenarioIIIw <- scale(scenarioIIIw, center = mean.train, scale = sd.train)

scenarioIVw <- scenarioIV[2:6,independentsScn]
scenarioIVw <- scale(scenarioIVw, center = mean.train, scale = sd.train)

scenarioVw <- scenarioV[2:6,independentsScn]
scenarioVw <- scale(scenarioVw, center = mean.train, scale = sd.train)

scenarioVIw <- scenarioVI[2:6,independentsScn]
scenarioVIw <- scale(scenarioVIw, center = mean.train, scale = sd.train)

dependent <- 4 # 2: Oats, 1: Corn, 3: Rice, 4: Wheat
y_train <- data[1:35, dependent]
y_test <- data[36:42, dependent]

wheatFrcSI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIw, 
                     dropout1 = 0.2, dropout2 = 0.25, dropout3 = 0.2, units = 20, activation = 1, 
                     alpha = 2, batchSize = 30, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIw, 
                      dropout1 = 0.2, dropout2 = 0.25, dropout3 = 0.2, units = 20, activation = 1, 
                      alpha = 2, batchSize = 30, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSIII <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIIIw, 
                       dropout1 = 0.2, dropout2 = 0.25, dropout3 = 0.2, units = 20, activation = 1, 
                       alpha = 2, batchSize = 30, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSIV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioIVw, 
                      dropout1 = 0.2, dropout2 = 0.25, dropout3 = 0.2, units = 20, activation = 1, 
                      alpha = 2, batchSize = 30, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSV <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVw, 
                      dropout1 = 0.2, dropout2 = 0.25, dropout3 = 0.2, units = 20, activation = 1, 
                      alpha = 2, batchSize = 30, printIter = FALSE, plotMetrics = FALSE)

wheatFrcSVI <- fitDNN(y_train = y_train, y_test = y_test, x_train = x_train, x_test = x_test, Out_x_test = scenarioVIw, 
                     dropout1 = 0.2, dropout2 = 0.25, dropout3 = 0.2, units = 20, activation = 1, 
                     alpha = 2, batchSize = 30, printIter = FALSE, plotMetrics = FALSE)


frcScenarioI <- data.frame(Scenario = "I", Year = c(2021:2025), Oats = oatsFrcSI$forecasts, Corn = cornFrcSI$forecasts, 
                           Rice = riceFrcSI$forecasts, Wheat = wheatFrcSI$forecasts)

frcScenarioII <- data.frame(Scenario = "II",Year = c(2021:2025), Oats = oatsFrcSII$forecasts, Corn = cornFrcSII$forecasts, 
                           Rice = riceFrcSII$forecasts, Wheat = wheatFrcSII$forecasts)

frcScenarioIII <- data.frame(Scenario = "III",Year = c(2021:2025), Oats = oatsFrcSIII$forecasts, Corn = cornFrcSIII$forecasts, 
                            Rice = riceFrcSIII$forecasts, Wheat = wheatFrcSIII$forecasts)

frcScenarioIV <- data.frame(Scenario = "IV",Year = c(2021:2025), Oats = oatsFrcSIV$forecasts, Corn = cornFrcSIV$forecasts, 
                             Rice = riceFrcSIV$forecasts, Wheat = wheatFrcSIV$forecasts)

frcScenarioV <- data.frame(Scenario = "V",Year = c(2021:2025), Oats = oatsFrcSV$forecasts, Corn = cornFrcSV$forecasts, 
                            Rice = riceFrcSV$forecasts, Wheat = wheatFrcSV$forecasts)

frcScenarioVI <- data.frame(Scenario = "VI",Year = c(2021:2025), Oats = oatsFrcSVI$forecasts, Corn = cornFrcSVI$forecasts, 
                           Rice = riceFrcSVI$forecasts, Wheat = wheatFrcSVI$forecasts)

frcAll <- rbind(frcScenarioI, frcScenarioII, frcScenarioIII, frcScenarioIV, frcScenarioV, frcScenarioVI)
# write.csv(frcAll,"frcAll.csv")
frcScenarioAll_long <- gather(frcAll, Crop, Forecast, Oats:Wheat, factor_key=TRUE)

p1 <- ggplot(frcScenarioAll_long[1:30,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) + 
  theme_bw() +
  xlab("Year") + ylab("Oats yield") + 
  scale_x_continuous(breaks = c(2021:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p1


p2 <- ggplot(frcScenarioAll_long[31:60,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) +
  theme_bw() +
  xlab("Year") + ylab("Corn yield") +
  scale_x_continuous(breaks = c(2021:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p2

p3 <- ggplot(frcScenarioAll_long[61:90,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) +
  theme_bw() +
  xlab("Year") + ylab("Rice yield") +
  scale_x_continuous(breaks = c(2021:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p3

p4 <- ggplot(frcScenarioAll_long[91:120,], aes(x = Year, y = Forecast, group = Scenario)) +
  geom_line(size = 1, aes(color = Scenario)) + geom_point(aes(color = Scenario)) +
  geom_point(aes(color = Scenario)) +
  theme_bw() +
  xlab("Year") + ylab("Wheat yield") +
  scale_x_continuous(breaks = c(2021:2025)) +
  theme(text = element_text(size=16, color="black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
p4

figure <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2); figure 

# Calculate production using yield and crop area

yieldVI <- frcScenarioVI[,3:6] # Yield under Scenario VI
cropArea <- scenarioVI[2:6,c(3,2,4,5)] 
productionVI <- yieldVI*cropArea*1000 # Crop production under Scenario VI



