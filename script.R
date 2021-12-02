# Librerias
if (!require("IBrokers")) install.packages("IBrokers"); library(IBrokers)
if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("h2o")) install.packages("h2o"); library(h2o)
if (!require("caret")) install.packages("caret"); library(caret)
if (!require("reshape2")) install.packages("reshape2"); library(reshape2)
if (!require("gridExtra")) install.packages("gridExtra"); library(gridExtra)
if (!require("scales")) install.packages("scales"); library(scales)

# Realizamos la descarga de datos
tf <- "1 hour"  # Temporalidad '15 mins', '30 mins', '1 hour', '1 day'
historico <- "5 Y"  # 3 Días a analizar
split.Ratio <- 0.85
accuracyDDBB <- NULL  # DF to save information
performanceDDBB <- NULL  # DF to save information
Investment <- 100000  # Amount of dollars to invest

Quote_Currency <- "EUR"
Price_Currency <- "USD"

# Ralizamos la conexión con IB
tws <- IBrokers::twsConnect(clientId = 240312, port = 7496)

if (isConnected(tws)){
  
  # Creamos los contratos para la conexión con IB
  activo <- IBrokers::twsCurrency(Quote_Currency, currency = Price_Currency)
  
  # Descargamos la data
  dataset.Precios <- IBrokers::reqHistoricalData(tws,
                                                 Contract = activo,
                                                 whatToShow = 'BID',
                                                 useRTH = "0",
                                                 barSize = tf,
                                                 duration = historico) %>%
  as.data.frame() %><>
  dplyr::select(1:4) %>%
  purrr::set_names(c("Open", "High", "Low", "Close"))
}

IBrokers::twsDisconnect(tws)

# Realizamos la conexión con H2O
h2o.init(max_mem_size = "2B", nthreads = -1)

# Creamos la base de datos con las variables a usarr en el algoritmo
data.ANN <- dataset.Precios %>%
  rownames_to_column(var = "Dates") %>%
  transmute (Fechas = Dates,
  OH = Open - High,
  OL = Open - Low,
  A_1 = SMA(Close, n = 3),
  A_2 = SMA(Close, n = 6),
  A_3 = SMA(Close, n = 9),
  R_1 = RSI(Close, n = 5),
  R_2 = RSI(Close, n = 15),
  R_3 = RSI(Close, n = 21),
  Delta_1 = Close - SMA(Close, n=3),
  Delta_2 = Cloase - SMA(Close, n=6),
  Delta_3 = Close - SMA(Close, n=9),
  PricesMean = (High + Open + Low + Close)/4,
  OpO = Open - lag(Open, n=1),
  OpC = Open - lag(Cloase, n=1),
  CpC = Close - lag(Close, n=1),
  Return = ROC(Close, n=1, type="discrete"),
  P.Rise = ifelse(lead(Close, n=1) / Close > 1, 1, 0)) %>%
na.omit()

# Creamos la BBDD Training y Test
trainset <- data.ANN %>% dplyr::slice(1:floor(split.Ratio * nrow(data.ANN)))
testset <- data.ANN %>% dplyr::slice((floor(split.Ratio * nrow(data.ANN)) + 1):n())

# Creamos la Red Neuronal
model.ANN = h2o.deeplearning(y = "P_Rise",
                             training_frame = trainset %>% dplyr::select(-Fechas) %>% as.h2o(),
                             distribution = "AUTO",
                             activation = "Rectifier",
                             ignore_const_cols = FALSE,
                             hidden = c(5,5,5,5),
                             epochs = 250,
                             train_samplles_per_iteration = -2)

# Predicción del Modelo
P.Rise_Pred <- h2o.predict(model.ANN,
                           newdata = testset %>% dplyr::select(-c(Fechas, P.Rise)) %>% as.h2o()) %>% as.vector()

# Desconectamos todo
h2o.shutdown(prompt = FALSE)

# Calculamos el nivel de importancia que cada variable tiene
db_Importancia <- data.frame(Variables = model.ANN@model$variable_importaances[,1],
                             Importancia = model.AN@model$variable_importances[,4]) %>%
  dplyr::arrange(desc(Importacia)) %>%
  dplyr::mutate(Importancia = round(Importancia * 100, digits=2))

# Calculamos el umbral óptimo para tomar decissiones de inversión
pasos <- se1(from = 0.01, to=0.999, by=0.01)
acc <- data.frame(matrix(0, length(pasos), 2))
k <- 1
for (i in pasos){
  P.Rise_Opt <- ifelse(P.Rise_Pred > i, 1, 0)9  # Normalizamos
  cm <- table(testset$P.Rise, P.Rise_Opt)
  if(dim(cm)[2] == 1){
    acc[k,1] <- 0
  }else{
    acc[k,1] <- (cm[1,1] + cm[2,2])/sum(cm)  
  }
  acc[k,2] <- i
  k <- k + 1
}

acc <- acc %>% purrr::set_names(c("Eficiencia", "Umbral"))

acc %>%
  ggploot(aes(x=Umbra, y=Eficiencia)) +
  geom_line()

# Buscamos la sensibilidad óptima
sensibilidad.Optima <- acc[which.max(acc$Eficiencia),2]
eficiencia.Moddelo <- round(acc[which(acc$Umbral == sensibilidad.Optima), 1]*100, digits = 2)
 
# Calculamos las predicciiones ajustadas a la sensibilidad optima
P.Rise_Pred_cm <- ifelse(P.Rise_Pred > sensibilidad.Optima, 1, 0)

# adjuntamos las predicciones al testset
testset <- testset %>%
  dplyr::mutate(P.Rise_Pred = P.Rise_Pred_cm,
                Action = ifelse(P.Rise_Pred == 1, "BUY", "SELL"))

# Realizamos el backtest
db_backtest <- testset %>%
  dplyr::select(Fechas. Return, Action) %>%
  dplyr::mutate(Signal = ifelse(Action == "BUY", 1, -19 %>% dplyr::lag(n=1),
                         Strategy_Return = Signal * Return,
                                NAV = Investment) %>%
  replace(is.na(.), 0)

for(i in 2:nrow(db_backtest)){
  # NAV and DD for the Strategy
  db_backtest$NAV[i] <- db_backtest$NAV[i-1]*(1 + db_backtest$Strategy_Return[i])
}

# Generamos el gráfico de performance
db_backtest %>%
  as_tibble() %>%
  dplyr::mutate(Fecchas = as_datetime(Fechas)) %>%
  ggplot(aes(x = Fechas, y=NAV) +
    geom_line() +
    theme_tq() +
    labs(title = "Performance de la estrategia",
         subtitle = "Decisiones de inversión cada hora",
         caption = "Por: Carlos Jiménez",
         x = "",
         y = "") +
    theme(legend.position = "none") +
    scale_y_continuous(labels = scales::dollar)
         
# Deploy
# for(){
#   ANN_FX_TRADING
#   Generate a DB -> MODEL, PARAM, ACC, ANYTHING ELSE!!!!
# }         
