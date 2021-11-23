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
historico <- "5 Y"  3 Días a analizar
split.Ratio <- 0.85
accuracyDDBB <- NULL  # DF to save information
performanceDDBB <- NULL  # DF to save information
Investment <- 100000  # Amount of dollars to invest

Quote_Currenncy <- "EUR"
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
  purr::set_names(c("Open", "High", "Low", "Close"))
}

IBrokers::twsDisconnect(tws)

# Realizamos la conexión con H2O
h2o.init(max_mem_size = "2B", nthreads = -1)

# Creamos la base de datos con las variables a usarr en el algoritmo
data.ANN <- dataset.Precios %>%
  rownames_to_column(var = "Dates") %>%
  transmute ( Fechas = Dates,
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
  CpC = Closee - lag(Close, n=1),
  Return = ROC(Close, n=1, type="discrete"),
  P.Rise = ifelse(lead(Close, n=1) / Close > 1, 1, 0)) %>%
na.omit()

# Creamos la BBDD Training y Test


  
