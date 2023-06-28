library(tidyverse)
library(tsibble)
library(lubridate)
library(fpp3)
library(forecast)
library(fable)


#local do arquivo

local = "D:/au"

setwd(local) #Setando diretório

#Lendo arquivo

dados = read_csv2("dadosClimaticos_164.csv", skip = 7)

serie = ts(rev(dados$tMed), start = c(2019, 1), frequency = 365.25)

dados_ts <- dados %>% select(data, tMed) #Selecionando as variáveis

dados_ts$data <- as_date(dados_ts$data, format = "%d/%m/%Y")

dados_ts <- as_tsibble(dados_ts, index = data)

##### MODELO DE REGRESSAO #####

##### MODELOS AR #####

#AR1

AR1 <- arima(serie, order = c(1,0,0))

autoplot(serie) +
  autolayer(fitted(AR1), series="Fitted")

checkresiduals(AR1)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(1,0,0)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_AR1 <- mean(errorArima^2, na.rm = TRUE)

#AR2

AR2 <- arima(serie, order = c(2,0,0))

autoplot(serie) +
  autolayer(fitted(AR2), series="Fitted")

checkresiduals(AR2)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(2,0,0)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_AR2 <-  mean(errorArima^2, na.rm = TRUE)

#AR3

AR3 <- arima(serie, order = c(3,0,0))

autoplot(serie) +
  autolayer(fitted(AR3), series="Fitted")

checkresiduals(AR3)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(3,0,0)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_AR3 <- mean(errorArima^2, na.rm = TRUE)

##### Modelo MA #####   

#MA1

MA1 <- arima(serie, order = c(0,0,1))

autoplot(serie) +
  autolayer(fitted(MA1), series="Fitted")

checkresiduals(MA1)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(0,0,1)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_MA1 <-  mean(errorArima^2, na.rm = TRUE)

#MA2

MA2 <- arima(serie, order = c(0,0,2))

autoplot(serie) +
  autolayer(fitted(MA2), series="Fitted")

checkresiduals(MA2)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(0,0,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_MA2 <- mean(errorArima^2, na.rm = TRUE)

#MA3

MA3 <- arima(serie, order = c(0,0,3))

autoplot(serie) +
  autolayer(fitted(MA3), series="Fitted")

checkresiduals(MA3)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(0,0,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_MA3 <- mean(errorArima^2, na.rm = TRUE)


##### Modelo ARMA #####

#ARMA11

ARMA11 <- arima(serie, order = c(1,0,1))

autoplot(serie) +
  autolayer(fitted(ARMA11), series="Fitted")

checkresiduals(ARMA11)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(1,0,1)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA11 <- mean(errorArima^2, na.rm = TRUE)

#ARMA21

ARMA21 <- arima(serie, order = c(2,0,1))

autoplot(serie) +
  autolayer(fitted(ARMA21), series="Fitted")

checkresiduals(ARMA21)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(2,0,1)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA21 <- mean(errorArima^2, na.rm = TRUE)

#ARMA31

ARMA31 <- arima(serie, order = c(3,0,1))

autoplot(serie) +
  autolayer(fitted(ARMA31), series="Fitted")

checkresiduals(ARMA31)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(3,0,1)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA31 <- mean(errorArima^2, na.rm = TRUE)

#ARMA12

ARMA12 <- arima(serie, order = c(1,0,2))

autoplot(serie) +
  autolayer(fitted(ARMA12), series="Fitted")

checkresiduals(ARMA12)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(1,0,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA12 <- mean(errorArima^2, na.rm = TRUE)

#ARMA13

ARMA13 <- arima(serie, order = c(1,0,3))

autoplot(serie) +
  autolayer(fitted(ARMA13), series="Fitted")

checkresiduals(ARMA13)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(1,0,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA13 <- mean(errorArima^2, na.rm = TRUE)

#ARMA22

ARMA22 <- arima(serie, order = c(2,0,2))

autoplot(serie) +
  autolayer(fitted(ARMA22), series="Fitted")

checkresiduals(ARMA22)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(2,0,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA22 <- mean(errorArima^2, na.rm = TRUE)

#ARMA23

ARMA23 <- arima(serie, order = c(2,0,3))

autoplot(serie) +
  autolayer(fitted(ARMA23), series="Fitted")

checkresiduals(ARMA23)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(2,0,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA23 <- mean(errorArima^2, na.rm = TRUE)

#ARMA32

ARMA32 <- arima(serie, order = c(3,0,2))

autoplot(serie) +
  autolayer(fitted(ARMA32), series="Fitted")

checkresiduals(ARMA32)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(3,0,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA32 <- mean(errorArima^2, na.rm = TRUE)

#ARMA33

ARMA33 <- arima(serie, order = c(3,0,3))

autoplot(serie) +
  autolayer(fitted(ARMA33), series="Fitted")

checkresiduals(ARMA33)

#Cross Validation
fArima <- function(x, h) {
  forecast(arima(x, order = c(3,0,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARMA33 <- mean(errorArima^2, na.rm = TRUE)

##### MODELO ARIMA #####

for (p1 in 1:3) {
  for (d1 in 1:3) {
    for (q1 in 1:3) {
      order <- c(p1, d1, q1)
      modelo <- arima(serie, order = order)
      autoplot(serie) +
        autolayer(fitted(modelo), series = "Fitted")
      checkresiduals(modelo)
    }
  }
}

#Cross Validation ARIMA (1,1,2)

fArima <- function(x, h) {
  forecast(arima(x, order = c(1,1,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA112 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (1,1,3)

fArima <- function(x, h) {
  forecast(arima(x, order = c(1,1,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA113 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (1,2,3)

fArima <- function(x, h) {
  forecast(arima(x, order = c(1,2,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA123 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (2,1,1)

fArima <- function(x, h) {
  forecast(arima(x, order = c(2,1,1)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA211 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (2,1,2)

fArima <- function(x, h) {
  forecast(arima(x, order = c(2,1,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA212 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (2,1,3)

fArima <- function(x, h) {
  forecast(arima(x, order = c(2,1,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA213 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (2,2,2)

fArima <- function(x, h) {
  forecast(arima(x, order = c(2,2,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA222 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (3,1,1)

fArima <- function(x, h) {
  forecast(arima(x, order = c(3,1,1)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA311 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (3,1,2)

fArima <- function(x, h) {
  forecast(arima(x, order = c(3,1,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA312 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (3,2,2)

fArima <- function(x, h) {
  forecast(arima(x, order = c(3,2,2)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA322 <- mean(errorArima^2, na.rm = TRUE)

#Cross Validation ARIMA (3,2,3)

fArima <- function(x, h) {
  forecast(arima(x, order = c(3,2,3)), h=h)
}

errorArima <- tsCV(serie, fArima, window = 365, h = 1)

MSE_ARIMA323 <- mean(errorArima^2, na.rm = TRUE)

nomes = c("AR1", "AR2", "AR3", "MA1", "MA2", "MA3", "ARMA11", "ARMA12", 
          "ARMA13", "ARMA21" ,"ARMA22" ,"ARMA23", "ARMA31", "ARMA32", "ARMA33",
          "ARIMA112", "ARIMA113", "ARIMA123", "ARIMA211", "ARIMA212", "ARIMA213",
          "ARIMA222", "ARIMA311", "ARIMA312", "ARIMA322", "ARIMA323")

valores = c(MSE_AR1, MSE_AR2, MSE_AR3, MSE_MA1, MSE_MA2, MSE_MA3, MSE_ARMA11, MSE_ARMA12, 
            MSE_ARMA13, MSE_ARMA21, MSE_ARMA22, MSE_ARMA23, MSE_ARMA31, MSE_ARMA32, MSE_ARMA33,
            MSE_ARIMA112, MSE_ARIMA113, MSE_ARIMA123, MSE_ARIMA211, MSE_ARIMA212, MSE_ARIMA213,
            MSE_ARIMA222, MSE_ARIMA311, MSE_ARIMA312, MSE_ARIMA322, MSE_ARIMA323)

tabela_MSE <- data.frame(Modelos = nomes, MSE = valores)



