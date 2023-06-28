#Depois de toda a Análise de todos os modelos que criamos e testamos, chegamos a conclusão
#que o modelo que melhor encaixa nos nossos dados é o ARIMA(2,1,1), portanto é este modelo
#que usaremos para fazer a previsão

#Como nossos dados só vão até o dia 23/06/2023, vamos fazer a previsão durante 7 dias, essas previsões
#serão as temperaturas médias dos dias 24/06/2023 até 30/06/2023

#Lendo os pacotes
library(forecast)

#Lendo os dados
local = "D:/au"

setwd(local) #Setando diretório

dados = read_csv2("dadosClimaticos_164.csv", skip = 7)

serie = ts(rev(dados$tMed), start = c(2019, 1), frequency = 365.25)

#Fazendo a previsão

forecast(arima(serie, order = c(2,1,1)), h=1)

