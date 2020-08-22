library(datasets)
library(forecast)
library(stats)
trend.test(mortes)


?USAccDeaths
mortes <- datasets::USAccDeaths
plot(mortes)
length(mortes)

fit <- auto.arima(mortes)
fit

#teste inicial
hw_mortes <- HoltWinters(mortes)
plot(hw_mortes)
lines(fit$fitted,col='blue')


#separando dados de teste
mortes
mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49)
mortes_teste

hw_mortes_treino <- HoltWinters(mortes_treino, seasonal = "add")
hw_mortes_treino

arima_mortes_treino <- auto.arima(mortes_treino)
arima_mortes_treino


plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_mortes_treino, 24), col="red")
lines(predict(arima_mortes_treino,24)$pred,col = 'blue')
legend("top", inset=.05,
       c("Prevista","Real"), lwd=1, lty=1, col=c("red","black")) 
#FYI: nesse caso o arima ganhou 




#https://rpubs.com/davoodastaraky/TSA1
?decompose

decompose(mortes)
plot(decompose(mortes))
