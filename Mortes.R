library(datasets)
library(forecast)
library(stats)
library(pastecs)

?USAccDeaths
mortes <- datasets::USAccDeaths
ts.plot(mortes)
length(mortes)
trend.test(mortes)

fit <- auto.arima(mortes)
fit

#teste inicial
hw_mortes <- HoltWinters(mortes)
plot(hw_mortes)
lines(fit$fitted,col='blue')


#separando dados de teste
mortes
mortes_treino <- subset(mortes, end = 60)
mortes_treino
mortes_teste <- subset(mortes, start = 61)
mortes_teste

hw_mortes_treino <- HoltWinters(mortes_treino, seasonal = "add")  #a=0.73 b=0.01 c=1
hw_mortes_treino

arima_mortes_treino <- auto.arima(mortes_treino)
arima_mortes_treino


plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_mortes_treino, 12), col="red", lty=2)
lines(predict(arima_mortes_treino,12)$pred,col = 'blue', lty=2)
legend("top", inset=.05,
       c("Real","HW", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 
#FYI: nesse caso o arima ganhou 



# Cross-Validation

a = 0.1
b = 0.1
c = 0.1
STE_menor = 10^100


for(i in 1:9){
  b <- 0.1
  for(j in 1:9){
    c <- 0.1
    for(k in 1:9){
      hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=b, gamma=c, seasonal = "add")
      STE <- sum(predict(hw_cross, 12) - mortes_teste)^2
      if(STE < STE_menor){
        STE_menor <- STE
        A <- a
        B <- b
        C <- c
        }
      c <- c + 0.1
    }
    b <- b + 0.1
  }
  a = a + 0.1
}

STE_menor
A
B
C

hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_cross, 12), col="red", lty=2)
lines(predict(arima_mortes_treino,12)$pred,col = 'blue', lty=2)
legend("top", inset=.05,
       c("Real","HW", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 

sum(predict(hw_cross, 12) - mortes_teste)^2/var(mortes)
sum(predict(hw_mortes_treino, 12) - mortes_teste)^2/var(mortes)
sum(predict(arima_mortes_treino,12)$pred - mortes_teste)^2/var(mortes)






