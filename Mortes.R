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
mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49)
mortes_teste

hw_mortes_treino <- HoltWinters(mortes_treino, seasonal = "add")  #a=0.78 b=0 c=0.43
hw_mortes_treino

arima_mortes_treino <- auto.arima(mortes_treino)
arima_mortes_treino


plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_mortes_treino, 24), col="red", lty=2)
lines(predict(arima_mortes_treino, 24)$pred,col = 'blue', lty=2)
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
      STE <- sum((predict(hw_cross, 24) - mortes_teste)^2)
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
lines(predict(hw_cross, 24), col="red", lty=2)
lines(predict(arima_mortes_treino, 24)$pred,col = 'blue', lty=2)
legend("top", inset=.05,
       c("Real","HW_CV", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 

SSE_HW_CV <- sum((predict(hw_cross, 24) - mortes_teste)^2)                       # HW Cross-Validation
SSE_arima_auto <- sum((predict(arima_mortes_treino, 24)$pred - mortes_teste)^2)  # ARIMA automatico
SSE_HW_auto <- sum((predict(hw_mortes_treino, 24) - mortes_teste)^2)             # HW automatico


{plot(c(SSE_HW_CV, SSE_arima_auto, SSE_HW_auto), col=c("blue","red","dark orange"), type = "p", main = "Test Error", ylab="SSE")
legend("top", inset=.05, c("HW Cross-Validation", "ARIMA automatico","HW automatico"), lty = 1, 
       col=c("blue","red","dark orange")) 
}
segments(x0=1, y0=SSE_HW_CV, x1=2, y1=SSE_arima_auto, lty=3)
segments(x0=2, y0=SSE_arima_auto, x1=3, y1=SSE_HW_auto, lty=3)





