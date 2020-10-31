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

hw_mortes_treino <- HoltWinters(mortes_treino, seasonal = "add") # a=0.78 b=0 c=0.43
hw_mortes_treino

arima_mortes_treino <- auto.arima(mortes_treino)    # ARIMA(0,1,1)(1,1,0)[12]
arima_mortes_treino


# ACF e PACF
acf(ts(mortes_treino), lag.max = 30)
pacf(ts(mortes_treino), lag.max = 30)

serie_diff=ts(diff(mortes_treino,lag = 1))
acf(serie_diff, lag.max = 30)
pacf(serie_diff, lag.max = 30)

serie_diff12 = diff(serie_diff, lag = 12)
acf(serie_diff12, lag.max = 30)
pacf(serie_diff12, lag.max = 30)

aj1 = arima(mortes_treino,c(0,1,0),seasonal = list(order=c(1,0,0),period = 12))
aj2 = arima(mortes_treino,c(0,1,1),seasonal = list(order=c(1,0,0),period = 12))
aj3 = arima(mortes_treino,c(0,1,1),seasonal = list(order=c(1,1,1),period = 12))
aj4 = arima(mortes_treino,c(0,1,1),seasonal = list(order=c(1,1,0),period = 12))  # igual ao auto.arima

SSE_aj1 <- sum((predict(aj1, 24)$pred - mortes_teste)^2)
SSE_aj2 <- sum((predict(aj2, 24)$pred - mortes_teste)^2)
SSE_aj3 <- sum((predict(aj3, 24)$pred - mortes_teste)^2)
SSE_aj4 <- sum((predict(aj4, 24)$pred - mortes_teste)^2)

AICs = c(AIC(aj1),AIC(aj2),AIC(aj3),AIC(aj4))
BICs = c(BIC(aj1),BIC(aj2),BIC(aj3),BIC(aj4))
SSEs <- c(SSE_aj1,SSE_aj2,SSE_aj3,SSE_aj4)
cbind(AICs,BICs,SSEs)

#podemos ver que o ajuste com menores erros é o 4 (auto.arima)

#Para a primeira parte do Arima, podemos ver que o acf quebra 



# Teste infantil inicial
{plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_mortes_treino, 24), col="red", lty=2)
lines(predict(aj4, 24)$pred,col = 'blue', lty=2)
legend("top", inset=.05,
       c("Real","HW_auto", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 
}
#FYI: nesse caso o arima ganhou (HW_auto é uma bosta)


# Cross-Validation

# niveis <- seq(from=0.01, to=1, by=0.01)
# n <- length(niveis)
# STE_menor = 10^100
# 
# for(i in 1:n){
#   a <- niveis[i]
#   for(j in 1:n){
#     b <- niveis[j]
#     for(k in 1:n){
#       c <- niveis[k]
#       hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=b, gamma=c, seasonal = "add")
#       STE <- sum( (predict(hw_cross, 24) - mortes_teste)^2 )
#       if(STE < STE_menor){
#         STE_menor <- STE
#         A <- a
#         B <- b
#         C <- c
#       }
#     }
#   }
# }

# STE_menor
# # step:  0.1  |  0.05  |  0.01
# A      # 0.1  |  0.1   |  0.02
# B      # 0.5  |  0.85  |  0.65
# C      # 0.5  |  0.25  |  0.35

A <- 0.02
B <- 0.65
C <- 0.35


hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
{plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_cross, 24), col="red", lty=2)
lines(predict(arima_mortes_treino, 24)$pred,col = 'blue', lty=2)
legend("top", inset=.05,
       c("Real","HW_CV", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 
}

SSE_HW_CV <- sum((predict(hw_cross, 24) - mortes_teste)^2)                       # HW Cross-Validation
SSE_arima_auto <- sum((predict(arima_mortes_treino, 24)$pred - mortes_teste)^2)  # ARIMA automatico
SSE_HW_auto <- sum((predict(hw_mortes_treino, 24) - mortes_teste)^2)             # HW automatico

cbind(SSE_HW_CV, SSE_arima_auto, SSE_HW_auto)

{plot(c(SSE_HW_CV, SSE_arima_auto, SSE_HW_auto), col=c("green","dark orange","red"), type = "p", main = "Test Error", ylab="SSE")
legend("top", inset=.05, c("HW Cross-Validation", "ARIMA automatico","HW automatico"), lty = 1, 
       col=c("green","dark orange","red")) 

segments(x0=1, y0=SSE_HW_CV, x1=2, y1=SSE_arima_auto, lty=3)
segments(x0=2, y0=SSE_arima_auto, x1=3, y1=SSE_HW_auto, lty=3)
}




