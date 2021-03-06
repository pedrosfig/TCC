library(datasets)
library(forecast)
#library(stats)
#library(pastecs)

?USAccDeaths
mortes <- datasets::USAccDeaths
plot.ts(mortes, bty="n", ylab="Mortes", xlab="Ano")   # Mortes.jpeg
length(mortes)

fit <- auto.arima(mortes)
fit

#teste inicial
hw_mortes <- HoltWinters(mortes)
plot(hw_mortes, bty="n")
lines(fit$fitted,col='blue')


#separando dados de teste
mortes
mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49, end = 60)
mortes_teste
mortes_prev <- subset(mortes, start = 61)
mortes_prev

hw_mortes_treino <- HoltWinters(mortes_treino, seasonal = "add") # a=0.78 b=0 c=0.43
hw_mortes_treino

arima_mortes_treino <- auto.arima(mortes_treino)    # ARIMA(0,1,1)(1,1,0)[12]
arima_mortes_treino


# ACF e PACF
acf(ts(mortes_treino), lag.max = 36,main = "",bty = 'n',ylab = 'FAC')   #(AR(1)),12(AR(1))
legend(x="topright",cex=0.9,c("Correlação","Linha de Significância"), lwd=1, lty=1, col=c("black","blue"), bty="n") 
pacf(ts(mortes_treino), lag.max = 36,main = "",bty = 'n',ylab = 'FACP')
legend(x="topright",cex=0.9,c("Correlação","Linha de Significância"), lwd=1, lty=1, col=c("black","blue"), bty="n") 

serie_diff=ts(diff(mortes_treino,lag = 1))
acf(serie_diff, lag.max = 36,main = "",bty = 'n',ylab = 'FAC')
legend(x="topright",cex=0.9,c("Correlação","Linha de Significância"), lwd=1, lty=1, col=c("black","blue"), bty="n") 
pacf(serie_diff, lag.max = 36,main = "",bty = 'n',ylab = 'FACP')
legend(x=20,y=0.3,cex=0.9,c("Correlação","Linha de Significância"), lwd=1, lty=1, col=c("black","blue"), bty="n") 

serie_diff12 = diff(serie_diff, lag = 12)
acf(serie_diff12, lag.max = 36,main = "",bty = 'n',ylab = 'FAC')
legend(x="topright",cex=0.9,c("Correlação","Linha de Significância"), lwd=1, lty=1, col=c("black","blue"), bty="n") 
pacf(serie_diff12, lag.max = 36,main = "",bty = 'n',ylab = 'FACP')
legend(x="topright",cex=0.9,c("Correlação","Linha de Significância"), lwd=1, lty=1, col=c("black","blue"), bty="n") 

aj1 = arima(mortes_treino,c(1,0,0),seasonal = list(order=c(1,0,0),period = 12))
aj2 = arima(mortes_treino,c(0,1,0),seasonal = list(order=c(1,0,0),period = 12)) 
aj3 = arima(mortes_treino,c(1,1,0),seasonal = list(order=c(0,1,0),period = 12))
aj4 = arima(mortes_treino,c(0,1,1),seasonal = list(order=c(1,1,0),period = 12))  # igual ao auto.arima
aj5 = arima(mortes_treino,c(0,1,13),seasonal = list(order=c(0,1,0),period = 12))

EQM_aj1 <- sum((predict(aj1, 12)$pred - mortes_teste)^2)/12
EQM_aj2 <- sum((predict(aj2, 12)$pred - mortes_teste)^2)/12
EQM_aj3 <- sum((predict(aj3, 12)$pred - mortes_teste)^2)/12
EQM_aj4 <- sum((predict(aj4, 12)$pred - mortes_teste)^2)/12
EQM_aj5 <- sum((predict(aj5, 12)$pred - mortes_teste)^2)/12

EAM_aj1 <- sum(abs(predict(aj1, 12)$pred - mortes_teste))/12
EAM_aj2 <- sum(abs(predict(aj2, 12)$pred - mortes_teste))/12
EAM_aj3 <- sum(abs(predict(aj3, 12)$pred - mortes_teste))/12
EAM_aj4 <- sum(abs(predict(aj4, 12)$pred - mortes_teste))/12
EAM_aj5 <- sum(abs(predict(aj5, 12)$pred - mortes_teste))/12

EQMR_aj1 <- sqrt(mean((predict(aj1, 12)$pred - mortes_teste)^2/mortes_teste^2))
EQMR_aj2 <- sqrt(mean((predict(aj2, 12)$pred - mortes_teste)^2/mortes_teste^2))
EQMR_aj3 <- sqrt(mean((predict(aj3, 12)$pred - mortes_teste)^2/mortes_teste^2))
EQMR_aj4 <- sqrt(mean((predict(aj4, 12)$pred - mortes_teste)^2/mortes_teste^2))
EQMR_aj5 <- sqrt(mean((predict(aj5, 12)$pred - mortes_teste)^2/mortes_teste^2))

AICs <- c(AIC(aj1),AIC(aj2),AIC(aj3),AIC(aj4),AIC(aj5))
BICs <- c(BIC(aj1),BIC(aj2),BIC(aj3),BIC(aj4),BIC(aj5))
EQMs <- c(EQM_aj1,EQM_aj2,EQM_aj3,EQM_aj4,EQM_aj5)
EAMs <- c(EAM_aj1,EAM_aj2,EAM_aj3,EAM_aj4,EAM_aj5)
EQMRs <- c(EQMR_aj1,EQMR_aj2,EQMR_aj3,EQMR_aj4,EQMR_aj5)
ajuste <- c("aj1", "aj2", "aj3", "aj4","aj5")
data.frame(cbind(AICs,BICs,EQMs,EAMs,EQMRs), row.names=ajuste)
 

#podemos ver que o ajuste com menores erros é o 4 (auto.arima)

#Para a primeira parte do Arima, podemos ver que o acf quebra 



# Teste infantil inicial

HW_pred <- predict(hw_mortes_treino, 24, prediction.interval = T, level = 0.95)

pred = predict(aj4, 12)
ypred = pred$pred
qinf= ypred - qnorm(.975)*pred$se
qsup= ypred + qnorm(.975)*pred$se


{plot.ts(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes", bty="n")
      lines(HW_pred[,1], col="red", lty=1)      # HW_pred fit
      lines(HW_pred[,2], col="red", lty=3)      # HW_pred upr
      lines(HW_pred[,3], col="red", lty=3)      # HW_pred lwr
      lines(predict(aj4, 24)$pred,col = 'blue', lty=1)
      lines(qinf,col = 'blue',lty = 3)                  # sarima_pred lwr
      lines(qsup,col = 'blue',lty = 3)                  # sarima_pred upr
      legend("top", inset=.05,
             c("Real","HW_auto", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue"), bty="n") 
}

#FYI: nesse caso o arima ganhou (HW_auto é uma bosta)


# # Cross-Validation
# 
# niveis <- seq(from=0.01, to=1, by=0.01)
# n <- length(niveis)
# EQM_menor = 10^100
# 
# for(i in 1:n){
#   a <- niveis[i]
#   for(j in 1:n){
#     b <- niveis[j]
#     for(k in 1:n){
#       c <- niveis[k]
#       hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=b, gamma=c, seasonal = "mult")
#       EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
#       if(EQM < EQM_menor){
#         EQM_menor <- EQM
#         A <- a
#         B <- b
#         C <- c
#       }
#     }
#   }
# }
# 
# EQM_menor
# 
# # step:  0.1  |  0.05  |  0.01  | 0.01 (mult)
# A      # 0.1  |  0.10  |  0.09  |  0.09
# B      # 0.4  |  0.35  |  0.38  |  0.39
# C      # 0.7  |  0.80  |  0.82  |  0.80
# # EQM_menor     #      | 30344  | 28750  

A <- 0.09
B <- 0.39
C <- 0.80

hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "mult")
EQM_Holt <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
EQM_Holt

hw_pred <- predict(hw_cross, 24)
subset(hw_pred, start = 13)

EQM_Holt_prev <- sum( (subset(hw_pred, start = 13) - mortes_prev)^2 )/12
EQM_Holt_prev

EQMR_Holt_prev <- sqrt(mean((subset(hw_pred, start = 13) - mortes_prev)^2/mortes_prev^2))
EQMR_Holt_prev
  
{
   marks <- c(1976, 1977, 1978, 1979)
   plot.ts(subset(mortes, start=37), ylim=c(7000, 10500),       # Mortes_HW_mult.jpeg
           bty="n", ylab = "Mortes", xlab = "Ano", xaxt="n")
   lines(subset(hw_pred, end=12), col="blue", lty=2)
   lines(subset(hw_pred, start=13), col="blue", lty=3)
   legend(x=1976.5,y=10800,cex = 0.9,
          c("Real","HW_teste", "HW_prev"), lwd=1, lty=c(1,2,3), col=c("black","blue","blue"), bty="n") 
   axis(1,at=marks,labels=formatC(marks, digits = 4))
}



####### Cross-Validation do arima 

# aj4$coef
# teste = arima(mortes_treino,c(0,1,1),seasonal = list(order = c(1,1,0),period = 12),fixed = c(-0.3868365,-0.4418667) )
# teste$coef

# niveis <- seq(from= -1, to=1, by=0.02)
# l <- length(niveis)
# EQM_menor = 10^100
# 
#  for(i in 1:l){
#    m <- niveis[i]
#    for(j in 1:l){
#      n <- niveis[j]
#        arima_cross <- arima(mortes_treino,c(0,1,1),seasonal = list(order = c(1,1,0),period = 12),fixed = c(m,n))
#        EQM <- sum( (predict(arima_cross, 12)$pred - mortes_teste)^2 )/12
#        if(EQM < EQM_menor){
#          EQM_menor <- EQM
#          M <- m
#          N <- n
#        }
#    }
#  }

# EQM_menor (a partir do aj2)
#  step:   0.2   |  0.1  |  0.02
# M       -0.4  |  -0.5 |  -0.46
# N       -0.2  |  -0.1 |  -0.14
# Original: -0.387 -0.442 

M <- -0.46
N <- -0.14

arima_cross <- arima(mortes_treino,c(0,1,1),seasonal = list(order = c(1,1,0),period = 12),fixed = c(M,N))
EQM_Arima <- sum( (predict(arima_cross, 12)$pred - mortes_teste)^2 )/12
EQM_Arima

arima_pred <- predict(arima_cross, 24)$pred
subset(arima_pred, start = 13)

EQM_Arima_prev <- sum( (subset(arima_pred, start = 13) - mortes_prev)^2 )/12
EQM_Arima_prev

EQM_Arima_prev <- sqrt(mean((subset(arima_pred, start = 13) - mortes_prev)^2/mortes_prev^2))
EQM_Arima_prev


{
   marks <- c(1976, 1977, 1978, 1979)
   plot.ts(subset(mortes, start=37), ylim=c(7000, 10500),       # Mortes_SARIMA.jpeg
           bty="n", ylab = "Mortes", xlab = "Ano", xaxt="n")
   lines(subset(arima_pred, end=12), col="red", lty=2)
   lines(subset(arima_pred, start=13), col="red", lty=3)
   legend(x = 1976.46,y=10800,cex=0.8,
          c("Real","SARIMA_teste", "SARIMA_prev"), lwd=1, lty=c(1,2,3), col=c("black","red","red"), bty="n") 
   axis(1,at=marks,labels=formatC(marks, digits = 4))
}




