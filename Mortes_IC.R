library(datasets)
library(forecast)
library(stats)
library(pastecs)

mortes <- datasets::USAccDeaths
plot.ts(mortes, bty="n")

# Separando dados de teste

mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49, end = 60)
mortes_teste
mortes_prev <- subset(mortes, start = 61)
mortes_prev


A <- 0.09
B <- 0.38
C <- 0.82
# seasonal = add

hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
EQM_HW <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
EQM_HW

hw_pred <- predict(hw_cross, 24)
subset(hw_pred, start = 13)

EQM_HW_prev <- sum( (subset(hw_pred, start = 13) - mortes_prev)^2 )/12
EQM_HW_prev

EAM_HW_pred <- sum( abs(subset(hw_pred, start = 13) - mortes_prev) )/12
EAM_HW_pred


{
  plot.ts(subset(mortes, start=37), ylim=c(7000, 10500), bty="n", ylab = "Mortes")
  lines(subset(hw_pred, end=12), col="blue", lty=2)
  lines(subset(hw_pred, start=13), col="blue", lty=3)
  legend(x=1976.5,y=10800,cex=0.9,
         c("Real","HW_teste", "HW_prev"), lwd=1, lty=c(1,2,3), col=c("black","blue","blue"), bty="n") 
}


# Intervalo de Confiança

fitted <- hw_cross$fitted[,1]  # anos de 1974 a 1976 (primeiro ano nao ajusta)
original <- subset(mortes_treino, start = 13)

erros <- fitted - original
plot(erros, type="p")          # aparentam ser correlacionados

qqnorm(erros, bty="n")#, main = "")
qqline(erros)        # normalidade relativamente ok

v <- var(erros)

e_1 <- as.vector(subset(erros, start = 2))
e_2 <- as.vector(subset(erros, end = 35))
cor(e_1,e_2)
acf(erros)

# soma de nu_i^2

k <- 1:24
N <- 12

nu_1 <- 1 + (k-1)*A^2 * (1 + k*B + k*(2*k-1)*B^2/6)
nu_2 <- floor(k/N) * ( C^2*(1-A)^2 + A*C*(1-A)*(2 + N*B*(floor(k/N)+1)) )

nu <- nu_1 + nu_2
nu

z <- qnorm(0.975)

interval <- z * sqrt(nu*v)

hw_sup <- hw_pred + interval
hw_inf <- hw_pred - interval



# SARIMA

M <- -0.46
N <- -0.14

arima_cross <- arima(mortes_treino,c(0,1,1),seasonal = list(order = c(1,1,0),period = 12),fixed = c(M,N))
EQM_Arima <- sum( (predict(arima_cross, 12)$pred - mortes_teste)^2 )/12
EQM_Arima

arima_pred <- predict(arima_cross, 24)
subset(arima_pred$pred, start = 13)

EQM_Arima_prev <- sum( (subset(arima_pred$pred, start = 13) - mortes_prev)^2 )/12
EQM_Arima_prev

EAM_Arima_prev <- sum( abs(subset(arima_pred$pred, start = 13) - mortes_prev) )/12
EAM_Arima_prev


ypred_cross = arima_pred$pred
qinf_cross= ypred_cross - qnorm(.975)*arima_pred$se
qsup_cross= ypred_cross + qnorm(.975)*arima_pred$se



######## Com intervalo de confiança ##########
{
  plot.ts(subset(mortes, start=37), ylim=c(4000, 15000), bty="n", ylab = "Mortes")
  lines(hw_pred, col="blue")
  lines(hw_sup, col="blue", lty=3)
  lines(hw_inf, col="blue", lty=3)
  lines(arima_pred$pred, col="red")
  lines(qinf_cross,col='red', lty=3)
  lines(qsup_cross,col='red', lty=3)
  legend("topleft", inset=.05,
         c("Real","Holt-Winters", "SARIMA"), lwd=1, lty=1, col=c("black","blue","red"), bty="n") 
}


######## Sem intervalo de confiança ##########
{
  plot.ts(subset(mortes, start=37), ylim=c(7000, 10500), bty="n", ylab = "Mortes")
  lines(subset(hw_pred, end=12), col="blue", lty=2)
  lines(subset(hw_pred, start=13),col = "blue", lty=3)
  lines(subset(arima_pred$pred,end = 12), col="red", lty=2)
  lines(subset(arima_pred$pred,start = 13), col="red", lty=3)
  legend(x=1976.5,y=10800,cex=0.9,
         c("Real","Holt-Winters", "SARIMA"), lwd=1, lty=1, col=c("black","blue","red"), bty="n") 
}




erros_arima <- arima_cross$residuals[13:48]# anos de 1974 a 1976 (primeiro ano nao ajusta)
plot(erros_arima, type="p")          # aparentam ser não correlacionados

qqnorm(erros_arima, bty="n")#, main = "")
qqline(erros_arima)        # normalidade relativamente ok

v <- var(erros_arima)

e_1 <- as.vector(erros_arima[2:36])
e_2 <- as.vector(erros_arima[1:35])
cor(e_1,e_2)
acf(erros_arima)

