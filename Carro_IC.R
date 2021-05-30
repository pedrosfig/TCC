library(datasets)
library(forecast)


carro <- datasets::UKDriverDeaths
plot.ts(carro, bty="n", ylab="Acidentes")

# Separando dados de teste

carro_treino <- subset(carro, end = 180)
carro_treino
carro_teste <- subset(carro, start = 181)
carro_teste



A <- 0.08
B <- 0.01
C <- 0.26

hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
EQM




# Intervalo de Confiança

hw_pred <- predict(hw_cross, 12)

fitted <- hw_cross$fitted[,1]  # anos de 1970 a 1983 (primeiro ano nao ajusta)
original <- subset(carro_treino, start = 13)

erros_HW <- fitted - original
plot(erros_HW, type="p")          # aparentam ser correlacionados

qqnorm(erros_HW, bty="n", main = "",           # Mortes_HW_qqplot.jpeg
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais")
qqline(erros_HW)        # normalidade relativamente ok

v <- var(erros_HW)

e_1 <- as.vector(subset(erros_HW, start = 2))
e_2 <- as.vector(subset(erros_HW, end = 167))
cor(e_1,e_2)
acf(erros_HW)

# soma de nu_i^2

k <- 1:12
N <- 12

nu_1 <- 1 + (k-1)*A^2 * (1 + k*B + k*(2*k-1)*B^2/6)
nu_2 <- floor(k/N) * ( C^2*(1-A)^2 + A*C*(1-A)*(2 + N*B*(floor(k/N)+1)) )

nu <- nu_1 + nu_2
nu

z <- qnorm(0.975)

interval <- z * sqrt(nu*v)

hw_sup <- hw_pred + interval
hw_inf <- hw_pred - interval



# # SARIMA
# 
# M <- -0.46
# N <- -0.14
# 
# arima_cross <- arima(mortes_treino,c(0,1,1),seasonal = list(order = c(1,1,0),period = 12),fixed = c(M,N))
# EQM_Arima <- sum( (predict(arima_cross, 12)$pred - mortes_teste)^2 )/12
# EQM_Arima
# 
# arima_pred <- predict(arima_cross, 24)
# subset(arima_pred$pred, start = 13)
# 
# EQM_Arima_prev <- sum( (subset(arima_pred$pred, start = 13) - mortes_prev)^2 )/12
# EQM_Arima_prev
# 
# EAM_Arima_prev <- sum( abs(subset(arima_pred$pred, start = 13) - mortes_prev) )/12
# EAM_Arima_prev
# 
# 
# ypred_cross = arima_pred$pred
# qinf_cross= ypred_cross - qnorm(.975)*arima_pred$se
# qsup_cross= ypred_cross + qnorm(.975)*arima_pred$se
# 
# 
# erros_arima <- arima_cross$residuals[13:48]# anos de 1974 a 1976 (primeiro ano nao ajusta)
# plot(erros_arima, type="p")          # aparentam ser não correlacionados
# 
# qqnorm(erros_arima, bty="n", main = "",           # Mortes_SARIMA_qqplot.jpeg
#        xlab = "Quantis Teóricos",
#        ylab = "Quantis Amostrais")
# qqline(erros_arima)        # normalidade relativamente ok
# 
# v <- var(erros_arima)
# 
# e_1 <- as.vector(erros_arima[2:36])
# e_2 <- as.vector(erros_arima[1:35])
# cor(e_1,e_2)
# acf(erros_arima)


# 
# ######## Sem intervalo de confiança ##########
# {
#   marks <- c(1976, 1977, 1978, 1979)
#   plot.ts(subset(mortes, start=37), ylim=c(7000, 10500),       # Mortes_Prev.jpeg
#           bty="n", ylab = "Mortes", xlab = "Ano", xaxt="n")
#   lines(subset(hw_pred, end=12), col="blue", lty=2)
#   lines(subset(hw_pred, start=13),col = "blue", lty=3)
#   lines(subset(arima_pred$pred,end = 12), col="red", lty=2)
#   lines(subset(arima_pred$pred,start = 13), col="red", lty=3)
#   legend(x=1976.5,y=10800,cex=0.9,
#          c("Real","Holt-Winters", "SARIMA"), lwd=1, lty=1, col=c("black","blue","red"), bty="n")
#   axis(1,at=marks,labels=formatC(marks, digits = 4))
# }


######## Com intervalo de confiança ##########
{
  marks <- c(1976, 1977, 1978, 1979)
  plot.ts(subset(carro, start=37), ylim=c(4000, 15000),       #Mortes_IC.jpeg
          bty="n", ylab = "Mortes", xlab = "Ano", xaxt="n")
  lines(hw_pred, col="blue")
  lines(hw_sup, col="blue", lty=3)
  lines(hw_inf, col="blue", lty=3)
  legend("topleft", inset=.05,
         c("Real","Holt-Winters", "SARIMA"), lwd=1, lty=1, col=c("black","blue","red"), bty="n")
  axis(1,at=marks,labels=formatC(marks, digits = 4))
}




{
  marks <- c(1983, 1984, 1985)
  plot.ts(carro, ylim=c(800, 2100), xlim=c(1983, 1985),           # Carro_HW.jpeg
          bty="n", ylab = "Acidentes", xlab= "Ano", xaxt="n")
  lines(hw_pred, col="blue", lty=2)
  lines(hw_sup, col="blue", lty=3)
  lines(hw_inf, col="blue", lty=3)
  legend("top", inset=.05,
         c("Real","HW_teste"), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n")
  axis(1,at=marks,labels=formatC(marks, digits = 4))
}

