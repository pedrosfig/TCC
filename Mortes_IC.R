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
EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
EQM

hw_pred <- predict(hw_cross, 24)
subset(hw_pred, start = 13)

EQM_prev <- sum( (subset(hw_pred, start = 13) - mortes_prev)^2 )/12
EQM_prev

{
  plot.ts(mortes, ylim=c(6000, 12000), bty="n", ylab = "Mortes")
  lines(subset(hw_pred, end=12), col="blue", lty=2)
  lines(subset(hw_pred, start=13), col="blue", lty=3)
  legend("top", inset=.05,
         c("Real","HW_CV_teste", "HW_prev"), lwd=1, lty=c(1,2,3), col=c("black","blue","blue"), bty="n") 
}


# Intervalo de Confiança

fitted <- hw_cross$fitted[,1]  # anos de 1974 a 1976 (primeiro ano nao ajusta)
original <- subset(mortes_treino, start = 13)

erros <- fitted - original
plot(erros, type="p")          # aparentam ser correlacionados

qqnorm(erros)
qqline(erros)        # normalidade relativamente ok

v <- var(erros)


# soma de nu_i^2

k <- 1:24
N <- 12

nu_1 <- 1 + (k-1)*A^2 * (1 + k*B + k*(2*k-1)*B^2/6)
nu_2 <- floor(k/N) * ( C^2*(1-A)^2 + A*C*(1-A)*(2 + N*B*(floor(k/N)+1)) )

nu <- nu_1 + nu_2
nu

z <- qnorm(0.95)

interval <- z * sqrt(nu*v)

hw_sup <- hw_pred + interval
hw_inf <- hw_pred - interval

{
  plot.ts(subset(mortes, start=36), ylim=c(4000, 15000), bty="n", ylab = "Mortes")
  lines(subset(hw_pred, end=12), col="blue", lty=2)
  lines(subset(hw_pred, start=13), col="blue", lty=3)
  lines(hw_sup, col="cyan")
  lines(hw_inf, col="cyan")
  legend("top", inset=.05,
         c("Real","HW_CV_teste", "HW_prev"), lwd=1, lty=c(1,2,3), col=c("black","blue","blue"), bty="n") 
}

