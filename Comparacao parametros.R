library(datasets)
library(forecast)
library(stats)
library(pastecs)

?USAccDeaths
mortes <- datasets::USAccDeaths
ts.plot(mortes)

# Separando dados de teste

mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49)
mortes_teste


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
A  #0.5
B  #0.3
C  #0.3


hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_cross, 24), col="red", lty=2)
legend("top", inset=.05,
       c("Real","HW"), lwd=1, lty=c(1,2), col=c("black","red")) 


# Comparação pra ver a diferença nos parametros


# Alpha

HW_alpha_1 <- HoltWinters(mortes_treino, alpha=0.1, beta=B, gamma=C, seasonal = "add")
HW_alpha_2 <- HoltWinters(mortes_treino, alpha=0.3, beta=B, gamma=C, seasonal = "add")
HW_alpha_3 <- HoltWinters(mortes_treino, alpha=0.5, beta=B, gamma=C, seasonal = "add")
HW_alpha_4 <- HoltWinters(mortes_treino, alpha=0.8, beta=B, gamma=C, seasonal = "add")

windows()
par(mfrow = c(2,2))

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_alpha_1, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_alpha_2, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_alpha_3, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_alpha_4, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 




# Beta

HW_beta_1 <- HoltWinters(mortes_treino, alpha=A, beta=0.1, gamma=C, seasonal = "add")
HW_beta_2 <- HoltWinters(mortes_treino, alpha=A, beta=0.3, gamma=C, seasonal = "add")
HW_beta_3 <- HoltWinters(mortes_treino, alpha=A, beta=0.5, gamma=C, seasonal = "add")
HW_beta_4 <- HoltWinters(mortes_treino, alpha=A, beta=0.8, gamma=C, seasonal = "add")


windows()
par(mfrow = c(2,2))

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_beta_1, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_beta_2, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_beta_3, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_beta_4, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 





# Gama

HW_gama_1 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.1, seasonal = "add")
HW_gama_2 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.3, seasonal = "add")
HW_gama_3 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.5, seasonal = "add")
HW_gama_4 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.8, seasonal = "add")


windows()
par(mfrow = c(2,2))

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_gama_1, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_gama_2, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_gama_3, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(mortes, ylim=c(5000, 12000), main = "Holt Winters: Mortes")
lines(predict(HW_gama_4, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 













