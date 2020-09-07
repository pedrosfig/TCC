library(datasets)
library(forecast)
library(stats)
library(pastecs)

?UKDriverDeaths
carro <- datasets::UKDriverDeaths
ts.plot(carro)
length(carro)
#trend.test(carro)


# Separando dados de teste

carro_treino <- subset(carro, end = 180)
carro_treino
carro_teste <- subset(carro, start = 181)
carro_teste


hw_cross <- HoltWinters(carro_treino, seasonal = "add")
plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(hw_cross, 12), col="red", lty=2)
legend("top", inset=.05,
       c("Real","HW"), lwd=1, lty=c(1,2), col=c("black","red")) 

A <- hw_cross$alpha  # 0.3931
B <- hw_cross$beta   # 0.0167
C <- hw_cross$gamma  # 0.3015



# Comparação pra ver a diferença nos parametros


# Alpha

HW_alpha_1 <- HoltWinters(carro_treino, alpha=0.1, beta=B, gamma=C, seasonal = "add")
HW_alpha_2 <- HoltWinters(carro_treino, alpha=0.3, beta=B, gamma=C, seasonal = "add")
HW_alpha_3 <- HoltWinters(carro_treino, alpha=0.5, beta=B, gamma=C, seasonal = "add")
HW_alpha_4 <- HoltWinters(carro_treino, alpha=0.8, beta=B, gamma=C, seasonal = "add")

windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_alpha_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_alpha_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_alpha_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_alpha_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 





# Beta

HW_beta_1 <- HoltWinters(carro_treino, alpha=A, beta=0.1, gamma=C, seasonal = "add")
HW_beta_2 <- HoltWinters(carro_treino, alpha=A, beta=0.3, gamma=C, seasonal = "add")
HW_beta_3 <- HoltWinters(carro_treino, alpha=A, beta=0.5, gamma=C, seasonal = "add")
HW_beta_4 <- HoltWinters(carro_treino, alpha=A, beta=0.8, gamma=C, seasonal = "add")


windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_beta_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_beta_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_beta_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_beta_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 





# Gama

HW_gama_1 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.1, seasonal = "add")
HW_gama_2 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.3, seasonal = "add")
HW_gama_3 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.5, seasonal = "add")
HW_gama_4 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.8, seasonal = "add")


windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_gama_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_gama_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_gama_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro")
lines(predict(HW_gama_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 



# Multiplicativo

HW_gama_M_1 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.1, seasonal = "mult")
HW_gama_M_2 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.3, seasonal = "mult")
HW_gama_M_3 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.5, seasonal = "mult")
HW_gama_M_4 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.8, seasonal = "mult")


windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(1000, 3000), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 
