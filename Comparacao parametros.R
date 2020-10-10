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

hw_cross_add <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
hw_cross_mult <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "mult")
{plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_cross_add, 24), col="red", lty=2)
lines(predict(hw_cross_mult, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","HW_add", "HW_mult"), lwd=1, lty=c(1,2,2), col=c("black","red","blue")) 
}



# Comparação pra ver a diferença nos parametros


# Alpha

HW_alpha_1 <- HoltWinters(mortes_treino, alpha=0.1, beta=B, gamma=C, seasonal = "add")
HW_alpha_2 <- HoltWinters(mortes_treino, alpha=0.3, beta=B, gamma=C, seasonal = "add")
HW_alpha_3 <- HoltWinters(mortes_treino, alpha=0.5, beta=B, gamma=C, seasonal = "add")
HW_alpha_4 <- HoltWinters(mortes_treino, alpha=0.8, beta=B, gamma=C, seasonal = "add")

{windows()
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
}



# Beta

HW_beta_1 <- HoltWinters(mortes_treino, alpha=A, beta=0.1, gamma=C, seasonal = "add")
HW_beta_2 <- HoltWinters(mortes_treino, alpha=A, beta=0.3, gamma=C, seasonal = "add")
HW_beta_3 <- HoltWinters(mortes_treino, alpha=A, beta=0.5, gamma=C, seasonal = "add")
HW_beta_4 <- HoltWinters(mortes_treino, alpha=A, beta=0.8, gamma=C, seasonal = "add")


{windows()
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
}




# Gama

HW_gama_1 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.1, seasonal = "add")
HW_gama_2 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.3, seasonal = "add")
HW_gama_3 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.5, seasonal = "add")
HW_gama_4 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.8, seasonal = "add")


{windows()
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
}







# Cross Validation Errors

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)

# Alpha

error_a <- rep(0, n)

for(i in 1:n){
  a <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=B, gamma=C, seasonal = "add")
  STE <- sum( (predict(hw_cross, 24) - mortes_teste)^2 )
  error_a[i] <- STE
  
}

min(error_a)                # erro minimo
niveis[which.min(error_a)]  # valor que leva a esse erro

{plot(niveis, error_a, main = "Cross-Validation: Error", xlab = "Alpha", ylab = "SSE")
legend("top",lty=0, c("Beta = B = 0.65", "Gamma = C = 0.35"),lwd=1, bty="n")
}



# Beta

error_b <- rep(0, n)

for(i in 1:n){
  b <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=b, gamma=C, seasonal = "add")
  STE <- sum( (predict(hw_cross, 24) - mortes_teste)^2 )
  error_b[i] <- STE
  
}

min(error_b)                # erro minimo
niveis[which.min(error_b)]  # valor que leva a esse erro
{plot(niveis, error_b, main = "Cross-Validation: Error", xlab = "Beta", ylab = "SSE")
legend("top",lty=0, c("Alpha = A = 0.02", "Gamma = C = 0.35"),lwd=1, bty="n")
}



# Gamma Aditivo

error_c_add <- rep(0, n)

for(i in 1:n){
  c <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=c, seasonal = "add")
  STE <- sum( (predict(hw_cross, 24) - mortes_teste)^2 )
  error_c_add[i] <- STE
  
}

min(error_c_add)                # erro minimo
niveis[which.min(error_c_add)]  # valor que leva a esse erro
{plot(niveis, error_c_add, main = "Cross-Validation: Error", xlab = "Gamma (aditivo)", ylab = "SSE")
legend("top",lty=0, c("Alpha = A = 0.02", "Beta = B = 0.65"),lwd=1, bty="n")
}



# Gamma Multiplicativo

error_c_mult <- rep(0, n)

for(i in 1:n){
  c <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=c, seasonal = "mult")
  STE <- sum( (predict(hw_cross, 24) - mortes_teste)^2 )
  error_c_mult[i] <- STE
  
}

min(error_c_mult)                # erro minimo
niveis[which.min(error_c_mult)]  # valor que leva a esse erro
{plot(niveis, error_c_mult, main = "Cross-Validation: Error", xlab = "Gamma (multiplicativo)", ylab = "SSE")
legend("top",lty=0, c("Alpha = A = 0.02", "Beta = B = 0.65"),lwd=1, bty="n")
}



{windows()
par(mfrow = c(2,2))

plot(niveis, error_a, main = "Cross-Validation: Error", xlab = "Alpha", ylab = "SSE")
legend("top",lty=0, c("Beta = B = 0.65", "Gamma = C = 0.35"),lwd=1, bty="n")
plot(niveis, error_b, main = "Cross-Validation: Error", xlab = "Beta", ylab = "SSE")
legend("top",lty=0, c("Alpha = A = 0.02", "Gamma = C = 0.35"),lwd=1, bty="n")
plot(niveis, error_c_add, main = "Cross-Validation: Error", xlab = "Gamma (aditivo)", ylab = "SSE")
legend("top",lty=0, c("Alpha = A = 0.02", "Beta = B = 0.65"),lwd=1, bty="n")
plot(niveis, error_c_mult, main = "Cross-Validation: Error", xlab = "Gamma (multiplicativo)", ylab = "SSE")
legend("top",lty=0, c("Alpha = A = 0.02", "Beta = B = 0.65"),lwd=1, bty="n")
}
