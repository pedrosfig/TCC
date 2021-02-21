library(datasets)
library(forecast)
library(stats)
library(pastecs)

?USAccDeaths
mortes <- datasets::USAccDeaths
plot.ts(mortes, bty="n")

# Separando dados de teste

mortes
mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49, end = 60)
mortes_teste
mortes_prev <- subset(mortes, start = 61)
mortes_prev


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











hw_cross_add <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
hw_cross_mult <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=C, seasonal = "mult")
{plot.ts(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes", bty="n")
lines(predict(hw_cross_add, 24), col="red", lty=2)
lines(predict(hw_cross_mult, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","HW_add", "HW_mult"), lwd=1, lty=c(1,2,2), col=c("black","red","blue"), bty="n") 
}



# Comparação pra ver a diferença nos parametros


# Alpha

HW_alpha_1 <- HoltWinters(mortes_treino, alpha=0.1, beta=B, gamma=C, seasonal = "add")
HW_alpha_2 <- HoltWinters(mortes_treino, alpha=0.3, beta=B, gamma=C, seasonal = "add")
HW_alpha_3 <- HoltWinters(mortes_treino, alpha=0.5, beta=B, gamma=C, seasonal = "add")
HW_alpha_4 <- HoltWinters(mortes_treino, alpha=0.8, beta=B, gamma=C, seasonal = "add")

{windows()
par(mfrow = c(2,2))

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_alpha_1, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_alpha_2, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.3"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_alpha_3, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_alpha_4, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}



# Beta

HW_beta_1 <- HoltWinters(mortes_treino, alpha=A, beta=0.1, gamma=C, seasonal = "add")
HW_beta_2 <- HoltWinters(mortes_treino, alpha=A, beta=0.3, gamma=C, seasonal = "add")
HW_beta_3 <- HoltWinters(mortes_treino, alpha=A, beta=0.5, gamma=C, seasonal = "add")
HW_beta_4 <- HoltWinters(mortes_treino, alpha=A, beta=0.8, gamma=C, seasonal = "add")


{windows()
par(mfrow = c(2,2))

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_beta_1, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_beta_2, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.3"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_beta_3, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_beta_4, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}




# Gama

HW_gama_1 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.1, seasonal = "add")
HW_gama_2 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.3, seasonal = "add")
HW_gama_3 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.5, seasonal = "add")
HW_gama_4 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.8, seasonal = "add")


{windows()
par(mfrow = c(2,2))

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_gama_1, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_gama_2, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.3"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_gama_3, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(mortes, ylim=c(5000, 12000), bty="n")
lines(predict(HW_gama_4, 24), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}







# Cross Validation Errors

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)

# Alpha

error_a <- rep(0, n)

for(i in 1:n){
  a <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=B, gamma=C, seasonal = "mult")
  EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
  error_a[i] <- EQM
  
}

min(error_a)                # erro minimo
niveis[which.min(error_a)]  # valor que leva a esse erro

{plot(niveis, error_a, xlab = expression(alpha), ylab = "EQM", bty="n")
legend("top",lty=0, c(expression(paste(beta, " = 0.39")), expression(paste(gamma, " = 0.80"))),lwd=1, bty="n")
}



# Beta

error_b <- rep(0, n)

for(i in 1:n){
  b <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=b, gamma=C, seasonal = "mult")
  EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
  error_b[i] <- EQM
  
}

min(error_b)                # erro minimo
niveis[which.min(error_b)]  # valor que leva a esse erro
{plot(niveis, error_b, xlab = expression(beta), ylab = "EQM", bty="n")
legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(gamma, " = 0.80"))),lwd=1, bty="n")
}




# Gamma Multiplicativo

error_c_mult <- rep(0, n)

for(i in 1:n){
  c <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=c, seasonal = "mult")
  EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
  error_c_mult[i] <- EQM
  
}

min(error_c_mult)                # erro minimo
niveis[which.min(error_c_mult)]  # valor que leva a esse erro
{plot(niveis, error_c_mult, xlab = expression(gamma), ylab = "EQM", bty="n")
legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(beta, " = 0.39"))),lwd=1, bty="n")
}


{windows()
  par(mfrow = c(1,3))
  
  plot(niveis, error_a, xlab = expression(alpha), ylab = "EQM", bty="n")
  legend("top",lty=0, c(expression(paste(beta, " = 0.39")), expression(paste(gamma, " = 0.80"))),lwd=1, bty="n")
  plot(niveis, error_b, xlab = expression(beta), ylab = "EQM", bty="n")
  legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(gamma, " = 0.80"))),lwd=1, bty="n")
  plot(niveis, error_c_mult, xlab = expression(paste(gamma, " (multiplicativo)")), ylab = "EQM", bty="n")
  legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(beta, " = 0.39"))),lwd=1, bty="n")
}


# # Gamma Aditivo
# 
# error_c_add <- rep(0, n)
# 
# for(i in 1:n){
#   c <- niveis[i]
#   hw_cross <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=c, seasonal = "add")
#   EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
#   error_c_add[i] <- EQM
#   
# }
# 
# min(error_c_add)                # erro minimo
# niveis[which.min(error_c_add)]  # valor que leva a esse erro
# {plot(niveis, error_c_add, xlab = expression(paste(gamma, " (aditivo)")), ylab = "EQM", bty="n")
#   legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(beta, " = 0.39"))),lwd=1, bty="n")
# }
# 
# 
# 
# 
# {windows()
# par(mfrow = c(2,2))
# 
# plot(niveis, error_a, xlab = expression(alpha), ylab = "EQM", bty="n")
# legend("top",lty=0, c(expression(paste(beta, " = 0.39")), expression(paste(gamma, " = 0.80"))),lwd=1, bty="n")
# plot(niveis, error_b, xlab = expression(beta), ylab = "EQM", bty="n")
# legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(gamma, " = 0.80"))),lwd=1, bty="n")
# plot(niveis, error_c_add, xlab = expression(paste(gamma, " (aditivo)")), ylab = "EQM", bty="n")
# legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(beta, " = 0.39"))),lwd=1, bty="n")
# plot(niveis, error_c_mult, xlab = expression(paste(gamma, " (multiplicativo)")), ylab = "EQM", bty="n")
# legend("top",lty=0, c(expression(paste(alpha, " = 0.09")), expression(paste(beta, " = 0.39"))),lwd=1, bty="n")
# }
