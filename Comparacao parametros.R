library(datasets)
library(forecast)
#library(stats)
#library(pastecs)

?USAccDeaths
mortes <- datasets::USAccDeaths
plot.ts(mortes, bty="n")

# Separando dados de teste

mortes
mortes_treino <- subset(mortes, end = 48)
mortes_teste <- subset(mortes, start = 49, end = 60)
mortes_prev <- subset(mortes, start = 61)

A <- 0.09
B <- 0.39
C <- 0.80



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


{plot(niveis, error_a, xlab = "Parâmetro", type = "l",          # Mortes_EQM_todos.jpeg
      lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 8000000))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_add, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
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


#-----------------------------------------------


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


#-----------------------------------------------
  
# # Comparação pra ver a diferença nos parametros
# 
# 
# # Alpha
# 
# HW_alpha_1 <- HoltWinters(mortes_treino, alpha=0.1, beta=B, gamma=C, seasonal = "mult")
# HW_alpha_2 <- HoltWinters(mortes_treino, alpha=0.3, beta=B, gamma=C, seasonal = "mult")
# HW_alpha_3 <- HoltWinters(mortes_treino, alpha=0.5, beta=B, gamma=C, seasonal = "mult")
# HW_alpha_4 <- HoltWinters(mortes_treino, alpha=0.8, beta=B, gamma=C, seasonal = "mult")
# 
# {windows()
#   par(mfrow = c(2,2))
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_alpha_1, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(alpha, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_alpha_2, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(alpha, " = 0.3"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_alpha_3, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(alpha, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_alpha_4, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(alpha, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
# }
# 
# 
# 
# # Beta
# 
# HW_beta_1 <- HoltWinters(mortes_treino, alpha=A, beta=0.1, gamma=C, seasonal = "mult")
# HW_beta_2 <- HoltWinters(mortes_treino, alpha=A, beta=0.3, gamma=C, seasonal = "mult")
# HW_beta_3 <- HoltWinters(mortes_treino, alpha=A, beta=0.5, gamma=C, seasonal = "mult")
# HW_beta_4 <- HoltWinters(mortes_treino, alpha=A, beta=0.8, gamma=C, seasonal = "mult")
# 
# 
# {windows()
#   par(mfrow = c(2,2))
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_beta_1, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(beta, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_beta_2, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(beta, " = 0.3"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_beta_3, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(beta, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_beta_4, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(beta, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
# }
# 
# 
# 
# 
# # Gama
# 
# HW_gama_1 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.1, seasonal = "mult")
# HW_gama_2 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.3, seasonal = "mult")
# HW_gama_3 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.5, seasonal = "mult")
# HW_gama_4 <- HoltWinters(mortes_treino, alpha=A, beta=B, gamma=0.8, seasonal = "mult")
# 
# 
# {windows()
#   par(mfrow = c(2,2))
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_gama_1, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(gamma, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_gama_2, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(gamma, " = 0.3"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_gama_3, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(gamma, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
#   
#   plot.ts(mortes, ylim=c(5000, 12000), bty="n")
#   lines(predict(HW_gama_4, 24), col="blue", lty=2)
#   legend("top", inset=.05,
#          c("Real",expression(paste(gamma, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
# }
# 
# 
# 

