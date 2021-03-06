library(datasets)
library(forecast)
library(stats)
library(pastecs)

carro <- datasets::UKDriverDeaths
plot.ts(carro, bty="n", ylab="Acidentes")

# Separando dados de teste

carro_treino <- subset(carro, end = 180)
carro_treino
carro_teste <- subset(carro, start = 181)
carro_teste
 
# A <- 0.08
# B <- 0.01
# C <- 0.26
# # seasonal = add
# EQM = 1336

A <- 0.21
B <- 0.80
C <- 0.20
# seasonal = mult
# EQM = 1391

hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=C, seasonal = "mult")
EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
EQM




# Cross Validation Errors

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)

# Alpha

error_a <- rep(0, n)

for(i in 1:n){
  a <- niveis[i]
  hw_cross <- HoltWinters(carro_treino, alpha=a, beta=B, gamma=C, seasonal = "mult")
  EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
  error_a[i] <- EQM
  
}


{plot(niveis, error_a, xlab = expression(alpha), ylab = "EQM", bty="n")
  legend("topleft",lty=0, c(expression(paste(beta, " = 0.01")), expression(paste(gamma, " = 0.26"))),lwd=1, bty="n")
}



# Beta

error_b <- rep(0, n)

for(i in 1:n){
  b <- niveis[i]
  hw_cross <- HoltWinters(carro_treino, alpha=A, beta=b, gamma=C, seasonal = "mult")
  EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
  error_b[i] <- EQM
  
}

{plot(niveis, error_b, xlab = expression(beta), ylab = "EQM", bty="n")
  legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(gamma, " = 0.26"))),lwd=1, bty="n")
}



# Gamma Multiplicativo

error_c_mult <- rep(0, n)

for(i in 1:n){
  c <- niveis[i]
  hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=c, seasonal = "mult")
  EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
  error_c_mult[i] <- EQM
  
}

{plot(niveis, error_c_mult, xlab = c(expression(paste(gamma, " (multiplicativo)"))), ylab = "EQM", bty="n")
  legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(beta, " = 0.01"))),lwd=1, bty="n")
}
{plot(niveis, error_c_mult, ylim = c(0,1e+10), xlab = c(expression(paste(gamma, " (multiplicativo)"))), ylab = "EQM", bty="n")
  legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(beta, " = 0.01"))),lwd=1, bty="n")
}


{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+6))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+7))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+8))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+9))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+10))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}


{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+11))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+12))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+13))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+14))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}

{plot(niveis, error_a, xlab = "Parâmetro", type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "EQM", bty="n", ylim = c(0, 1e+15))
  lines(niveis, error_b, lty = 2, lwd = 2, col = "blue")
  lines(niveis, error_c_mult, lty = 3, lwd = 2, col = "green4")
  legend("topleft",c(expression(alpha), expression(beta), expression(gamma)), 
         lty=c(1,2,3), lwd=2, bty="n",col=c("red","blue", "green4"))
}
