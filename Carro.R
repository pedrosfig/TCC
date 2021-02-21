library(datasets)
library(forecast)
library(stats)
library(pastecs)

?UKDriverDeaths
carro <- datasets::UKDriverDeaths
plot.ts(carro, bty="n", ylab="Acidentes")
length(carro)
#trend.test(carro)

# Seatbelts
?Seatbelts

Seatbelts
plot.ts(Seatbelts)


plot.ts(Seatbelts[,5], bty="n", ylab="Kms")  # kms



# Separando dados de teste

carro_treino <- subset(carro, end = 180)
carro_treino
carro_teste <- subset(carro, start = 181)
carro_teste


hw_carro_treino <- HoltWinters(carro_treino, seasonal = "add")        # a=0.3931 b=0.0167 c=0.3015
hw_carro_treino

arima_carro_treino <- auto.arima(carro_treino)    # ARIMA(1,1,2)(0,1,1)[12]
arima_carro_treino

windows()
par(mfrow = c (2,1))
series = ts(carro_treino)
acf(series,lag.max = 48)
pacf(series,lag.max = 48)

series2 = diff(series,lag = 1)
acf(series2,lag.max = 48)
pacf(series2,lag.max = 48)

series3 = diff(series2,lag=12)
acf(series3,lag.max = 48);abline(v=c(12,24,36),lty = 2)
pacf(series3,lag.max = 48);abline(v=c(12,24,36),lty = 2)

aj1 <- arima(carro_treino,c(1,1,2),seasonal = list(order = c(0,1,1),period = 12) ) #replicando a parte 1 do auto.arima
aj2 <- arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,1),period = 12) ) 
aj3 <- arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,2),period = 12) )
aj4 <- arima(carro_treino,c(1,0,0),seasonal = list(order = c(1,0,0),period = 12) )

EQM_aj1 <- sum((predict(aj1, 12)$pred - carro_teste)^2)/12
EQM_aj2 <- sum((predict(aj2, 12)$pred - carro_teste)^2)/12
EQM_aj3 <- sum((predict(aj3, 12)$pred - carro_teste)^2)/12
EQM_aj4 <- sum((predict(aj4, 12)$pred - carro_teste)^2)/12

EAM_aj1 <- sum(abs(predict(aj1, 12)$pred - carro_teste))/12
EAM_aj2 <- sum(abs(predict(aj2, 12)$pred - carro_teste))/12
EAM_aj3 <- sum(abs(predict(aj3, 12)$pred - carro_teste))/12
EAM_aj4 <- sum(abs(predict(aj4, 12)$pred - carro_teste))/12

AICs <- c(AIC(aj1),AIC(aj2),AIC(aj3),AIC(aj4))
BICs <- c(BIC(aj1),BIC(aj2),BIC(aj3),BIC(aj4))
EQMs <- c(EQM_aj1,EQM_aj2,EQM_aj3,EQM_aj4)
EAMs <- c(EAM_aj1,EAM_aj2,EAM_aj3,EAM_aj4)
ajuste <- c("aj1", "aj2", "aj3", "aj4")
data.frame(cbind(AICs,BICs,EQMs,EAMs), row.names=ajuste)    # overfitting


# Teste infantil inicial

HW_pred <- predict(hw_carro_treino, 12, prediction.interval = T, level = 0.95)

pred = predict(aj2, 12)
ypred = pred$pred
qinf= ypred - qnorm(.975)*pred$se
qsup= ypred + qnorm(.975)*pred$se


{plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro", bty="n", ylab="Acidentes")
      lines(HW_pred[,1], col="red", lty=1)      # HW_pred fit
      lines(HW_pred[,2], col="red", lty=3)      # HW_pred upr
      lines(HW_pred[,3], col="red", lty=3)      # HW_pred lwr
      #lines(predict(hw_carro_treino, 12), col="red", lty=2)
      lines(predict(aj2, 12)$pred,col = 'blue', lty=1)  # sarima_pred fit
      lines(qinf,col = 'blue',lty = 3)                  # sarima_pred lwr
      lines(qsup,col = 'blue',lty = 3)                  # sarima_pred upr
      legend("top", inset=.05,
             c("Real","HW_auto", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue"), bty="n") 
}

# # Cross-Validation do HW
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
#       hw_cross <- HoltWinters(carro_treino, alpha=a, beta=b, gamma=c, seasonal = "add")
#       EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
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
# # step:  0.1  |  0.05  |  0.01  | 0.01 (mult)
# A      # 0.3  |  0.2   |  0.08  |  0.21
# B      # 0.6  |  0.75  |  0.01  |  0.80
# C      # 0.1  |  0.20  |  0.26  |  0.2
# EQM_menor     #        |  1336  |  1391

A <- 0.08
B <- 0.01
C <- 0.26

hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
EQM

{
plot.ts(carro, ylim=c(500, 2500), xlim=c(1983, 1985), bty="n", ylab = "Acidentes")
  lines(predict(hw_cross, 12), col="blue", lty=2)
  legend("top", inset=.05,
         c("Real","HW_CV"), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}



####### Cross-Validation do arima 

aj2$coef
teste = arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,1),period = 12),fixed = c(0.2205,-0.7579,-0.9) )
teste$coef

#  niveis <- seq(from= -1, to=1, by=0.02)
#  n <- length(niveis)
#  EQM_menor = 10^100
  
#  for(i in 1:n){
#    m <- niveis[i]
#    for(j in 1:n){
#      n <- niveis[j]
#      for(k in 1:n){
#        o <- niveis[k]
#        arima_cross <- arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,1),period = 12),fixed = c(m,n,o))
#        EQM <- sum( (predict(arima_cross, 12)$pred - carro_teste)^2 )/12
#        if(EQM < EQM_menor){
#          EQM_menor <- EQM
#          M <- m
#          N <- n
#          O <- o
#        }
#      }
#    }
#  }
  
  # EQM_menor (a partir do aj2)
# # step:  0.2  |  0.1   |  0.02
# M      # 0.2  |  0.3   |  0.16
# N      # -1   |  1     |  -0.96
# O      # -0.4 |  -0.9  |  -0.76
# Original: 0.2205 -0.7579 -0.9127861

M <- 0.16
N <- -0.96
O <- -0.76

hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
HW_pred_cross <- predict(hw_cross, 12, prediction.interval = T, level = 0.95)

arima_cross <- arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,1),period = 12),fixed = c(M,N,O))
pred_cross = predict(arima_cross, 12)
ypred_cross = pred_cross$pred
qinf_cross= ypred_cross - qnorm(.975)*pred_cross$se
qsup_cross= ypred_cross + qnorm(.975)*pred_cross$se

  
{plot.ts(carro, ylim=c(500, 2500), xlim=c(1983, 1985), main = "Holt Winters: Carro", bty="n")
      lines(HW_pred_cross[,1], col="red", lty=1)
      lines(HW_pred_cross[,2], col="red", lty=3)      # HW_pred upr
      lines(HW_pred_cross[,3], col="red", lty=3)      # HW_pred lwr
      lines(predict(arima_cross, 12)$pred,col = 'blue', lty=1) 
      lines(qinf_cross,col = 'blue',lty = 3)                   # sarima_pred lwr
      lines(qsup_cross,col = 'blue',lty = 3)                   # sarima_pred upr
      legend("top", inset=.05,
             c("Real","HW_CV", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue"), bty="n") 
}



EQM_HW_CV <- sum((predict(hw_cross, 12) - carro_teste)^2)/12              # HW Cross-Validation
EQM_arima_CV <- sum((predict(arima_cross, 12)$pred - carro_teste)^2)/12   # ARIMA automatico
EQM_HW_auto <- sum((predict(hw_carro_treino, 12) - carro_teste)^2)/12     # HW automatico
EQM_arima_auto <- sum((predict(aj2, 12)$pred - carro_teste)^2)/12         # ARIMA automatico

EAM_HW_CV <- sum(abs(predict(hw_cross, 12) - carro_teste))/12             # HW Cross-Validation
EAM_arima_CV <- sum(abs(predict(arima_cross, 12)$pred - carro_teste))/12  # ARIMA automatico
EAM_HW_auto <- sum(abs(predict(hw_carro_treino, 12) - carro_teste))/12    # HW automatico
EAM_arima_auto <- sum(abs(predict(aj2, 12)$pred - carro_teste))/12        # ARIMA automatico


EQMs <- c(EQM_HW_CV, EQM_arima_CV, EQM_HW_auto,EQM_arima_auto)
EAMs <- c(EAM_HW_CV, EAM_arima_CV, EAM_HW_auto,EAM_arima_auto)
ajuste <- c("HW CV", "ARIMA CV", "HW auto", "ARIMA auto")
data.frame(cbind(EQMs, EAMs), row.names = ajuste)


{plot(c(EQM_HW_CV,EQM_arima_CV,EQM_arima_auto, EQM_HW_auto), col=c("green","dark orange","red","blue"), type = "p", main = "Test Error", ylab="EQM", bty="n")
      legend("topleft", inset=.05, c("HW CV","Arima CV" ,"ARIMA automatico","HW automatico"), lty = 1, 
             col=c("green","dark orange","red","blue"), bty="n") 
        
      segments(x0=1, y0=EQM_HW_CV, x1=2, y1=EQM_arima_CV, lty=3)
      segments(x0=2, y0=EQM_arima_CV, x1=3, y1=EQM_arima_auto, lty=3)
      segments(x0=3, y0=EQM_arima_auto, x1=4, y1=EQM_HW_auto, lty=3)
}













# Comparação pra ver a diferença nos parametros


# Alpha

HW_alpha_1 <- HoltWinters(carro_treino, alpha=0.05, beta=B, gamma=C, seasonal = "add")
HW_alpha_2 <- HoltWinters(carro_treino, alpha=0.2, beta=B, gamma=C, seasonal = "add")
HW_alpha_3 <- HoltWinters(carro_treino, alpha=0.5, beta=B, gamma=C, seasonal = "add")
HW_alpha_4 <- HoltWinters(carro_treino, alpha=0.8, beta=B, gamma=C, seasonal = "add")

{windows()
par(mfrow = c(2,2))

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_alpha_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.1"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_alpha_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.2"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_alpha_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_alpha_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(alpha, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}




# Beta

HW_beta_1 <- HoltWinters(carro_treino, alpha=A, beta=0.05, gamma=C, seasonal = "add")
HW_beta_2 <- HoltWinters(carro_treino, alpha=A, beta=0.2, gamma=C, seasonal = "add")
HW_beta_3 <- HoltWinters(carro_treino, alpha=A, beta=0.5, gamma=C, seasonal = "add")
HW_beta_4 <- HoltWinters(carro_treino, alpha=A, beta=0.8, gamma=C, seasonal = "add")


{windows()
par(mfrow = c(2,2))

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_beta_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.05"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_beta_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.2"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_beta_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_beta_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(beta, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}



# Gama

HW_gama_1 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.05, seasonal = "add")
HW_gama_2 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.2, seasonal = "add")
HW_gama_3 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.5, seasonal = "add")
HW_gama_4 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.8, seasonal = "add")


{windows()
par(mfrow = c(2,2))

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_gama_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.05"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_gama_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.2"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_gama_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), bty="n", ylab="Acidentes")
lines(predict(HW_gama_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.8"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
}


# Multiplicativo

HW_gama_M_1 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.05, seasonal = "mult")
HW_gama_M_2 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.2, seasonal = "mult")
HW_gama_M_3 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.5, seasonal = "mult")
HW_gama_M_4 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.8, seasonal = "mult")


{windows()
par(mfrow = c(2,2))

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)", bty="n")
lines(predict(HW_gama_M_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.05"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)", bty="n")
lines(predict(HW_gama_M_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.2"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)", bty="n")
lines(predict(HW_gama_M_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real",expression(paste(gamma, " = 0.5"))), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 

plot.ts(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)", bty="n")
lines(predict(HW_gama_M_4, 12), col="blue", lty=2)
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
        hw_cross <- HoltWinters(carro_treino, alpha=a, beta=B, gamma=C, seasonal = "add")
        EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
        error_a[i] <- EQM
        
}

min(error_a)                # erro minimo
niveis[which.min(error_a)]  # valor que leva a esse erro

{plot(niveis, error_a, xlab = expression(alpha), ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(beta, " = 0.01")), expression(paste(gamma, " = 0.26"))),lwd=1, bty="n")
}



# Beta

error_b <- rep(0, n)

for(i in 1:n){
        b <- niveis[i]
        hw_cross <- HoltWinters(carro_treino, alpha=A, beta=b, gamma=C, seasonal = "add")
        EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
        error_b[i] <- EQM
        
}

min(error_b)                # erro minimo
niveis[which.min(error_b)]  # valor que leva a esse erro
{plot(niveis, error_b, xlab = expression(beta), ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(gamma, " = 0.26"))),lwd=1, bty="n")
}



# Gamma Aditivo

error_c_add <- rep(0, n)

for(i in 1:n){
        c <- niveis[i]
        hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=c, seasonal = "add")
        EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
        error_c_add[i] <- EQM
        
}

min(error_c_add)                # erro minimo
niveis[which.min(error_c_add)]  # valor que leva a esse erro
{plot(niveis, error_c_add, xlab = expression(gamma), ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(beta, " = 0.01"))),lwd=1, bty="n")
}



# Gamma Multiplicativo

error_c_mult <- rep(0, n)

for(i in 1:n){
        c <- niveis[i]
        hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=c, seasonal = "mult")
        EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
        error_c_mult[i] <- EQM
        
}

min(error_c_mult)                # erro minimo
niveis[which.min(error_c_mult)]  # valor que leva a esse erro
{plot(niveis, error_c_mult, xlab = c(expression(paste(gamma, " (multiplicativo)"))), ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(beta, " = 0.01"))),lwd=1, bty="n")
}



{windows()
        par(mfrow = c(2,2))
        
        plot(niveis, error_a, xlab = "Alpha", ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(beta, " = 0.01")), expression(paste(gamma, " = 0.26"))),lwd=1, bty="n")
        plot(niveis, error_b, xlab = "Beta", ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(gamma, " = 0.26"))),lwd=1, bty="n")
        plot(niveis, error_c_add, xlab = "Gamma (aditivo)", ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(beta, " = 0.01"))),lwd=1, bty="n")
        plot(niveis, error_c_mult, xlab = "Gamma (multiplicativo)", ylab = "EQM", bty="n")
        legend("topleft",lty=0, c(expression(paste(alpha, " = 0.08")), expression(paste(beta, " = 0.01"))),lwd=1, bty="n")
}
