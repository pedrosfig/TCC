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

aj1 = arima(carro_treino,c(1,1,2),seasonal = list(order = c(0,1,1),period = 12) ) #replicando a parte 1 do auto.arima
aj2 = arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,1),period = 12) ) 
aj3 = arima(carro_treino,c(1,1,1),seasonal = list(order = c(0,1,2),period = 12) )
aj4 = arima(carro_treino,c(1,0,0),seasonal = list(order = c(1,0,0),period = 12) )

x1 = as.vector(predict(aj1, 12)$pred)
x2 = as.vector(predict(aj2, 12)$pred)
x3 = as.vector(predict(aj3, 12)$pred)
x4 = as.vector(predict(aj4, 12)$pred)

SSE_aj1 <- sum((x1 - carro_teste)^2)
SSE_aj2 <- sum((x2 - carro_teste)^2)
SSE_aj3 <- sum((x3 - carro_teste)^2)
SSE_aj4 <- sum((x4 - carro_teste)^2)

AICs = c(AIC(aj1),AIC(aj2),AIC(aj3),AIC(aj4))
BICs = c(BIC(aj1),BIC(aj2),BIC(aj3),BIC(aj4))
SSEs <- c(SSE_aj1,SSE_aj2,SSE_aj3,SSE_aj4)
cbind(AICs,BICs,SSEs)                              # overfitting


# Teste infantil inicial
{ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(hw_carro_treino, 12), col="red", lty=2)
lines(predict(aj2, 12)$pred,col = 'blue', lty=2)
legend("top", inset=.05,
       c("Real","HW_auto", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 
}


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
#       hw_cross <- HoltWinters(carro_treino, alpha=a, beta=b, gamma=c, seasonal = "add")
#       STE <- sum( (predict(hw_cross, 12) - carro_teste)^2 )
#       if(STE < STE_menor){
#         STE_menor <- STE
#         A <- a
#         B <- b
#         C <- c
#       }
#     }
#   }
# }
# 
# STE_menor
# # step:  0.1  |  0.05  |  0.01
# A      # 0.3  |  0.2   |  0.08
# B      # 0.6  |  0.75  |  0.01
# C      # 0.1  |  0.2  |  0.26

A <- 0.08
B <- 0.01
C <- 0.26



hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
{plot(carro, ylim=c(1000, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
        lines(predict(hw_cross, 12), col="red", lty=2)
        lines(predict(arima_carro_treino, 12)$pred,col = 'blue', lty=2)
        legend("top", inset=.05,
               c("Real","HW_CV", "SARIMA"), lwd=1, lty=c(1,2,2), col=c("black","red", "blue")) 
}



SSE_HW_CV <- sum((predict(hw_cross, 12) - carro_teste)^2)                       # HW Cross-Validation
SSE_arima_auto <- sum((predict(arima_carro_treino, 12)$pred - carro_teste)^2)   # ARIMA automatico
SSE_HW_auto <- sum((predict(hw_carro_treino, 12) - carro_teste)^2)              # HW automatico

cbind(SSE_HW_CV, SSE_arima_auto, SSE_HW_auto)


{plot(c(SSE_HW_CV, SSE_arima_auto, SSE_HW_auto), col=c("green","dark orange","red"), type = "p", main = "Test Error", ylab="SSE")
        legend("bottom", inset=.05, c("HW Cross-Validation", "ARIMA automatico","HW automatico"), lty = 1, 
               col=c("green","dark orange","red")) 
        
        segments(x0=1, y0=SSE_HW_CV, x1=2, y1=SSE_arima_auto, lty=3)
        segments(x0=2, y0=SSE_arima_auto, x1=3, y1=SSE_HW_auto, lty=3)
}






# Comparação pra ver a diferença nos parametros


# Alpha

HW_alpha_1 <- HoltWinters(carro_treino, alpha=0.1, beta=B, gamma=C, seasonal = "add")
HW_alpha_2 <- HoltWinters(carro_treino, alpha=0.3, beta=B, gamma=C, seasonal = "add")
HW_alpha_3 <- HoltWinters(carro_treino, alpha=0.5, beta=B, gamma=C, seasonal = "add")
HW_alpha_4 <- HoltWinters(carro_treino, alpha=0.8, beta=B, gamma=C, seasonal = "add")

{windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_alpha_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_alpha_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_alpha_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_alpha_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","alpha = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 
}




# Beta

HW_beta_1 <- HoltWinters(carro_treino, alpha=A, beta=0.1, gamma=C, seasonal = "add")
HW_beta_2 <- HoltWinters(carro_treino, alpha=A, beta=0.3, gamma=C, seasonal = "add")
HW_beta_3 <- HoltWinters(carro_treino, alpha=A, beta=0.5, gamma=C, seasonal = "add")
HW_beta_4 <- HoltWinters(carro_treino, alpha=A, beta=0.8, gamma=C, seasonal = "add")


{windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_beta_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_beta_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_beta_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_beta_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","beta = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 
}



# Gama

HW_gama_1 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.1, seasonal = "add")
HW_gama_2 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.3, seasonal = "add")
HW_gama_3 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.5, seasonal = "add")
HW_gama_4 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.8, seasonal = "add")


{windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_gama_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_gama_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_gama_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro")
lines(predict(HW_gama_4, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.8"), lwd=1, lty=c(1,2), col=c("black","blue")) 
}


# Multiplicativo

HW_gama_M_1 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.1, seasonal = "mult")
HW_gama_M_2 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.3, seasonal = "mult")
HW_gama_M_3 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.5, seasonal = "mult")
HW_gama_M_4 <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=0.8, seasonal = "mult")


{windows()
par(mfrow = c(2,2))

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_1, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.1"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_2, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.3"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_3, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","gama = 0.5"), lwd=1, lty=c(1,2), col=c("black","blue")) 

ts.plot(carro, ylim=c(0, 3000), xlim=c(1983, 1985), main = "Holt Winters: Carro (Multiplicativo)")
lines(predict(HW_gama_M_4, 12), col="blue", lty=2)
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
        hw_cross <- HoltWinters(carro_treino, alpha=a, beta=B, gamma=C, seasonal = "add")
        STE <- sum( (predict(hw_cross, 12) - carro_teste)^2 )
        error_a[i] <- STE
        
}

min(error_a)                # erro minimo
niveis[which.min(error_a)]  # valor que leva a esse erro

{plot(niveis, error_a, main = "Cross-Validation: Error", xlab = "Alpha", ylab = "SSE")
        legend("topleft",lty=0, c("Beta = B = 0.01", "Gamma = C = 0.26"),lwd=1, bty="n")
}



# Beta

error_b <- rep(0, n)

for(i in 1:n){
        b <- niveis[i]
        hw_cross <- HoltWinters(carro_treino, alpha=A, beta=b, gamma=C, seasonal = "add")
        STE <- sum( (predict(hw_cross, 12) - carro_teste)^2 )
        error_b[i] <- STE
        
}

min(error_b)                # erro minimo
niveis[which.min(error_b)]  # valor que leva a esse erro
{plot(niveis, error_b, main = "Cross-Validation: Error", xlab = "Beta", ylab = "SSE")
        legend("topleft",lty=0, c("Alpha = A = 0.08", "Gamma = C = 0.26"),lwd=1, bty="n")
}



# Gamma Aditivo

error_c_add <- rep(0, n)

for(i in 1:n){
        c <- niveis[i]
        hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=c, seasonal = "add")
        STE <- sum( (predict(hw_cross, 12) - carro_teste)^2 )
        error_c_add[i] <- STE
        
}

min(error_c_add)                # erro minimo
niveis[which.min(error_c_add)]  # valor que leva a esse erro
{plot(niveis, error_c_add, main = "Cross-Validation: Error", xlab = "Gamma (aditivo)", ylab = "SSE")
        legend("topleft",lty=0, c("Alpha = A = 0.08", "Beta = B = 0.01"),lwd=1, bty="n")
}



# Gamma Multiplicativo

error_c_mult <- rep(0, n)

for(i in 1:n){
        c <- niveis[i]
        hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=c, seasonal = "mult")
        STE <- sum( (predict(hw_cross, 12) - carro_teste)^2 )
        error_c_mult[i] <- STE
        
}

min(error_c_mult)                # erro minimo
niveis[which.min(error_c_mult)]  # valor que leva a esse erro
{plot(niveis, error_c_mult, main = "Cross-Validation: Error", xlab = "Gamma (multiplicativo)", ylab = "SSE")
        legend("topleft",lty=0, c("Alpha = A = 0.08", "Beta = B = 0.01"),lwd=1, bty="n")
}



{windows()
        par(mfrow = c(2,2))
        
        plot(niveis, error_a, main = "Cross-Validation: Error", xlab = "Alpha", ylab = "SSE")
        legend("topleft",lty=0, c("Beta = B = 0.01", "Gamma = C = 0.26"),lwd=1, bty="n")
        plot(niveis, error_b, main = "Cross-Validation: Error", xlab = "Beta", ylab = "SSE")
        legend("topleft",lty=0, c("Alpha = A = 0.08", "Gamma = C = 0.26"),lwd=1, bty="n")
        plot(niveis, error_c_add, main = "Cross-Validation: Error", xlab = "Gamma (aditivo)", ylab = "SSE")
        legend("topleft",lty=0, c("Alpha = A = 0.08", "Beta = B = 0.01"),lwd=1, bty="n")
        plot(niveis, error_c_mult, main = "Cross-Validation: Error", xlab = "Gamma (multiplicativo)", ylab = "SSE")
        legend("topleft",lty=0, c("Alpha = A = 0.08", "Beta = B = 0.01"),lwd=1, bty="n")
}
