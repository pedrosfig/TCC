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



# Cross-Validation do HW - todos os parametros ----------------------------------------------

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



# Cross-Validation do HW - sem sazonalidade -------------------------------------------------------

# niveis <- seq(from=0.01, to=1, by=0.01)
# n <- length(niveis)
# EQM_menor = 10^100
# 
# for(i in 1:n){
#   a <- niveis[i]
#   for(j in 1:n){
#     b <- niveis[j]
#     hw_cross <- HoltWinters(carro_treino, alpha=a, beta=b, gamma=0)
#     EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
#     if(EQM < EQM_menor){
#       EQM_menor <- EQM
#       A <- a
#       B <- b
#       C <- 0
#     }
#   }
# }
# 
# EQM_menor
# # step:         0.01 
# A           #   0.18  
# B           #   0.76  
# C           #    0
# EQM_menor   #  14618 



# Cross-Validation do HW - sem tendencia - sazonalidade aditiva -------------------------------------

# niveis <- seq(from=0.01, to=1, by=0.01)
# n <- length(niveis)
# EQM_menor = 10^100
# 
# for(i in 1:n){
#   a <- niveis[i]
#   for(j in 1:n){
#     c <- niveis[j]
#     hw_cross <- HoltWinters(carro_treino, alpha=a, beta=0, gamma=c, seasonal = "add")
#     EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
#     if(EQM < EQM_menor){
#       EQM_menor <- EQM
#       A <- a
#       B <- 0
#       C <- c
#     }
#   }
# }
# 
# EQM_menor
# # step:         0.01 
# A           #   0.20
# B           #    0
# C           #   0.22
# EQM_menor   #   2291



# Cross-Validation do HW - sem tendencia - sazonalidade multiplicativa -----------------------------------

# niveis <- seq(from=0.01, to=1, by=0.01)
# n <- length(niveis)
# EQM_menor = 10^100
# 
# for(i in 1:n){
#   a <- niveis[i]
#   for(j in 1:n){
#     c <- niveis[j]
#     hw_cross <- HoltWinters(carro_treino, alpha=a, beta=0, gamma=c, seasonal = "mult")
#     EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
#     if(EQM < EQM_menor){
#       EQM_menor <- EQM
#       A <- a
#       B <- 0
#       C <- c
#     }
#   }
# }
# 
# EQM_menor
# # step:         0.01 
# A           #   0.19
# B           #    0
# C           #   0.35
# EQM_menor   #   2107



# Cross-Validation do HW - sem tendencia e sem sazonalidade -----------------------------------------

# niveis <- seq(from=0.01, to=1, by=0.01)
# n <- length(niveis)
# EQM_menor = 10^100
# 
# for(i in 1:n){
#   a <- niveis[i]
#   hw_cross <- HoltWinters(carro_treino, alpha=a, beta=0, gamma=0)
#   EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
#   if(EQM < EQM_menor){
#     EQM_menor <- EQM
#     A <- a
#     B <- 0
#     C <- 0
#   }
# }
# 
# EQM_menor
# # step:         0.01 
# A           #   0.25
# B           #    0
# C           #    0
# EQM_menor   #  14649




# Melhor modelo - Final -------------------------------------

A <- 0.08
B <- 0.01
C <- 0.26
# seasonal = add

hw_cross <- HoltWinters(carro_treino, alpha=A, beta=B, gamma=C, seasonal = "add")
EQM <- sum( (predict(hw_cross, 12) - carro_teste)^2 )/12
EQM

plot.ts(carro, ylim=c(500, 2500), xlim=c(1983, 1985), bty="n", ylab = "Acidentes")
lines(predict(hw_cross, 12), col="blue", lty=2)
legend("top", inset=.05,
       c("Real","HW_CV"), lwd=1, lty=c(1,2), col=c("black","blue"), bty="n") 
