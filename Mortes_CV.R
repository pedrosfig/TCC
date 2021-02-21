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


# # Cross-Validation - todos os parametros --------------------------------------------------
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



# Cross-Validation - sem sazonalidade --------------------------------------------------

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)
EQM_menor = 10^100

for(i in 1:n){
  a <- niveis[i]
  for(j in 1:n){
    b <- niveis[j]
    hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=b, gamma=0)
    EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
    if(EQM < EQM_menor){
      EQM_menor <- EQM
      A <- a
      B <- b
      C <- 0
    }
  }
}

EQM_menor
# step:         0.01
A           #    1
B           #   0.02
C           #    0
EQM_menor   #  107233



# Cross-Validation - sem tendencia - sazonalidade aditiva -----------------------------------------------

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)
EQM_menor = 10^100

for(i in 1:n){
  a <- niveis[i]
  for(j in 1:n){
    c <- niveis[j]
    hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=0, gamma=c, seasonal = "add")
    EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
    if(EQM < EQM_menor){
      EQM_menor <- EQM
      A <- a
      B <- 0
      C <- c
    }
  }
}

EQM_menor
# step:         0.01
A           #    1
B           #    0
C           #   0.14
EQM_menor   #  195075



# Cross-Validation - sem tendencia - sazonalidade multiplicativa ------------------------------------

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)
EQM_menor = 10^100

for(i in 1:n){
  a <- niveis[i]
  for(j in 1:n){
    c <- niveis[j]
    hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=0, gamma=c, seasonal = "mult")
    EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
    if(EQM < EQM_menor){
      EQM_menor <- EQM
      A <- a
      B <- 0
      C <- c
    }
  }
}

EQM_menor
# step:         0.01
A           #    1
B           #    0
C           #   0.46
EQM_menor   #  221339



# Cross-Validation - sem tendencia e sem sazonalidade ------------------------------------------

niveis <- seq(from=0.01, to=1, by=0.01)
n <- length(niveis)
EQM_menor = 10^100

for(i in 1:n){
  a <- niveis[i]
  hw_cross <- HoltWinters(mortes_treino, alpha=a, beta=0, gamma=0)
  EQM <- sum( (predict(hw_cross, 12) - mortes_teste)^2 )/12
  if(EQM < EQM_menor){
    EQM_menor <- EQM
    A <- a
    B <- 0
    C <- 0
  }
}

EQM_menor
# step:         0.01
A           #    1
B           #    0
C           #    0
EQM_menor   #  195075




# Melhor modelo - Final -------------------------------------

A <- 0.09
B <- 0.39
C <- 0.80
# seasonal = mult

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


