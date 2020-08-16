library(datasets)
library(forecast)

?USAccDeaths
mortes <- datasets::USAccDeaths
plot(mortes)
length(mortes)

#teste inicial
hw_mortes <- HoltWinters(mortes)
plot(hw_mortes)


#separando dados de teste
mortes
mortes_treino <- subset(mortes, end = 48)
mortes_treino
mortes_teste <- subset(mortes, start = 49)
mortes_teste

hw_mortes_treino <- HoltWinters(mortes_treino, seasonal = "add")
hw_mortes_treino

plot(mortes, ylim=c(6000, 12000), main = "Holt Winters: Mortes")
lines(predict(hw_mortes_treino, 24), col="red")
legend("top", inset=.05,
       c("Prevista","Real"), lwd=1, lty=1, col=c("red","black"))



#https://rpubs.com/davoodastaraky/TSA1
?decompose

decompose(mortes)
plot(decompose(mortes))
