library(forecast)
library(openxlsx)
library(smooth)
library(graphics)
library(forecast)
library(astsa)

# Suavização exponencial simples (so com A diferente de 0)
SES = arima.sim(list(order = c(0,1,1), ma = 0.7), n = 200)
plot.ts(SES, bty="n")
auto.arima(SES)

HW_SES <- HoltWinters(SES, beta = FALSE, gamma = FALSE)
plot(HW_SES, bty="n")
erro_SES <- HW_SES$fitted[,1] - SES
plot(erro_SES)
qqnorm(erro_SES)
qqline(erro_SES)

# treino e teste - uma bosta!

SES_treino <- subset(SES, end = 170)
SES_treino
SES_teste <- subset(SES, start = 171)
SES_teste

HW_SES_tt <- HoltWinters(SES_treino, beta = FALSE, gamma = FALSE)
plot.ts(SES)
lines(HW_SES_tt$fitted[,1], col="red")
lines(predict(HW_SES_tt, 31), col="blue")





# Tendência aditiva, tem A e B
TA = arima.sim(list(order = c(0,2,2), ma = c(-0.2279, 0.2488)), n = 200) 
ts.plot(TA)
auto.arima(TA)

HW_TA <- HoltWinters(TA, gamma = FALSE)
plot(HW_TA, bty="n")
erro_TA <- HW_TA$fitted[,1] - TA
plot(erro_TA)
qqnorm(erro_TA)
qqline(erro_TA)

# treino e teste - maravilhoso!

TA_treino <- subset(TA, end = 170)
TA_treino
TA_teste <- subset(TA, start = 171)
TA_teste

HW_TA_tt <- HoltWinters(TA_treino, gamma = FALSE)
plot.ts(TA, lwd=2)
lines(HW_TA_tt$fitted[,1], col="red", lty = 2, lwd=2)
lines(predict(HW_TA_tt, 31), col="blue", lty = 2, lwd=2)









#Sazonalidade (p = 3 meses)
p = 3
TSA = arima.sim(list(order = c(0,1,p+1), ma = c(-0.2279, 0.2488),order=c(0,1,0)), n = 200) #Tendência e sazonalidade aditiva, tem A e B e C

#SARIMA(0,1,p+ 1)×(0,1,0)p

#######limitação pq aparentemente a ordem do MA tem q ser menor que a ordem do seasonal####
sarima.sim(ar = NULL, d = 1, ma = c(-0.3,.4,.2,.1), sar = NULL, D = 1, sma = NULL, S = 3, n = 200)
sarima.sim(ar = NULL, d = 1, ma = c(-0.3,.4,.2), sar = NULL, D = 1, sma = NULL, S = 3, n = 200)


######sim_sarima nao ta funcionando######
x=sim_sarima(n = 144, model = list(sma = 0.4, ma = 0.4, sar = 0.8, ar = 0.5,nseasons = 12, sigma2 = 1), xintercept = 1:144)








#####Period#####
mult_p = rep(NA,60)
for (i in 1:60){mult_p[i] = cos(pi*i/6)}
ts.plot(mult_p)

add_p = rep(NA,60)
for (i in 1:60){add_p[i] = cos(pi*i/6)*2500}
ts.plot(add_p)

######Trend######
trend = rep(NA,60)
for (i in 1:60){trend[i]=400*i}
ts.plot(trend)

windows()
par(mfrow = c(1,2))
ts.plot(trend+add_p)
ts.plot(trend*(1+mult_p))


# for (i in 1:8) {
#   series = ts(matriz[,i],frequency = 12)
#   holt = HoltWinters(series)
#   p = predict(holt,48)
#   predição[,i] = p
#   fct = forecast(holt)
#   erro[,i] = fct$residuals #subtração
# }

serie_add = ts(trend+add_p,frequency= 12)
serie_mult = ts(trend*(1+mult_p+0.1),frequency=12) #nao podemos zerar os dados pra usar o fator mult


?HoltWinters()
# ht_add <- HoltWinters(serie_add, alpha=0.3, beta = FALSE, gamma = FALSE, seasonal = "additive")
ht_add <- HoltWinters(serie_mult, optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),seasonal = "multiplicative")
ts.plot(ht_add$fitted[,1])
lines(serie_mult,col = 'green')
plot(decompose(serie_add))

# ?Arima()
# Arima()

?auto.arima()
fit_add <- auto.arima(serie_add)
fit_add

fit_mult <- auto.arima(serie_mult)
fit_mult


?auto.ssarima()
fit_add_s <- auto.ssarima(serie_add)
fit_add_s

fit_mult_s <- auto.ssarima(serie_mult)
fit_mult_s

