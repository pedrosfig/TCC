library(forecast)
library("openxlsx")
library(smooth)
require(graphics)
require(latex)
require(forecast)

##############
SES = arima.sim(list(order = c(0,1,1), ma = 0.7), n = 200) #Suavização exponencial simples (so com A diferete de 0)
ts.plot(SES)
auto.arima(SES)

TA = arima.sim(list(order = c(0,2,2), ma = c(-0.2279, 0.2488)), n = 200) #Tendência aditiva, tem A e B
ts.plot(TA)
auto.arima(TA)

#Sazonalidade (p = 3 meses)
p = 3
TSA = arima.sim(list(order = c(0,1,p+1), ma = c(-0.2279, 0.2488),order=c(0,1,0)), n = 200) #Tendência e sazonalidade aditiva, tem A e B e C

SARIMA(0,1,p+ 1)×(0,1,0)p

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

