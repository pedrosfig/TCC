library(forecast)
library("openxlsx")
library(smooth)
require(graphics)
require(latex)

 
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

