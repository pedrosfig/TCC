library(forecast)
library("openxlsx")
library(smooth)
require(graphics)
require(latex)

 
#####Period#####
mult_p = rep(NA,60)
for (i in 1:60){mult_p[i] = cos(pi*i/6)}
plot(mult_p,type='l')

add_p = rep(NA,60)
for (i in 1:60){add_p[i] = cos(pi*i/6)*2500}
plot(add_p,type='l')

######Trend######
trend = rep(NA,60)
for (i in 1:60){trend[i]=400*i}
plot(trend,type = 'l')

windows()
par(mfrow = c(1,2))
plot(trend+add_p)
plot(trend*mult_p)


# for (i in 1:8) {
#   series = ts(matriz[,i],frequency = 12)
#   holt = HoltWinters(series)
#   p = predict(holt,48)
#   predição[,i] = p
#   fct = forecast(holt)
#   erro[,i] = fct$residuals #subtração
# }

serie_add = trend+add_p
serie_mult = trend*mult_p


?HoltWinters()
# ht_add <- HoltWinters(serie_add, alpha=0.3, beta = FALSE, gamma = FALSE, seasonal = "additive")
ht_add <- HoltWinters(serie_add, seasonal = "additive")
ht_add

# ?Arima()
# Arima()




