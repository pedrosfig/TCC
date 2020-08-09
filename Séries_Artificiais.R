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
plot(mult_p,type='l')

######Trend######
trend = rep(NA,60)
for (i in 1:60){trend[i]=400*i}
plot(trend,type = 'l')

windows()
par(mfrow = c(1,1))
plot(trend+add_p)
plot(trend*mult_p)





