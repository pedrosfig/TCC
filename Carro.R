library(datasets)
library(forecast)
library(stats)
library(pastecs)

?UKDriverDeaths
carro <- datasets::UKDriverDeaths
plot(carro)
length(carro)
trend.test(carro)






