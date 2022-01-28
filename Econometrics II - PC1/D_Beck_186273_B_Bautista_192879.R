rm(list = ls())
library(ggplot2)
library(PerformanceAnalytics)
library(tseries)
library(timeSeries)
library(xts)
library(urca)
library(forecast)
library(quantmod)
library(data.table)
library(rugarch)
library(vars)
sink("/Users/martinbeckd/Desktop/output.txt",append=T)
#### Obeteniendo la data

getSymbols('SPY', from ='2012-01-01', to ='2016-12-31')
class(SPY)
SPY_Close = SPY[,4]

#Gr?fico
plot(SPY_Close)

#Transformacion logaritmica a la serie
lg_SPY_Close <- log(SPY_Close)

plot(lg_SPY_Close)


#Probamos el Augmented Dickey Fuller o ADF test
adf_SPY <- adf.test(lg_SPY_Close,alternative = c("stationary", "explosive"), 
                    k = 0)
adf_SPY
#p-value = 0.3439 > 0.05 afirmamos que la serie tiene raiz unitaria

#ACF y PACF
ggtsdisplay(lg_SPY_Close, lag.max = 24)
#Confimamos no estacionariedad con el ACF

#Diferenciamos la serie
diff_SPY <- diff(lg_SPY_Close, lag=1)

#Corregimos Missing Values
diff_SPY <- na.locf(diff_SPY, na.rm = TRUE,
                    fromLast = TRUE)

plot(diff_SPY)
#Ahora la serie es estacionaria con una diferenciaci?n

#Comprobamos con el ADF test
adf_diff_SPY <- adf.test(diff_SPY, alternative = c("stationary", "explosive"), 
                         k = 0)
adf_diff_SPY
#p-value = 0.01 < 0.05. La serie no presenta ra?z unitaria. Estacionaria.

#ACF y PACF diferenciados
par(mfrow=c(1,2))
diff_acf <- acf(diff_SPY)
diff_pacf <- pacf(diff_SPY)

#ARIMA
Arima_SPY <- auto.arima(diff_SPY, stationary = TRUE, ic = c("aicc", "aic", "bic"), 
                        trace = TRUE)
rescuad<-residuals(Arima_SPY)^2
rescuad
chartSeries(rescuad)
checkresiduals(Arima_SPY)

#Según criterio gráfico de correlogramas un 4,4 sería el mejor
arma44 <- arima(diff_SPY,order = c(4,0,4))
arma12 <- arima(diff_SPY,order = c(1,0,2))

#Pero, su es BIC=-8518.88, mayor al BIC=-8485.176 del arma12
BIC(arma44)
BIC(arma12)
    
#Mejor modelo seg?n criterio es un ARIMA(1,0,2)
summary(Arima_SPY)

#Coefficients:
#ar1      ma1      ma2   mean
#0.9463  -0.9573  -0.0165  4e-04
#sigma^2 estimated as 6.54e-05:  log likelihood=4277.29
#AIC=-8544.57   AICc=-8544.52   BIC=-8518.88
#Revisamos autocorrelaci?n de errores
#No hay autocorrelaci?n de errores

#Probamos predicci?n
arima <- arima(diff_SPY, order = c(2,0,4))
summary(arima)

par(mfrow=c(1,1))
Prediccion1 <- forecast(arima, h = 100)
plot(Prediccion1)
#Se mantiene en la media no condicional o de LP

#GARCH
ug_spec<- ugarchspec(mean.model = list(armaOrder=c(1,2)))
ga_SPY = ugarchfit(spec = ug_spec, data = diff_SPY)

#EGARCH
eg_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                      mean.model = list(armaOrder = c(1,2)))
eg_SPY <- ugarchfit(spec = eg_spec, data = diff_SPY, solver = "solnp")
dt_egarch_SPY <- data.table(infocriteria(eg_SPY),keep.rownames = T)
#gamma1  0.066323. No hay efecto leverage porque es positivo y significativo.
#No se debe usar el egarch

#Tgarch
ta_spec <- ugarchspec(variance.model = list(model = "gjrGARCH"),
                      mean.model = list(armaOrder = c(1,2)))
ta_SPY <- ugarchfit(spec = ta_spec,
                    data = diff_SPY,
                    solver = "solnp")
dt_tgarch_SPY <- data.table(infocriteria(ta_SPY),keep.rownames = T)

sink()