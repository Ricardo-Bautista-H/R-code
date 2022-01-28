library(readxl)
PIB_Pesca <- read_excel("~/Win7Files/Escritorio/-/Cursos Online/R Econometria Avanzado - ICIP/Sesión 5/01-ARCHIVOS UTILIZADOS-DATA/ECONR-AVA-SESION 5-TAREA-1.1-DATA.xlsx")
PIB_Pesca_ts <- ts(PIB_Pesca$pib_pesca, start=1950)

#Ejercicio 1

E1 <- arima(PIB_Pesca_ts, order = c(1,2,1))
E1

E2 <- arima(PIB_Pesca_ts, order = c(1,1,0))
E2

BIC(E1) #886.9259
BIC(E2) #893.1967

#Comparando los criterios de BIC y el AIC, se aprecia que el modelo 1
#presenta un BIC y un AIC menor. Por lo tanto, se elige el primero.

#Ejercicio 2

#Estimamos el correlograma para tener una primera idea de qué orden estimar la
#parte AR y MA

acf(PIB_Pesca_ts)
#Para la parte MA, el cual la función de acutocorrelación simple nos indica su
#orden, sugiere un MA 12 por su rezago significante

pacf(PIB_Pesca_ts)
#Para la parte AR, el cual la función de acutocorrelación parcial nos indica su
#orden, sugiere un AR 8 por su último rezago significante

#En este caso se trata de una serie con al menos una raíz unitaria, por lo que
#se prueba con una y dos diferencias

#Una diferencia

acf(diff(PIB_Pesca_ts))
#Para la parte MA, el cual la función de acutocorrelación simple nos indica su
#orden, sugiere un MA 3 por su rezago significante

pacf(diff(PIB_Pesca_ts))
#Para la parte AR, el cual la función de acutocorrelación parcial nos indica su
#orden, sugiere un AR 0 o 10 por su último rezago significante

#Dos diferencias

acf(diff(PIB_Pesca_ts, differences = 2))
#Para la parte MA, el cual la función de acutocorrelación simple nos indica su
#orden, sugiere un MA 3 por su rezago significante

pacf(diff(PIB_Pesca_ts, differences = 2))
#Para la parte AR, el cual la función de acutocorrelación parcial nos indica su
#orden, sugiere un AR 4 o 11 por su último rezago significante


#Ejercicio 3

#Predicciones dos años adelante
predic.E1 <- predict(E1, n.ahead = 2)
predic.E2 <- predict(E2, n.ahead = 2)

PredicE1 <- ts(predic.E1$pred, start = c(2016))
PredicE2 <- ts(predic.E2$pred, start = c(2016))

ts.plot(cbind(PIB_Pesca_ts,PredicE1), lty =c(1:2))
ts.plot(cbind(PIB_Pesca_ts,PredicE2), lty =c(1:2))

library(forecast)
plot(forecast(E1, h=2))
plot(forecast(E2, h=2))

