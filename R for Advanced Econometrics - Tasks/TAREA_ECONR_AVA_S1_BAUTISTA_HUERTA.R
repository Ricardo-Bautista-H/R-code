#librerías
library(readxl)

#Importación
infl <- read_excel("R Econometria Avanzado/Sesión 1/Tarea/01-ARCHIVOS UTILIZADOS-DATA/ECONR-AVA-SESION 1-TAREA-1.1-DATA.xlsx")

#creación de time series y gráfico
infl.ts <- ts(infl$inf, start=(1950), end=(2015), frequency = 1)

print(infl.ts)

plot(infl.ts, ylab="inflación", xlab="año")

#Descomposición y gráfico
infl.descomp = decompose(infl.ts , type="mult")

tend  = infl.descomp$trend 
estac = infl.descomp$seasonal
aleat = infl.descomp$random

plot(gnp.desc)