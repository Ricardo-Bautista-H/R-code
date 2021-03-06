#librer�as
library(readxl)

#Importaci�n
infl <- read_excel("R Econometria Avanzado/Sesi�n 1/Tarea/01-ARCHIVOS UTILIZADOS-DATA/ECONR-AVA-SESION 1-TAREA-1.1-DATA.xlsx")

#creaci�n de time series y gr�fico
infl.ts <- ts(infl$inf, start=(1950), end=(2015), frequency = 1)

print(infl.ts)

plot(infl.ts, ylab="inflaci�n", xlab="a�o")

#Descomposici�n y gr�fico
infl.descomp = decompose(infl.ts , type="mult")

tend  = infl.descomp$trend 
estac = infl.descomp$seasonal
aleat = infl.descomp$random

plot(gnp.desc)