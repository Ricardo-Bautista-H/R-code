#Proceso de ruido blanco
set.seed(1)
w <- rnorm(50)
plot(w, type = "l")

acf(w) + theme_clean()

#Estimar AR
library(readxl)
library(dplyr)
tasa_crec_pib <- read_excel("~/Win7Files/Escritorio/-/Cursos Online/R Econometria Avanzado/Sesión 2/Tarea/01-ARCHIVOS UTILIZADOS-DATA/DATA-ECONR-AVA-SESION 2-TAREA-1.1.xlsx")
tasa_crec_pib.ar = ar(tasa_crec_pib$varpib, method = "ols")
tasa_crec_pib.ar$ar

