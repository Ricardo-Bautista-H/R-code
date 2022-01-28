

# 1. Importar Enaho01-2019-100.sav ----
library(foreign)

enahoM01 = read.spss('Enaho01-2019-100.sav', to.data.frame = T)

# seleccionar los campos:
  
enahoM01 = enahoM01[,c(8,13,24, 30, 35, 141, 191)] 

summary(enahoM01)

# 2. Seleccionar encuestas completas ----

# RESULT: Completa

table(enahoM01$RESULT)

enahoM01 = enahoM01[enahoM01$RESULT == 'Completa',]

# 3. Convertir los NA de P1172.12 en cero ----

# tip: usar ifelse()

sum(is.na(enahoM01$P1172.12))
enahoM01$P1172.12[is.na(enahoM01$P1172.12)] = 0
sum(is.na(enahoM01$P1172.12))

# 4. Crear una variable que sea el Porcentaje del Gasto de Celular ----

# crear la variable con el nombre: PorcGastCel
# calculo: (P1172.12 / P117T2)*100
# gasto celular: P1172.12
# gasto total: P117T2

enahoM01$PorcGastCel = (enahoM01$P1172.12 / enahoM01$P117T2)*100
enahoM01$gasto_celular = enahoM01$P1172.12
enahoM01$total = enahoM01$P117T2

# 5. Eliminar filas cuando PorcGastCel tenga valores NA ----

enahoM01 = enahoM01[complete.cases(enahoM01[,"PorcGastCel"]),]
sum(is.na(enahoM01$PorcGastCel))

# 6. Crear un grafico boxplot de PorcGastCel segun DOMNIO ----
# para visualizar atipicos

ggplot(enahoM01, aes(x=DOMINIO, y=PorcGastCel)) + 
  geom_boxplot()


# 7. Crear una variable que identifique a los valores atipicos de PorcGastCel

# crear la variable con el nombre: PorcGastCel_Atip
# utilizar la funcion IQR
iqrscore = function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | 
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}
# la variable debe tener dos valores: 1 cuando es atipico y 0 cuando no lo es
# tip: usar ifelse()
enahoM01$PorcGastCel_Atip= iqrscore(enahoM01$PorcGastCel)

enahoM01$PorcGastCel_Atip=ifelse(enahoM01$PorcGastCel_Atip == TRUE, 1, 0)

# 8. ¿Cual es el porcentaje de valores atipicos? ----

"% valores atípicos"= round((sum(enahoM01$PorcGastCel_Atip)/nrow(enahoM01))*100, digits = 2)