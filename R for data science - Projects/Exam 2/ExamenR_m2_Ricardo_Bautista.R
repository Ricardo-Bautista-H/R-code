
# 1. Importar infidelidad.csv ----

library(readr)

inf <- read_csv("infidelidad.csv")

inf$affairs = as.factor(inf$affairs)
inf$gender = as.factor(inf$gender)
inf$children = as.factor(inf$children)
inf$affairs2 = as.factor(ifelse(inf$affairs == 0, 0, 1))

str(inf)

# 2. Eliminar variable affairs ----

inf$affairs = NULL

# 3. Division de data, train 80% y test 20% ----
# * semilla 2021

library(caTools)

set.seed(2021)
d = sample.split(inf$affairs2, SplitRatio = 0.8)
table(d)

inf_train = subset(inf, d == TRUE)
inf_test = subset(inf, d == FALSE)

nrow(inf_train)/nrow(inf) #0.8

# 4. Balancear x over-sampling en inf_train ----
# * semilla 2021

library(ROSE)

inf_train_b = ovun.sample(affairs2~., data=inf_train, p=0.5, seed = 2021,
                           method = "over")
inf_train_b$data

inf_train_b = inf_train_b$data

table(inf_train$affairs2)
prop.table(table(inf_train$affairs2))*100

table(inf_train_b$affairs2)
prop.table(table(inf_train_b$affairs2))*100

# 5. Crear modelo Regresion logistica binaria ----

modelo1 = glm(affairs2 ~., family = binomial(logit), data=inf_train_b)
modelo1

# 6. Interpretar coeficientes de regresion logistica (Betas) ----

#gendermale: 0.592503, ser hombre aumenta la probabilidad de ser infiel

#age: -0.044760, tener más años de edad reduce la probabilidad de ser infiel

#yearsmarried:0.081657, tener más años de matrimonio aumenta la probabilidad de ser
# infiel

#childrenyes: 0.923899, tener hijos aumenta la probabilidad de ser infiel

#religiousness: -0.263499, tener un nivel de religiosidad mayor disminuye la
#probabilidad de ser infiel

#education: 0.028366, tener más años de educación aumenta la probabilidad de ser
# infiel

#ocupation: -0.006617, tener un mayor nivel de ocupación en la escala de Hollingshead
# reduce la probabilidad de ser infiel

#rating: -0.545889, tener un matrimonio más feliz reduce la probabilidad de ser infiel

# 7. ¿Cuales son las variables significativas del modelo, por qué? ----

summary(modelo1)

#childrenyes, religiousness y rating son significativos a un 99.9% de confianza

#gendermale, age, yearsmarried son significativos a un 99% de confianza

#Se rechaza la hipótesis nula de que los estimadores sean igual a 0

# 8. ¿Cuanto es la Criterio de Informacion de Akaike? ----

modelo1[["aic"]]
#857.1607

# 9. Determinar las variables significativas con stepAIC ----

library(MASS)

stepAIC(modelo1)

#gender + age + yearsmarried + children + religiousness + rating

# 10. Crear la version2 del modelo Regres. logist. binaria...----
#     ... segun el mejor resultado del punto 9

modelo1_v2 = glm(affairs2 ~ gender + age + yearsmarried + children + religiousness + rating,
                 family = binomial(logit), data=inf_train_b)

# 11. En inf_test, estimar la probabilidad de affairs2 ----

inf_test$affairs2_prob = predict(modelo1_v2, newdata = inf_test, type = "response")

# 12. En inf_test, estimar la categoria pronosticada de affairs2 ----
# * regla: 
# ** probabilidad de affairs2 > 0.5, entonces si es infiel (1)
# ** probabilidad de affairs2 <= 0.5, entonces no es infiel (0)

inf_test$affairs2_pred = ifelse(inf_test$affairs2_prob > 0.5, 1, 0)

# 14. Convertir a factor la variable affairs2_pred ----

inf_test$affairs2_pred = as.factor(inf_test$affairs2_pred)

str(inf_test)

# 13. Crear la matriz de confusion ----

library(caret)

confusionMatrix(data = inf_test$affairs2_pred,
                reference= inf_test$affairs2, positive = "1")

# 15. ¿Cuanto es el precision total, sensibilidad, especificidad y AUC ? ----

#Accuracy : 0.7083
#Sensitivity : 0.7667
#Specificity : 0.5333

library(AUC)

auc(roc(inf_test$affairs2_prob, inf_test$affairs2)) #0.6337037

# 16. Interpretar la precision total, sensibilidad y especificidad ----

# El accuracy mide el porcentaje de las instancias correctamente clasificadas
# (Número de instancias correctamente predichas /Número de todas las instancias)
# En este caso, se predijo correctamente el 70.83%

# La sensibilidad es el procentaje de instancias que su condición es positiva y
# el modelo predijo correctamente. O sea, la probabilidad de clasificar
# correctamente a un individuo el cual su estado real sea definido como positivo
# (Verdaderos positivos / Total de instancias positivas)
# En este modelo, la probabilidad es de 76.68%

# La especificidad es la fracción de instancias negativas predichas correctamente o
# la probabilidad de clasificar a un individuo cuyo estado real sea clasficado como
# negativo
# (Verdadero negativo / Total de instancias negativas)
# En este modelo, la probabilidad de esto es de 53.33%


# 17. Crear modelo kmeans con 3 clusters, utilizar las variables:... ----
# ... age, yearsmarried, religiousness, rating, affairs2_prob
# * semilla 2021

set.seed(2021)

library(dplyr)
inf_k = select(inf_test, age, yearsmarried, religiousness, rating, affairs2_prob)

modelo2 = kmeans(inf_k, 3)
modelo2

modelo2$cluster
modelo2$centers

# 18. Visualizar los 3 clusters ----

cluster::clusplot(inf_k, modelo2$cluster, color=T, labels=2)

# 19. ¿Cuanto es la silueta promedio? ----

library(fpc)

estad = cluster.stats(dist(inf_k), modelo2$cluster)
estad

silueta = estad$avg.silwidth
silueta

#0.4004095

# 20. Interpretar los centroides de los clusters ----

# Un centroide se define como el punto equidistante de los objetos
# pertenecientes a dicho cluster

centroides = data.frame(modelo2$centers)
centroides$kmeans3 = 1:3
centroides

# Así, se puede decir que cada centroide es como la observación promedio de cada
# variable en cada cluster.
# Por ejemplo, para el primer cluster la edad promedio es 22 años, para el segundo
# 41 años y para el tercero 28 años.
# Para el primer cluster la probabilidad promedio de ser infiel es de 24.52%,
# para el segundo, de 54,01% y para el tercero, de 38,76%.


