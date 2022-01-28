#Cargando data
library(foreign)

telco <- read.spss("telco.sav", to.data.frame = T)
str(telco)
head(telco)

#Eliminando vacíos 
telco = na.omit(telco)
summary(telco)

#Considerando solo las variables pedidas
library(dplyr)
telco <- telco %>% select(c(tenure, cardten, address, equipmon, employ,
                            internet, churn))

# division de data, train 70% y test 30%
library(caTools)

set.seed(2021)
d = sample.split(telco$churn, SplitRatio = 0.7)
table(d)

telco_train = subset(telco, d == TRUE)
telco_test = subset(telco, d == FALSE)

# balancear x oversampling
library(ROSE)

telco_train_b = ovun.sample(churn~., data=telco_train, p=0.5, seed = 2021,
                           method = "over")
head(telco_train_b$data)
telco_train_b = telco_train_b$data

table(telco_train$churn)
prop.table(table(telco_train$churn))*100

table(telco_train_b$churn)
prop.table(table(telco_train_b$churn))*100

save.image(file = "ExamenR_m3_Ricardo_Bautista.RData")


# ARBOL DE DECISION
rm(list = ls())
load("ExamenR_m3_Ricardo_Bautista.RData")
## 1
library(rpart)

arbol_v1 = rpart(churn ~., data=telco_train_b, method = "class")
arbol_v1

library(rattle)
asRules(arbol_v1)

fancyRpartPlot(arbol_v1)

## 2
arbol_v2 = rpart(churn ~., data=telco_train_b, method = "class",
                      control = rpart.control(minsplit = 19,
                                              minbucket = 13))
arbol_v2

asRules(arbol_v2)

fancyRpartPlot(arbol_v2)

# Pronosticando
## 1
telco_test$arbol_pred_v1 = predict(arbol_v1, telco_test, type = "class")

p1 = predict(arbol_v1, telco_test, type = "prob")
head(p1)

telco_test = data.frame(telco_test, p1)
colnames(telco_test)[9:10] = c("churn_probNO_ARBOL1", "churn_probSI_ARBOL1")

## 2
telco_test$arbol_pred_v2 = predict(arbol_v2, telco_test, type = "class")

p2 = predict(arbol_v2, telco_test, type = "prob")
head(p2)

telco_test = data.frame(telco_test, p2)
colnames(telco_test)[12:13] = c("churn_probNO_ARBOL2", "churn_probSI_ARBOL2")

# Evaluando los 2 modelos de arboles

library(caret)

confusionMatrix(data=telco_test$arbol_pred_v1, reference = telco_test$churn,
                positive = "Sí")

confusionMatrix(data=telco_test$arbol_pred_v2, reference = telco_test$churn,
                positive = "Sí")

library(AUC)
auc(roc(telco_test$churn_probSI_ARBOL1, telco_test$churn))
auc(roc(telco_test$churn_probSI_ARBOL2, telco_test$churn))

save.image("Arbol_decision.RData")

# RANDOM FOREST
rm(list = ls())
load("ExamenR_m3_Ricardo_Bautista.RData")
library(randomForest)

set.seed(2021)
#1
rf_v1 = randomForest(churn ~., data=telco_train_b,
                          ntree = 300, mtry = 4, nodesize= 10,
                          importance = T, proximity = T)

rf_v1

ggVarImp(rf_v1) # internet, employ, cardnten y equipmon más importantes


#2 Variables importantes

set.seed(2021)
rf_v2 = randomForest(churn ~., data=telco_train_b[,-c(1,3)],
                     ntree = 300, mtry = 4, nodesize= 10,
                     importance = T, proximity = T)
rf_v2


# Pronosticando ----
#1
telco_test$rf_pred_v1 = predict(rf_v1, telco_test, 
                                    type = "response")

p1 = predict(rf_v1, telco_test, type = "prob")
head(p1)

telco_test = data.frame(telco_test, p1)

colnames(telco_test)[9:10] = c("churn_probNO_rf1", 
                               "churn_probSI_rf1")
#2
telco_test$rf_pred_v2 = predict(rf_v2, telco_test, 
                                type = "response")

p2 = predict(rf_v2, telco_test, type = "prob")
head(p2)

telco_test = data.frame(telco_test, p2)

colnames(telco_test)[12:13] = c("churn_probNO_rf2", 
                                "churn_probSI_rf2")

# Evaluando
#1
confusionMatrix(data=telco_test$rf_pred_v1, 
                reference = telco_test$churn,
                positive = "Sí")

auc(roc(telco_test$churn_probSI_rf1, telco_test$churn))

plot(roc(telco_test$churn_probSI_rf1, telco_test$churn), 
     main="Random Forest 1 Curva ROC")

#2
confusionMatrix(data=telco_test$rf_pred_v2, 
                reference = telco_test$churn,
                positive = "Sí")

auc(roc(telco_test$churn_probSI_rf2, telco_test$churn))

plot(roc(telco_test$churn_probSI_rf2, telco_test$churn), 
     main="Random Forest 2 Curva ROC")

save.image("Random_Forest.RData")


# REDES NEURONALES ----
rm(list = ls())
load("ExamenR_m3_Ricardo_Bautista.RData")

str(telco_train_b)

telco_train_b$Si = telco_train_b$churn == "Sí"
telco_train_b$No = telco_train_b$churn == "No"

#neuralnet no acepta factor, transformamos internet a binario
telco_train_b$internet<- as.integer(telco_train_b$internet)
telco_train_b$internet<-recode(telco_train_b$internet, '1' = 0, '2' = 1 )

head(telco_train_b)

telco_test$internet<- as.integer(telco_test$internet)
telco_test$internet<-recode(telco_test$internet, '1' = 0, '2' = 1 )

head(telco_test)

library(neuralnet)
#1
set.seed(2021)
rna_v1 = neuralnet(No + Si ~ tenure + cardten + address + equipmon +
                     employ + internet,
                   data = telco_train_b,)

ls(rna_v1)
plot(rna_v1)


#2 primera regla (de 2 a 6 neuronas ocultas)
set.seed(2021)
rna_v2 = neuralnet(No + Si ~ tenure + cardten + address + equipmon +
                     employ + internet,
                   data = telco_train_b, hidden = c(6,2), 
                        linear.output = F, stepmax = 1e+06)

ls(rna_v2)
plot(rna_v2)


# * Pronosticando
#1
p1 = predict(rna_v1, telco_test, type = "raw")
head(p1) # probab No y del Si, segun orden de la formulacion

telco_test = data.frame(telco_test, p1)

colnames(telco_test)[8:9] = c("churn_probNO_RED1", 
                               "churn_probSI_RED1")

telco_test$rna_pred_v1 = ifelse(telco_test$churn_probSI_RED1 > 0.5, 
                                    "Sí", "No")

#2
p2 = predict(rna_v2, telco_test, type = "raw")
head(p2) # probab No y del Si, segun orden de la formulacion

telco_test = data.frame(telco_test, p2)

colnames(telco_test)[11:12] = c("churn_probNO_RED2", 
                              "churn_probSI_RED2")

telco_test$rna_pred_v2 = ifelse(telco_test$churn_probSI_RED2 > 0.5, 
                                "Sí", "No")

# * Evaluando
#1
str(telco_test$rna_pred_v1)

telco_test$rna_pred_v1 = as.factor(telco_test$rna_pred_v1)
str(telco_test$rna_pred_v1)

confusionMatrix(data=telco_test$rna_pred_v1, 
                reference = telco_test$churn,
                positive = "Sí")

auc(roc(telco_test$churn_probSI_RED1, telco_test$churn))

plot(roc(telco_test$churn_probSI_RED1, telco_test$churn), 
     main="Red 1 Curva ROC")

head(rna_v1$net.result[[1]])
summary(rna_v1$net.result[[1]])

#2
str(telco_test$rna_pred_v2)

telco_test$rna_pred_v2 = as.factor(telco_test$rna_pred_v2)
str(telco_test$rna_pred_v2)

confusionMatrix(data=telco_test$rna_pred_v2, 
                reference = telco_test$churn,
                positive = "Sí")

auc(roc(telco_test$churn_probSI_RED2, telco_test$churn))

plot(roc(telco_test$churn_probSI_RED2, telco_test$churn), 
     main="Red 2 Curva ROC")

head(rna_v2$net.result[[1]])
summary(rna_v2$net.result[[1]])

save.image("Red_Neuronal.RData")

