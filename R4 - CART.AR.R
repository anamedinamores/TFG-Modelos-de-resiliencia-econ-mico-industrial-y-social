rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")


# install.packages("readxl") 
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("caret")

library(readxl) 
library(rpart)
library(rpart.plot)
library(caret)


set.seed(22226)

datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)

datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))

datosreg=datos[ ,2:24]           #quitar $pais

datoseco = datos[ , c(2, 4:12)]
datosindus = datos[ , c(2, 13:18)]
datossoc = datos[ , c(2, 19:24)]

### ÁRBOLES POR BLOQUES: rpart + plotcp + prune
# Árbol Económico: 
set.seed(22226)
arboleco = rpart(meses ~ ., data = datoseco, method = "anova")
rpart.plot (arboleco, main="Árbol de Regresión: \n Bloque Económico")
plotcp(arboleco)
printcp(arboleco)
arboleco.pod = prune(arboleco, cp = 0.23)
rpart.plot (arboleco.pod, main="Árbol de Regresión: \n Bloque Económico Podado")
# Árbol Industrial:
set.seed(22226)
arbolindus = rpart(meses ~ ., data = datosindus, method = "anova")
rpart.plot (arbolindus, main="Árbol de Regresión:\n Bloque Industrial")
plotcp(arbolindus)
printcp(arbolindus)
arbolindus.pod = prune (arbolindus, cp = 0.27)
rpart.plot (arbolindus.pod, main= "Árbol de Regresión: \n Bloque Industrial Podado")
# Árbol social:
set.seed(22226)
arbolsoc = rpart(meses ~ ., data = datossoc, method = "anova")
rpart.plot (arbolsoc, main="Árbol de Regresión: Bloque Social")
plotcp(arbolsoc)
printcp(arbolsoc)
arbolsoc.pod = prune (arbolsoc, cp = 0.19)

rpart.plot (arbolsoc.pod, main= "Árbol de Regresión: \n Bloque Social Podado")
# Árbol de síntesis por bloques:
set.seed(22226)
arbolbloq = rpart(meses ~ resimp + prodlab + pobreza, data = datosreg, method = "anova")
rpart.plot (arbolbloq, main="Árbol de Síntesis por Bloques")
plotcp(arbolbloq)
printcp(arbolbloq)
arbolbloq.pod = prune (arbolbloq, cp = 0.23)
rpart.plot (arbolbloq.pod, main= "Árbol de Síntesis por Bloques Podado")

### ÁRBOL TOTAL:
set.seed(22226)
arboltotal = rpart(meses ~ ., data = datosreg, method = "anova")
rpart.plot (arboltotal, main="Árbol de Regresión Total")
plotcp(arboltotal)
printcp(arboltotal)
arboltotal.pod = prune (arboltotal, cp = 0.27)
rpart.plot (arboltotal.pod, main= "Árbol de Regresión Total Podado")

arbolbloq$variable.importance
arboltotal$variable.importance


### comprobación R2
VT = sum((datosreg$meses - mean(datosreg$meses))^2)
RSS.arbolbloq = sum(( datos$meses - predict(arbolbloq, datosreg))^2)
RSS.arboltotal = sum(( datos$meses - predict(arboltotal, datosreg))^2)

R2.arbolbloq = 1 - RSS.arbolbloq/VT
R2.arbolbloq
R2.arboltotal = 1 - RSS.arboltotal/VT
R2.arboltotal


### VALIDACIÓN CRUZADA: Leave-One-Out Cross-Validation (LOOCV) ###
control.loocv = trainControl(method = "LOOCV")
# Modelo de Síntesis por Bloques:
set.seed(22226)
loocv.bloq = train(meses ~ resimp + prodlab + pobreza, data = datosreg, 
                   method = "lm", trControl = control.loocv)
print(loocv.bloq)
# Modelo Total:
set.seed(22226)
loocv.total = train(meses ~ ., data = datosreg, 
                    method = "lm", trControl = control.loocv)
print(loocv.total)