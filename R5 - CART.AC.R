rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

# install.packages("readxl") 
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("caret")

library(readxl) 
library(rpart)
library(rpart.plot)
library (rattle)
library(caret)

set.seed(22226)

datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef2.xlsx")
names(datos)

datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))
datos$impact = factor (datos$impact, labels = c("sin impacto", "con impacto"))

datosreg=datos[ ,2:24]           #quitar $pais

datoseco = datos[ , c(2, 4:12)]
datosindus = datos[ , c(2, 13:18)]
datossoc = datos[ , c(2, 19:24)]

### ÁRBOLES POR BLOQUES: rpart + plotcp + prune
# Árbol Económico: 
set.seed(22226)
arboleco = rpart(impact ~ ., data = datoseco, method = "class")
fancyRpartPlot (arboleco, main="Árbol de Clasificación: \n Bloque Económico")
plotcp(arboleco)
printcp(arboleco)
arboleco.pod = prune(arboleco, cp = 0.01)
fancyRpartPlot (arboleco.pod, main="Árbol de Clasificación: \n Bloque Económico Podado")
# Árbol Industrial:
set.seed(22226)
arbolindus = rpart(impact ~ ., data = datosindus, method = "class")
fancyRpartPlot (arbolindus, main="Árbol de Clasificación:\n Bloque Industrial")
plotcp(arbolindus)
printcp(arbolindus)
arbolindus.pod = prune (arbolindus, cp = 0.01)
fancyRpartPlot (arbolindus.pod, main= "Árbol de Clasificación: \n Bloque Industrial Podado")
# Árbol social:
set.seed(22226)
arbolsoc = rpart(impact ~ ., data = datossoc, method = "class")
fancyRpartPlot (arbolsoc, main="Árbol de Clasificación: Bloque Social")
plotcp(arbolsoc)
printcp(arbolsoc)
arbolsoc.pod = prune (arbolsoc, cp = 0.1)
fancyRpartPlot (arbolsoc.pod, main= "Árbol de Clasificación: \n Bloque Social Podado")

# Árbol de síntesis por bloques:
set.seed(22226)
arbolbloq = rpart(impact ~ resimp + prodlab + pobreza, data = datosreg, method = "class")
fancyRpartPlot (arbolbloq, main="Árbol de Clasificación de Síntesis por Bloques")
plotcp(arbolbloq)
printcp(arbolbloq)
arbolbloq.pod = prune (arbolbloq, cp = 0.01)
fancyRpartPlot (arbolbloq.pod, main= "Árbol de Clasificación de Síntesis por Bloques Podado")

### ÁRBOL TOTAL:
set.seed(22226)
arboltotal = rpart(impact ~ ., data = datosreg, method = "class")
fancyRpartPlot (arboltotal, main="Árbol de Clasificación Total")
plotcp(arboltotal)
printcp(arboltotal)
arboltotal.pod = prune (arboltotal, cp = 0.01)
fancyRpartPlot (arboltotal.pod, main= "Árbol de Clasificación Total Podado")

arbolbloq$variable.importance
arboltotal$variable.importance


### Matriz de Confusión
pred.bloq = predict(arbolbloq, datosreg, type = "class")
matriz.bloq = table(Real = datosreg$impact, Predicho = pred.bloq)
aciertos.bloq = sum(diag(matriz.bloq)) / sum(matriz.bloq)

pred.total = predict(arboltotal, datosreg, type = "class")
matriz.total = table(Real = datosreg$impact, Predicho = pred.total)
aciertos.total = sum(diag(matriz.total)) / sum(matriz.total)


### VALIDACIÓN CRUZADA: Leave-One-Out Cross-Validation (LOOCV) ###
control.loocv = trainControl(method = "LOOCV")
# Modelo de Síntesis por Bloques:
set.seed(22226)
loocv.bloq = train(impact ~ resimp + prodlab + pobreza, data = datosreg, 
                   method = "rpart", trControl = control.loocv, 
                   metric = "ACcuracy")
print(loocv.bloq)
# Modelo Total:
set.seed(22226)
loocv.total = train(impact ~ ., data = datosreg, 
                    method = "rpart", trControl = control.loocv,
                    metric = "ACcuracy")
print(loocv.total)