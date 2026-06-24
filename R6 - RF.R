rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

# install.packages("readxl") 
library(readxl) 
# install.packages("randomForest")
library(randomForest)
set.seed(22226)

datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)

datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))

datosreg=datos[ ,2:24]           #quitar $pais

datoseco = datos[ , c(2, 4:12)]
datosindus = datos[ , c(2, 13:18)]
datossoc = datos[ , c(2, 19:24)]


### RANDOM FOREST POR BLOQUES: 
# RF Económico: 
set.seed(22226)
rf.eco = randomForest(meses ~ ., data = datoseco, importance = TRUE, ntree = 500)
rf.eco 
varImpPlot(rf.eco, main="Importancias: Bloque Económico", col="steelblue", pch=16)
# RF Industrial:
set.seed(22226)
rf.indus = randomForest(meses ~ ., data = datosindus, importance = TRUE, ntree = 500)
rf.indus
varImpPlot(rf.indus, main="Importancias: Bloque Industrial", col="steelblue", pch=16)
# RF Social
set.seed(22226)
rf.soc = randomForest(meses ~ ., data = datossoc, importance = TRUE, ntree = 500)
rf.soc
varImpPlot(rf.soc, main="Importancias: Bloque Social", col="steelblue", pch=16)

# Síntesis por bloques 
set.seed(22226)
rf.bloq = randomForest(meses ~ resimp + prodlab + pobreza, data = datosreg, importance = TRUE, ntree = 500)
rf.bloq
varImpPlot(rf.bloq, main="Importancias: Síntesis por Bloques", col="steelblue", pch=16)
importance(rf.bloq)

# Modelo Total 
set.seed(22226)
rf.total = randomForest(meses ~ ., data = datosreg, importance = TRUE, ntree = 500)
rf.total
varImpPlot(rf.total, main="Importancias: RF Total", col="steelblue", pch=16)
importance(rf.total)



### comprobación R2
VT = sum((datosreg$meses - mean(datosreg$meses))^2)

RSS.rf.bloq = sum((datos$meses - rf.bloq$predicted)^2)
RSS.rf.total = sum((datos$meses - rf.total$predicted)^2)

R2.rf.bloq = 1 - RSS.rf.bloq/VT
R2.rf.bloq
R2.rf.total = 1 - RSS.rf.total/VT
R2.rf.total

