rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

# install.packages("readxl") 
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("randomForest")

library(readxl) 
library(rpart)
library(rpart.plot)
library(randomForest)

#####################################################################
# ANÁLISIS VARIABLE OBJETIVO. DESCRIPTIVA ###

datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef3.xlsx")
names(datos)

mean(datos$porcent)
median(datos$porcent)
sd(datos$porcent)
var(datos$porcent)
summary(datos$porcent)
CV = sd(datos$porcent)/mean(datos$porcent) 
CV

cortes = seq(-40, 160, by = 20)
hist(datos$porcent, 
     main = "Distribución de la variación del PIB",   
     xlab = "Porcentaje de variación", 
     ylab = "Número de países", 
     col = "steelblue",
     breaks = cortes,           
     xlim = c(-40, 160),         
     xaxt = "n",
     ylim = c(0, 25))               
axis(1, at = cortes)

hist(datos$porcent, 
     main = "Distribución de la variación del PIB",   
     xlab = "Porcentaje de variación", 
     ylab = "Probabilidad", 
     col = "steelblue",
     prob = TRUE,
     breaks = cortes,           
     xlim = c(-40, 160),         
     xaxt = "n",
     ylim = c(0, 0.04))               
axis(1, at = cortes)

lines(density(datos$porcent), col = "red", lwd = 2)
legend("topright",
       legend = c("Densidad de los datos"), 
       col = "red", lwd = 2, bty = "n")

boxplot(datos$porcent) # --> NO AÑADIDO

# Gráfico Q-Q (Quantile-Quantile Plot)   --> NO AÑADIDO
qqnorm(datos$porcent, main = "Gráfico Q-Q de porcent")
qqline(datos$porcent, col = "red", lwd = 2)

# --> ANÁLISIS PORCENT ~ CONT ###
datos$cont = factor(datos$cont)

eur = datos[datos$cont == 1, ]
amer = datos[datos$cont == 2, ]
asia = datos[datos$cont == 3, ]

mean(eur$porcent)
median(eur$porcent)
sd(eur$porcent)
var(eur$porcent)
summary(eur$porcent)
CVeur = sd(eur$porcent)/mean(eur$porcent) 
CVeur

mean(amer$porcent)
median(amer$porcent)
sd(amer$porcent)
var(amer$porcent)
summary(amer$porcent)
CVamer = sd(amer$porcent)/mean(amer$porcent) 
CVamer

mean(asia$porcent)
median(asia$porcent)
sd(asia$porcent)
var(asia$porcent)
summary(asia$porcent)
CVasia = sd(asia$porcent)/mean(asia$porcent) 
CVasia

## densidad por continentes
plot(density(eur$porcent), 
     main = "Variación PIB por continente",
     xlab = "Porcentaje", ylab = "Densidad (probabilidad)",
     col = "steelblue", 
     lwd = 2, xlim = c(-40, 160), ylim = c(0, 0.06))
lines(density(amer$porcent), col = "springgreen3", lwd = 2)
lines(density(asia$porcent), col = "pink", lwd = 2) 

azul.t  = rgb(70, 130, 180, alpha = 125, maxColorValue = 255)   
verde.t = rgb(0, 205, 102,  alpha = 125, maxColorValue = 255) 
rosa.t  = rgb(255, 192, 203, alpha = 150, maxColorValue = 255)   

polygon(density(eur$porcent), col = azul.t)      
polygon(density(amer$porcent), col = verde.t)
polygon(density(asia$porcent), col = rosa.t)

legend("topright", legend = c("Europa", "América", "Asia"), 
       fill = c(azul.t, verde.t, rosa.t), bty = "n")

## boxplot por continentes
levels(datos$cont) = c("Europa", "America", "Asia")
plot(porcent ~ cont, data = datos,
     main = "Distribución de porcentaje por continente",
     xlab = "Continente",
     ylab = "Porcentaje", 
     col = paleta)

####################################################################
### ANÁLISIS RLM ###
datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))

datosreg=datos[ ,2:24]           #quitar pais
datosreg=datosreg[ ,-2]          #quitar cont

datoseco = datos[ , c(2, 4:12)]
datosindus = datos[ , c(2, 13:18)]
datossoc = datos[ , c(2, 19:24)]

# Modelo por bloques + step    
mod.eco = lm(porcent ~ . , data = datoseco)    
summary (mod.eco) 
mod.eco.op = step(mod.eco)  
summary(mod.eco.op)

mod.indus = lm(porcent ~ . , data = datosindus)
summary (mod.indus) 
mod.indus.op = step(mod.indus)  
summary(mod.indus.op) 

mod.soc = lm(porcent ~ . , data = datossoc)    
summary (mod.soc) 
mod.soc.op = step(mod.soc) 
summary(mod.soc.op)

### MODELO 3OP
mod3 = lm(porcent ~ deuda + defsup + resimp +
            prodind + FBCF + empind +
            desjuv + GINI + pobreza,
            data = datosreg)
summary(mod3)     
mod3.op = step(mod3)
summary(mod3.op)  

### MODELO OPTIMIZADO 
mod.total = lm(porcent ~ . , data = datosreg)   
summary (mod.total)
mod.total.op = step(mod.total)      
summary (mod.total.op)

# DIAGNOSIS
# Diagnosis Modelo por Bloques
par(mfrow = c(2,2), oma = c(0, 0, 3, 0)) 
plot(mod3.op)
mtext("Diagnosis Modelo por Bloques", 
      outer = TRUE, cex = 1.5, line = 1)
# Diagnosis Modelo Total
par(mfrow = c(2,2), oma = c(0, 0, 3, 0)) 
plot(mod.total.op)
mtext("Diagnosis Modelo Total", 
      outer = TRUE, cex = 1.5, line = 1)


####################################################################
### ANÁLISIS CART. AR ###
set.seed(22226)

### ÁRBOLES POR BLOQUES: rpart + plotcp + prune
# Árbol Económico: 
set.seed(22226)
arboleco = rpart(porcent ~ ., data = datoseco, method = "anova")
rpart.plot (arboleco, main="Árbol de Regresión: \n Bloque Económico")
plotcp(arboleco)
printcp(arboleco)
arboleco.pod = prune(arboleco, cp = 0.01)
rpart.plot (arboleco.pod, main="Árbol de Regresión: \n Bloque Económico Podado")
# Árbol Industrial:
set.seed(22226)
arbolindus = rpart(porcent ~ ., data = datosindus, method = "anova")
rpart.plot (arbolindus, main="Árbol de Regresión:\n Bloque Industrial")
plotcp(arbolindus)
printcp(arbolindus)
arbolindus.pod = prune (arbolindus, cp = 0.01)
rpart.plot (arbolindus.pod, main= "Árbol de Regresión: \n Bloque Industrial Podado")
# Árbol social:
set.seed(22226)
arbolsoc = rpart(porcent ~ ., data = datossoc, method = "anova")
rpart.plot (arbolsoc, main="Árbol de Regresión: Bloque Social")
plotcp(arbolsoc)
printcp(arbolsoc)
arbolsoc.pod = prune (arbolsoc, cp = 0.01)
rpart.plot (arbolsoc.pod, main= "Árbol de Regresión: \n Bloque Social Podado")
# Árbol de síntesis por bloques:
set.seed(22226)
arbolbloq = rpart(porcent ~ resimp + deuda + 
                    prodlab + prodind + 
                    escolariz + espvid, 
                  data = datosreg, method = "anova")
rpart.plot (arbolbloq, main="Árbol de Síntesis por Bloques")
plotcp(arbolbloq)
printcp(arbolbloq)
arbolbloq.pod = prune (arbolbloq, cp = 0.01)
rpart.plot (arbolbloq.pod, main= "Árbol de Síntesis por Bloques Podado")

### ÁRBOL TOTAL:
set.seed(22226)
arboltotal = rpart(porcent ~ ., data = datosreg, method = "anova")
rpart.plot (arboltotal, main="Árbol de Regresión Total")
plotcp(arboltotal)
printcp(arboltotal)
arboltotal.pod = prune (arboltotal, cp = 0.01)
rpart.plot (arboltotal.pod, main= "Árbol de Regresión Total Podado")


####################################################################
### ANÁLISIS RF ###
set.seed(22226)
# RF Económico: 
set.seed(22226)
rf.eco = randomForest(porcent ~ ., data = datoseco, importance = TRUE, ntree = 500)
rf.eco 
varImpPlot(rf.eco, main="Importancias: Bloque Económico. Variación del PIB", col="steelblue", pch=16)
# RF Industrial:
set.seed(22226)
rf.indus = randomForest(porcent ~ ., data = datosindus, importance = TRUE, ntree = 500)
rf.indus
varImpPlot(rf.indus, main="Importancias: Bloque Industrial. Variación del PIB", col="steelblue", pch=16)
# RF Social
set.seed(22226)
rf.soc = randomForest(porcent ~ ., data = datossoc, importance = TRUE, ntree = 500)
rf.soc
varImpPlot(rf.soc, main="Importancias: Bloque Social. Variación del PIB", col="steelblue", pch=16)

# Síntesis por bloques 
set.seed(22226)
rf.bloq = randomForest(porcent ~ resimp + prodlab + pobreza, data = datosreg, importance = TRUE, ntree = 500)
rf.bloq
varImpPlot(rf.bloq, main="Importancias: Síntesis por Bloques. Variación del PIB", col="steelblue", pch=16)
importance(rf.bloq)

# Modelo Total 
set.seed(22226)
rf.total = randomForest(porcent ~ ., data = datosreg, importance = TRUE, ntree = 500)
rf.total
varImpPlot(rf.total, main="Importancias: RF Total. Variación del PIB", col="steelblue", pch=16)
importance(rf.total)



### comprobación R2
VT = sum((datosreg$porcent - mean(datosreg$porcent))^2)

RSS.rf.bloq = sum((datos$porcent - rf.bloq$predicted)^2)
RSS.rf.total = sum((datos$porcent - rf.total$predicted)^2)

R2.rf.bloq = 1 - RSS.rf.bloq/VT
R2.rf.bloq
R2.rf.total = 1 - RSS.rf.total/VT
R2.rf.total




