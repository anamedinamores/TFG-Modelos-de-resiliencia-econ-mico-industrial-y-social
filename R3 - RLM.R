rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

# install.packages("readxl") 
library(readxl) 
datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)

datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))

datosreg=datos[ ,2:24]           #quitar $pais

datoseco = datos[ , c(2, 4:12)]
datosindus = datos[ , c(2, 13:18)]
datossoc = datos[ , c(2, 19:24)]

### MODELOS RLM ###
# Modelo con regresores más correlados sin multicolinealidad  
mod.corr = lm(meses ~ prodlab + prodind + pobreza, data = datosreg)
summary (mod.corr)

# Modelo por continentes    
mod.cont = lm(meses ~ cont, data = datosreg)
summary (mod.cont) 
    # Comprobación de diferencia significativa AMER vs ASIA
datosreg$cont = relevel (datosreg$cont, ref = 'AMER')
mod.cont2 = lm(meses ~ cont, data = datosreg)
summary (mod.cont2) 

# Modelo con interacción   
datosreg$cont = relevel (datosreg$cont, ref = 'EUR')
mod.inter = lm(meses ~ cont*prodlab, data = datosreg)
summary (mod.inter) 

# Modelo por bloques + step    
mod.eco = lm(meses ~ . , data = datoseco)    
summary (mod.eco) 
mod.eco.op = step(mod.eco)  
summary(mod.eco.op)

mod.indus = lm(meses ~ . , data = datosindus)
summary (mod.indus) 
mod.indus.op = step(mod.indus)  
summary(mod.indus.op) 

mod.soc = lm(meses ~ . , data = datossoc)    
summary (mod.soc) 
mod.soc.op = step(mod.soc) 
summary(mod.soc.op)

### MODELO 3OP
mod3 = lm(meses ~ deuda + BCC + prodlab + pobreza, data = datosreg)
summary(mod3)     
mod3.op = step(mod3)
summary(mod3.op)  

### MODELO OPTIMIZADO 
mod.total = lm(meses ~ . , data = datosreg)   
summary (mod.total)
mod.total.op = step(mod.total)      
summary (mod.total.op)    

### MAPA DE CALOR (azul = rápida recup; rojo = lenta recup)
# Modelo por Bloques:0.2554 - 3.45*BCC + 0.0005124*prodlab)
eje.prodlab = seq(min(datosreg$prodlab), max(datosreg$prodlab), length.out = 50)
eje.BCC = seq(min(datosreg$BCC), max(datosreg$BCC), length.out = 50)
z.bloques = outer(eje.prodlab, eje.BCC, 
                  function(p, b) 0.2554 - 3.45*b + 0.0005124*p)
image(eje.prodlab, eje.BCC, z.bloques, 
      col = hcl.colors(20, "RdYlBu", rev = TRUE), 
      main = "Mapa de Resiliencia: Modelo por Bloques",
      xlab = "Productividad Laboral (PIB/trabajador)", 
      ylab = "Balanza por Cuenta Corriente (%PIB)",
      las = 1, family = "serif")
contour(eje.prodlab, eje.BCC, z.bloques, add = TRUE, col = "black")
grid(nx = 5, ny = 5, col = "white", lty = 2)
# Modelo Total:12.94 - 3.798*BCC + 0.0004923*prodlab
eje.prodlab = seq(min(datosreg$prodlab), max(datosreg$prodlab), length.out = 50)
eje.BCC = seq(min(datosreg$BCC), max(datosreg$BCC), length.out = 50)
z.bloques = outer(eje.prodlab, eje.BCC, 
                  function(p, b) 12.94 - 3.798*b + 0.0004923*p)
image(eje.prodlab, eje.BCC, z.bloques, 
      col = hcl.colors(20, "RdYlBu", rev = TRUE), 
      main = "Mapa de Resiliencia: Modelo Total",
      xlab = "Productividad Laboral (PIB/trabajador)", 
      ylab = "Balanza por Cuenta Corriente (%PIB)",
      las = 1, family = "serif")
contour(eje.prodlab, eje.BCC, z.bloques, add = TRUE, col = "black")
grid(nx = 5, ny = 5, col = "white", lty = 2)


# VALORACIÓN IMPORTANCIAS --> NO añadido
# install.packages("relaimpo")
library(relaimpo)
# Importancias mod3.op
imp3 = calc.relimp(mod3.op, type = "lmg", rela = TRUE)
plot(imp3, 
     main = "Importancia Relativa de los Factores de Recuperación \n (Modelo por Bloques)")
imp3
# Importancias mod.total.op
imptotal = calc.relimp(mod.total.op, type = "lmg", rela = TRUE)
plot(imptotal, 
     main = "Importancia Relativa de los Factores de Recuperación \n (Modelo Total)")
imptotal


# GRÁFICOS DE PREDICCIÓN VS REAL --> NO añadido
# Gráfico predicción mod3 vs real
plot(predict(mod3.op), datosreg$meses, 
     main="Predicción vs Realidad (Modelo por bloques)",
     xlab="Meses predichos", ylab="Meses reales", 
     pch=19, col="steelblue")
abline(0, 1, col="red", lwd=2) 
# Gráfico predicción mod.todo.op vs real
plot(predict(mod.total.op), datosreg$meses, 
     main="Predicción vs Realidad (Modelo Stepwise Total)",
     xlab="Meses predichos", ylab="Meses reales", 
     pch=19, col="steelblue")
abline(0, 1, col="red", lwd=2) 


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