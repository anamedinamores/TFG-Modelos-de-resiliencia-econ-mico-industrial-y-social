rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

### ANÁLISIS VARIABLE OBJETIVO ###
# install.packages("readxl") 
library(readxl) 

datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)

mean(datos$meses)
median(datos$meses)
sd(datos$meses)
var(datos$meses)
summary(datos$meses)
CV = sd(datos$meses)/mean(datos$meses) 
CV

hist(datos$meses, main = "Distribución del tiempo de recuperación",   
     xlab = "Número de meses", ylab = "Número de países", col= "steelblue")

lambda.exp = 1 / mean(datos$meses)
hist(datos$meses, main = "Distribución del tiempo de recuperación",   
     xlab = "Número de meses", ylab = "Probabilidad", col= "steelblue",
     prob = TRUE)
curve(dexp(x, rate = lambda.exp), add = TRUE, col = "red", lwd = 2)
legend("topright",
       legend = paste("Exp. Negativa (λ =", round(lambda.exp, 3), ")"), 
       col = "red",lwd = 2, bty = "n")

boxplot(datos$meses) # --> (NO añadido)

# Gráfico Q-Q (Quantile-Quantile Plot) 
qqnorm(datos$meses, main = "Gráfico Q-Q de meses")
qqline(datos$meses, col = "red", lwd = 2)


# --> ANÁLISIS MESES ~ CONT ###
datos$cont = factor(datos$cont)

eur = datos[datos$cont == 1, ]
amer = datos[datos$cont == 2, ]
asia = datos[datos$cont == 3, ]

mean(eur$meses)
median(eur$meses)
sd(eur$meses)
var(eur$meses)
summary(eur$meses)
CVeur = sd(eur$meses)/mean(eur$meses) 
CVeur

mean(amer$meses)
median(amer$meses)
sd(amer$meses)
var(amer$meses)
summary(amer$meses)
CVamer = sd(amer$meses)/mean(amer$meses) 
CVamer
mean(asia$meses)
median(asia$meses)
sd(asia$meses)
var(asia$meses)
summary(asia$meses)
CVasia = sd(asia$meses)/mean(asia$meses) 
CVasia

## histograma por continentes  --> (NO añadido)
bins = seq(0, 140, by = 10)
azul.t  = rgb(70, 130, 180, alpha = 125, maxColorValue = 255)   
verde.t = rgb(0, 205, 102,  alpha = 125, maxColorValue = 255) 
rosa.t  = rgb(255, 192, 203, alpha = 150, maxColorValue = 255)   
hist(eur$meses, breaks = bins,
     main = "Distribución del tiempo de recuperación por continente",   
     xlab = "Número de meses", ylab = "Número de países", 
     col = azul.t, border = "white",
     xlim = c(0, 140), ylim = c(0, 6))
hist(amer$meses, breaks = bins, col = verde.t,border = "white", add = TRUE)
hist(asia$meses, breaks = bins, col = rosa.t, border = "white",add = TRUE)
legend("topright", legend = c("Europa", "América", "Asia"), 
       fill = c(azul.t, verde.t, rosa.t), bty = "n")

## densidad por continentes
plot(density(eur$meses), 
     main = "Tiempo recuperación por continente",
     xlab = "Número de meses", ylab = "Densidad (probabilidad)",
     col = "steelblue", 
     lwd = 2, xlim = c(0, 140), ylim = c(0, 0.03))
lines(density(amer$meses), col = "springgreen3", lwd = 2)
lines(density(asia$meses), col = "pink", lwd = 2) 

polygon(density(eur$meses), col = azul.t)     
polygon(density(amer$meses), col = verde.t)
polygon(density(asia$meses), col = rosa.t)

legend("topright", legend = c("Europa", "América", "Asia"), 
       fill = c(azul.t, verde.t, rosa.t), bty = "n")

## boxplot por continentes
levels(datos$cont) = c("Europa", "America", "Asia")
plot(meses ~ cont, data = datos,
     main = "Distribución de meses por continente",
     xlab = "Continente", ylab = "Meses", col = paleta) 

###########################################################

### ANÁLISIS DE REGRESORES ###
install.packages("corrplot")
library (corrplot)

datosreg=datos[ ,2:24]   #quitar $pais
datosreg=datosreg[ ,-2]  #quitar $cont

summary (datosreg)

# búsqueda de atípicos
par(mfrow = c(3, 4))
boxplot (datosreg$IPC, main = "IPC (%)" )
boxplot (datosreg$deuda, main = "Deuda pública general (%PIB)")
boxplot (datosreg$invext, main = "Inversión extranjera (%PIB) ")
boxplot (datosreg$REER, main = "Tipo de cambio real de efectivo (índice)")
boxplot (datosreg$resimp, main = "Reservas en meses de importaciones")
boxplot (datosreg$XBS, main = "Exportaciones de bienes y servicios (%PIB) ")
boxplot (datosreg$FBCF, main = "Formación bruta de capital fijo (%PIB)")
boxplot (datosreg$empind, main = "Empleo industrial (%empleo total)")
boxplot (datosreg$prodlab, main = "Productividad laboral (PIB/trabajador)")
boxplot (datosreg$pobreza, main = "Pobreza (%población)")
boxplot (datosreg$espvid, main = "Esperanza de vida al nacer (años)")
boxplot (datosreg$escolariz, main = "Escolarización secundaria (%)")

# búsqueda de colinealidad
par(mfrow = c(1, 1))
r=cor(datosreg)          
r
corrplot(r,method='ellipse', 
         title = "Matriz de correlaciones", mar = c(0, 0, 1, 0),
         order = "hclust")
r2 = r [ , 1]         #cor mesesVs.todo
r2
sort(r2, decreasing = TRUE)        #ordenar mayor a menor

# meses ~ regresor con mayor correlación
par(mfrow = c(2, 2))
plot(meses ~ prodlab, data = datosreg, 
     main = "Relación meses vs. Productividad laboral",
     xlab = "Productividad laboral (PIB/trabajador)", 
     ylab = "Meses de recuperación",
     pch = 19,  col = "steelblue")  
abline(lm(meses ~ prodlab, data = datosreg), 
       col = "red", lwd = 2)
plot(meses ~ espvid, data = datosreg, 
     main = "Relación meses vs. Esperanza de vida",
     xlab = "Esperanza de vida al nacer (años)", 
     ylab = "Meses de recuperación",
     pch = 19,  col = "steelblue")  
abline(lm(meses ~ espvid, data = datosreg), 
       col = "red", lwd = 2)
plot(meses ~ prodind, data = datosreg, 
     main = "Relación meses vs. Valor añadido de industria",
     xlab = "Valor añadido de industria (%PIB)", 
     ylab = "Meses de recuperación",
     pch = 19,  col = "steelblue")  
abline(lm(meses ~ prodind, data = datosreg), 
       col = "red", lwd = 2)
plot(meses ~ pobreza, data = datosreg, 
     main = "Relación meses vs. Pobreza",
     xlab = "Pobreza (%población)", 
     ylab = "Meses de recuperación",
     pch = 19,  col = "steelblue")  
abline(lm(meses ~ pobreza, data = datosreg), 
       col = "red", lwd = 2)

# regresor ~ regresor con mayor colienalidad 
panel.recta = function(x, y, ...) 
{ points(x, y, ...)               
  abline(lm(y ~ x), col = "red", lwd = 2) }
pairs(datosreg[, c("escolariz", "prodlab", "espvid")], 
      main="Multcolinealidad positiva  r ≈ 0,6", pch=19, col="steelblue",
      labels = c("Escolarización (%)", 
                 "Productividad laboral (PIB/trabajador)", 
                 "Esperanza de vida (años)"), panel = panel.recta)

par(mfrow = c(1, 1))
plot(desemp ~ desjuv, data = datosreg,
     main="Colinealidad positiva r = 0,88",
     xlab = "Desempleo juvenil (%población activa)", 
     ylab = "Desempleo (%población activa)",
     pch = 19,  col = "steelblue")  
abline(lm(desemp ~ desjuv, data = datosreg), 
       col = "red", lwd = 2)

plot(VAM ~ prodind, data = datosreg,
     main="Colinealidad positiva r = 0,81",
     xlab = "Valor añadido de la industria ($)", 
     ylab = "Valor añadido manufacturero (%PIB)",
     pch = 19,  col = "steelblue")  
abline(lm(VAM ~ prodind, data = datosreg), 
       col = "red", lwd = 2)
