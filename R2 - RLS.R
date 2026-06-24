rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

# install.packages("readxl") 
library(readxl) 
datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)

datos$cont = factor(datos$cont)
datosreg=datos[ ,2:24]   #quitar $pais

### MODELOS RLS ###
modprodlab = lm(meses ~ prodlab, data = datosreg)
summary (modprodlab) 
modespvid = lm(meses ~ espvid, data = datosreg)
summary (modespvid)  
modprodind = lm(meses ~ prodind, data = datosreg)
summary (modprodind)  
modpobreza = lm(meses ~ pobreza, data = datosreg)
summary (modpobreza)  

# MAE (Error Medio Absoluto)
prediccion1 = predict(modprodlab)
residuo1 = abs(datosreg$meses - prediccion1)
MAE1 = mean(residuo1)
MAE1
prediccion2 = predict(modespvid)
residuo2 = abs(datosreg$meses - prediccion2)
MAE2 = mean(residuo2)
MAE2
prediccion3 = predict(modprodind)
residuo3 = abs(datosreg$meses - prediccion3)
MAE3 = mean(residuo3)
MAE3
prediccion4 = predict(modpobreza)
residuo4 = abs(datosreg$meses - prediccion4)
MAE4 = mean(residuo4)
MAE4

# DIAGNOSIS 
plot(modprodlab)  # --> NO añadido
plot(modespvid)   # --> NO añadido
plot(modprodind)  # --> NO añadido
plot(modpobreza)  # --> NO añadido