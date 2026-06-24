rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")

### ANÁLISIS EXTENDIDO: TÉCNICAS MULTIVARIANTES ###
# install.packages("readxl") 
# install.packages("factoextra")
# install.packages("MASS")
install.packages('multiUS')  
library(readxl) 
library(factoextra)
library(MASS)
library(multiUS)


### COMPONENTES PPALES ###
datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)
datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))
datosreg=datos[ ,4:24]            
row.names(datosreg)=datos$pais
# Manera 1:
fit = princomp(datosreg, cor = T)
plot(fit, type='lines', main = "Gráfico del Codo")        
fit$loadings                         
fit$sdev^2                           
fit$sdev^2/sum(fit$sdev^2)           
cumsum(fit$sdev^2/sum(fit$sdev^2))  
fit$scores                           

# Biplot coloreado por bloques
colores_bloques = c(rep("springgreen3", 9), rep("steelblue", 6), rep("pink", 6))
biplot(fit, col = c("black", "transparent"), cex = c(0.6, 0.01),               
       main = "Análisis de Componentes Principales: países y variables",
       xlab = "Componente Principal 1", ylab = "Componente Principal 2",
       xlim = c(-0.4, 0.4))
escala = 5.2
text(fit$loadings[,1]*escala, fit$loadings[,2]*escala, 
     labels = rownames(fit$loadings), col = colores_bloques, cex = 0.8, font = 2) 
legend("topleft", legend = c("Económico", "Industrial", "Social"), 
       col = c("springgreen3", "steelblue", "pink"), pch = 16, cex = 0.8,
       bty = "n")
# Manera 2:
source('https://raw.githubusercontent.com/Edu-Caro/datos/refs/heads/main/d1') # prinfact
sol = prinfact(datosreg,3)          
sol$variances                       



### A.CLÚSTER ###
datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef.xlsx")
names(datos)
datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))
datosreg=datos[ ,4:24]            
row.names(datosreg)=datos$pais

datosregstand =scale(datosreg)
d=dist(datosregstand, method= 'euclidean')
print (d, digits=1) 
fviz_nbclust(datosregstand, kmeans, method='wss') +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold"))  #--> NO añadido
# Manera 1: Jerárquico 
h = hclust(d,method='ward.D')
plot (h, hang =-1, main ="Análisis Clúster: Dendrograma")
rect.hclust(h, k=3)
c3 = cutree (h, k=3) 
split(names(c3), c3)
# Manera 2: No jerárquico
set.seed(22226) 
k3 = kmeans (datosregstand, centers =3, nstart=25)
k3$cluster
split(names(k3$cluster), k3$cluster)
round(k3$centers,1)
fviz_cluster(list(data= datosregstand, cluster=k3$cluster),
             labelsize = 8) +
  labs(title = "Análisis Clúster: K-means (k=3)") +
  scale_color_manual(values = c("pink1", "springgreen3", "steelblue"),
                     labels = c("Economías emergentes", 
                                "Economías desarrolladas", 
                                "Economías avanzadas")) +
  scale_fill_manual(values = c("pink1", "springgreen3", "steelblue"),
                    labels = c("Economías emergentes", 
                               "Economías desarrolladas", 
                               "Economías avanzadas")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("Economías emergentes", 
                                "Economías desarrolladas", 
                                "Economías avanzadas")) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

k3$betweenss    #variabilidad externa (buscamos alta)
k3$withinss     #variabildid interna (buscamos baja)
k3$betweenss / k3$totss * 100  # % de variabilidad explicada por los clústeres



### A.DISCRIMINANTE ###
datos = read_excel("C:/Users/aname/Documents/ACADÉMICO/UPM/4 UPM/TFG/DATASET.3/datasetdef2.xlsx")
names(datos)
datos$cont = factor(datos$cont)
datos$cont = factor(datos$cont, labels = c("EUR", "AMER", "ASIA"))
datos$impact = factor (datos$impact, labels = c("sin impacto", "con impacto"))
datosreg=datos[ ,2:24]           #quitar $pais
datosreg=datosreg[ ,-2]          #quitar $cont

m1 = lda(impact ~ . ,data=datosreg)
m1$scaling    #sin estandarizar
m1$means
m1$prior
a0 = -sum(m1$scaling * colMeans(datosreg[ ,-1])) 
a0
m2= ldaPlus (x = datosreg[ ,-1], grouping =datosreg$impact)   #estandarizar
m2$standCoefWithin
plot(m2, col = "steelblue")

prediccion = predict (m2)
table(pred=prediccion$class, real= datosreg$impact)
datos$pais[prediccion$class != datosreg$impact]









