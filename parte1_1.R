# Seleccionar directorio a trabajar

setwd("/Users/daniel/Universidad/LEC1")
list.files()
load("poblacion.RData")
ls()

# Asignar el total de goles a una lista
totalGoles <- poblacion["GolesA"] + poblacion["GolesB"]
totalGoles <- unlist(totalGoles)
totalGoles <- as.numeric(totalGoles)

# Valores necesarios
maximo <- max(totalGoles)
minimo <- min(totalGoles)
rango <- maximo - minimo
amplitud1 <- rango/5
amplitud2 <- rango/15
amplitud3 <- rango/30


##### Parte A #####

#Intervalos para Numero de Clase: 5
intervalos1 <- c(minimo)
for (i in 1:5){
	intervalos1 <- c(intervalos1, amplitud1*i)
}
# Marcas y Medianas de Clase: 5
rangos1 = paste("[0,",toString(intervalos1[2]),"]")
val = subset(totalGoles, totalGoles >= intervalos1[1] & totalGoles <= intervalos1[2])
frecuencias1 = length(val)
medianas1 = median(val)
marcas1 = median(val)

current <- minimo - 1
for (i in 2:5){
	rangos1 <- c(rangos1, paste("(",intervalos1[i],",",intervalos1[i+1],"]"))
	val <- subset(totalGoles, totalGoles > intervalos1[i] & totalGoles <= intervalos1[i+1])
	frecuencias1 <- c(frecuencias1,length(val))
	medianas1 <- c(medianas1,median(val))
	marcas1 <- c(marcas1, median(val))
}

tabla1 <- cbind(rangos1,marcas1,medianas1,frecuencias1)
colnames(tabla1)[1] <- "Rangos"
colnames(tabla1)[2] <- "Marca de Clase"
colnames(tabla1)[3] <- "Mediana"
colnames(tabla1)[4] <- "Frecuencia Absoluta"


#Intervalos para Numero de Clase: 15
intervalos2 <- c(minimo)
for (i in 1:15){
	intervalos2 <- c(intervalos2, amplitud2*i)
}
# Marcas y Medianas de Clase: 15
rangos2 = paste("[0,",toString(intervalos2[2]),"]")
val = subset(totalGoles, totalGoles >= intervalos2[1] & totalGoles <= intervalos2[2])
frecuencias2 = length(val)
medianas2 = median(val)
marcas2 = median(val)

current <- minimo - 1
for (i in 2:15){
	rangos2 <- c(rangos2, paste("(",intervalos2[i],",",intervalos2[i+1],"]"))
	val <- subset(totalGoles, totalGoles > intervalos2[i] & totalGoles <= intervalos2[i+1])
	frecuencias2 <- c(frecuencias2,length(val))
	medianas2 <- c(medianas2,median(val))
	marcas2 <- c(marcas2, median(val))
}

tabla2 <- cbind(rangos2,marcas2,medianas2,frecuencias2)
colnames(tabla2)[1] <- "Rangos"
colnames(tabla2)[2] <- "Marca de Clase"
colnames(tabla2)[3] <- "Mediana"
colnames(tabla2)[4] <- "Frecuencia Absoluta"


#Intervalos para Numero de Clase: 15
intervalos3 <- c(minimo)
for (i in 1:30){
	intervalos3 <- c(intervalos3, amplitud3*i)
}
# Marcas y Medianas de Clase: 30
rangos3 = paste("[0,",toString(intervalos3[2]),"]")
val = subset(totalGoles, totalGoles >= intervalos3[1] & totalGoles <= intervalos3[2])
frecuencias3 = length(val)
medianas3 = median(val)
marcas3 = median(val)

current <- minimo - 1
for (i in 2:30){
	rangos3 <- c(rangos3, paste("(",intervalos3[i],",",intervalos3[i+1],"]"))
	val <- subset(totalGoles, totalGoles > intervalos3[i] & totalGoles <= intervalos3[i+1])
	frecuencias3 <- c(frecuencias3,length(val))
	medianas3 <- c(medianas3,median(val))
	marcas3 <- c(marcas3, median(val))
}

tabla3 <- cbind(rangos3,marcas3,medianas3,frecuencias3)
colnames(tabla3)[1] <- "Rangos"
colnames(tabla3)[2] <- "Marca de Clase"
colnames(tabla3)[3] <- "Mediana"
colnames(tabla3)[4] <- "Frecuencia Absoluta"

##### Parte B #####

#Histogramas
hist(totalGoles,nclass=5)
hist(totalGoles,nclass=10)
hist(totalGoles,nclass=30)
hist(totalGoles,nclass=100)



##### Parte C #####
# N = 10
muestra <- sample(totalGoles, size = 10)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 20
muestra <- sample(totalGoles, size = 20)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 30
muestra <- sample(totalGoles, size = 30)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 40
muestra <- sample(totalGoles, size = 40)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 50
muestra <- sample(totalGoles, size = 50)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 60
muestra <- sample(totalGoles, size = 60)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 70
muestra <- sample(totalGoles, size = 70)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 80
muestra <- sample(totalGoles, size = 80)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 90
muestra <- sample(totalGoles, size = 90)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 100
muestra <- sample(totalGoles, size = 100)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 200
muestra <- sample(totalGoles, size = 200)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 300
muestra <- sample(totalGoles, size = 300)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 400
muestra <- sample(totalGoles, size = 400)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 500
muestra <- sample(totalGoles, size = 500)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 600
muestra <- sample(totalGoles, size = 600)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)
# N = 700
muestra <- sample(totalGoles, size = 700)
k_sturges <- log2(length(muestra))
hist(muestra, nclass = k_sturges)

