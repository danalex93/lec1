# Seleccionar directorio a trabajar

setwd("/Users/daniel/Universidad/LEC1")
list.files()
load("poblacion.RData")
ls()

# Asignar el total de goles a una lista
totalGoles <- poblacion["GolesA"] + poblacion["GolesB"]
totalGoles <- as.numeric(totalGoles)

# Valores necesarios
maximo <- max(totalGoles)
minimo <- min(totalGoles)
rango <- maximo - minimo
amplitud1 <- rango/5
amplitud2 <- rango/15
amplitud3 <- rango/30

#Intervalos para Numero de Clase: 5
intervalos1 <- c(minimo)
for (i in 1:5){
	intervalos1 <- c(intervalos1, amplitud1*i)
}

#Intervalos para Numero de Clase: 15
intervalos2 <- c(minimo)
for (i in 1:15){
	intervalos2 <- c(intervalos2, amplitud2*i)
}

#Intervalos para Numero de Clase: 15
intervalos3 <- c(minimo)
for (i in 1:30){
	intervalos3 <- c(intervalos3, amplitud3*i)
}