# Seleccionar directorio a trabajar

setwd("/Users/daniel/Universidad/LEC1")
list.files()
load("poblacion.RData")
ls()

# Asignar el total de goles a una lista
totalGoles <- poblacion["GolesA"] + poblacion["GolesB"]
totalGoles <- unlist(totalGoles)
totalGoles <- as.numeric(totalGoles)



##### Parte A #####

# Calculos de:
media <- mean(totalGoles)
mediana <- median(totalGoles)
varianza <- var(totalGoles)

#Funcion para calcular la Moda
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

moda <- Mode(totalGoles)



##### Parte B #####
boxplot(totalGoles)



##### Parte C #####
mediaP = mean(totalGoles)
medianaP = median(totalGoles)
varianzaP = (length(totalGoles)-1)*var(totalGoles)/(length(totalGoles))
boxplot(totalGoles)