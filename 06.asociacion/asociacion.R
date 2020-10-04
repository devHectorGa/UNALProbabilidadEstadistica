# datos
tabla <- rbind(c(4, 9, 12), c(12, 7, 2))
rownames(tabla) <- c("Hombre", "Mujer")
colnames(tabla) <- c("Bachillerato", "Pregrado", "Posgrado")

# agregar totales
addmargins(A = tabla, margin = c(1,2))

# frecuencias relativas
addmargins(A = 100*prop.table(x=tabla), margin = c(1,2))

# perfiles fila
# addmargins(A = 100*prop.table(x = tabla, margin = 1), margin = 2)
pf <- 100*prop.table( x = tabla, margin = 1 )
addmargins(pf, margin = 2)

# perfiles columna
# addmargins(A = 100*prop.table(x = tabla, margin = 1), margin = 2)
pc <- 100*prop.table(x = tabla, margin = 2)
addmargins(A = pf, margin = 1)

# diagrama de barras perfiles fila
barplot(height = t(pf), ylim = c(0,120), legend.text = TRUE, 
        args.legend = list(x = "top", bty = "n", ncol = 3), 
        main = "Perfil fila", xlab = "Sexo", ylab = "Porcentaje (%)")

# diagrama de barras perfiles columna
barplot(height = pc, beside = FALSE, las = 1, ylim = c(0, 120), 
        legend.text = TRUE, args.legend = list(x = "top", bty = "n", ncol = 2), 
        main = "Perfil columna", xlab = "Nivel educativo", ylab = "Porcentaje (%)")

#############################
#  VARIABLES CUANTITATIVAS  #
#############################

# datos
temp <- c(12.3, 13.2, 12.5, 13.1, 12.9, 13.1, 12.4, 12.9, 13.2, 12.3, 12.4, 13.0, 12.5, 
          12.6, 12.8, 12.9, 12.5, 13.1, 13.0, 12.7, 12.2, 13.3, 12.4, 12.3, 12.6)
peso <- c(39.5, 41.0, 39.7, 40.8, 40.7, 41.3, 39.2, 40.4, 41.2, 38.8, 39.4, 40.2, 39.7, 
          39.8, 40.0, 40.3, 39.6, 41.1, 41.3, 40.3, 39.4, 41.1, 39.9, 39.6, 40.2)
# dispersograma
plot(x = edad, y = peso)

# promedios
mean(temp)
mean(peso)

#covarianza
cov(temp, peso)

# otra manera
n <- length(temp)
sum((temp - mean(temp))*(peso-mean(peso)))/(n-1)

################################
#  COEFICIENTE DE CORRELACIÓN  #
################################

# datos
temp <- c(12.3, 13.2, 12.5, 13.1, 12.9, 13.1, 12.4, 12.9, 13.2, 12.3, 12.4, 13.0, 12.5, 
          12.6, 12.8, 12.9, 12.5, 13.1, 13.0, 12.7, 12.2, 13.3, 12.4, 12.3, 12.6)
peso <- c(39.5, 41.0, 39.7, 40.8, 40.7, 41.3, 39.2, 40.4, 41.2, 38.8, 39.4, 40.2, 39.7, 
          39.8, 40.0, 40.3, 39.6, 41.1, 41.3, 40.3, 39.4, 41.1, 39.9, 39.6, 40.2)
# desviaciones estandar
sd(temp)
sd(peso)

# coeficiente de correlacion
cor(temp, peso)

# otra forma
cov(temp, peso)/(sd(temp)*sd(peso))
