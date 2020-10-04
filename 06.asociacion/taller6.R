library(rstudioapi)
filename = "taller6.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)
rm(filename, filepath,dir)

data <- read.table(file="taller06_datos.txt", header = T, sep=";")

###########
# PUNTO 1 #
###########
#  a - 3  #
#  b - 1  #
#  c - 4  #
#  d - 2  # 
###########

############
#  PUNTO 2 #
############

x <- c(1.4, 2.4, 4.0, 4.9, 5.7, 6.3,   7.8,  9.0,  9.3, 11.0)
y <- c(2.3, 3.7, 5.7, 9.9, 6.9, 15.8, 15.4, 36.9, 34.6, 53.2)

# Dispersograma
plot(x,y)
# Coeficiente de correlación
# cov(temp, peso)/(sd(x)*sd(y))
cor(x,y)
# La relacion entre x, y es lineal fuerte.

plot(x, log(y))
cor(x,log(y))
# La relacion entre x, ln(y) es lineal fuerte.

# Es mas fácil traajar con x, ln(y) por que su coeficiente de correlación es más fuerte.

#############
#  PUNTO 3  #
#############

peso <- data$peso
estatura <- data$estatura

plot(estatura, peso)
# Covariancia si es positiva la relacion es directa
cov(estatura, peso)

# Coeficiente de correlación de Pearson (r)
cor(estatura,peso)

# Corrección de peso
peso_cor <- peso + 2.5 - peso * 0.05
cov(estatura, peso_cor)
cor(estatura, peso_cor)

#############
#  PUNTO 4  #
#############
n <- 10
xbar <- 110/n
ybar <- 60/n

# Covarianza
cov_xy = (1868-n*ybar*xbar)/(n-1)
# Varianzas
var_x = (3156 - n*xbar^2)/(n-1)
var_y = (1138 - n*ybar^2)/(n-1)

# Correlación
cor_xy / (sqrt(var_x) * sqrt(var_y))
# Correlación lineal fuerte y directa
######################################

x_cor = 110 * 1.05
y_cor = 60 * 1.03

(1.05*1.03*cov_xy)/(1.05*1.03*sqrt(var_x)*sqrt(var_y))

# No cambia la correlación 1.05 y 1.03 se cancelan y queda el mismo valor


#############
#  PUNTO 5  #
#############

# Frecuencias relativas
tabla <- rbind(c(241,53,12), c(204,12,11))
n <- 533
colnames(tabla) <- c("A favor", "En contra", "NS/NR")
rownames(tabla) <- c("Hombre", "Mujer")

round(addmargins(A = 100*prop.table(x = tabla), margin = c(1,2)), digits = 2)

barplot(height = 100*tabla/n, legend.text = T, beside = T, ylab = "Porcentaje %", xlab = "Opinión" )

# Perfiles fila
pf <- 100*prop.table(x = tabla, margin=1)
round(addmargins(A = pf, margin = 2), digits = 2)

barplot(height = t(pf), ylim = c(0,120), legend.text = TRUE, 
        args.legend = list(x = "top", bty = "n", ncol = 3), 
        main = "Perfil fila", xlab = "Sexo", ylab = "Porcentaje (%)")
# Perfiles columna
pc <- 100*prop.table(x = tabla, margin=2)
round(addmargins(A = pc, margin = 1), digits = 2)

barplot(height = pc, beside = FALSE, las = 1, ylim = c(0, 120), 
        legend.text = TRUE, args.legend = list(x = "top", bty = "n", ncol = 2), 
        main = "Perfil columna", xlab = "Nivel educativo", ylab = "Porcentaje (%)")

#############################################
#                   5 D                     #
#############################################
# % individuos hombres en contra  #  9.94%  #
# % de mujeres a favor            # 89.87%  #
# % individuos en contra, hombres # 81.54%  #
#############################################