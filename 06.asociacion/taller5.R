library(rstudioapi)
filename = "taller5.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)

# importar base de datos en el objeto llamado datos
datos <- read.table(file = "taller05_datos.txt", header = TRUE, sep = ";")

n <- nrow(datos)

# extraer los ingresos de los hombres en x_h
x_h <- datos$ingresos[ datos$sexo == 1 ]

# extraer los ingresos de los mujeres en x_m
x_m <- datos$ingresos[ datos$sexo == 0 ]

###################
#  PRIMER PUNTO A #
###################

# Frecuencia relativa porcentual
tab <- 100*table(datos$sexo)/n

names(tab) <- c("Mujeres", "Hombres")

barplot(tab, xlab = "Sexos", ylab= "Frec Rel")

##################
# PRIMER PUNTO B #
##################
n_m <- length(x_m)
n_h <- length(x_h)

summary(x_m)
summary(x_h)
summary(datos$ingresos)
# desviacion estandar
sd(x_m)
sd(x_h)
sd(datos$ingresos)

# Coeficiente de variación
cv_m <- 100*sd(x_m)/mean(x_m)
cv_h <- 100*sd(x_h)/mean(x_h)
cv <- 100 * sd(datos$ingresos)/mean(datos$ingresos)

# rango
R <- max(datos$ingresos) - min(datos$ingresos)

# amplitud
a <- R / 15

# Limites
li <- min(datos$ingresos) + (0:15) * a

nj_m <- table(cut(x = x_m, breaks = li, include.lowest = TRUE))
nj_h <- table(cut(x = x_h, breaks = li, include.lowest = TRUE))
nj <- table(cut(x = datos$ingresos, breaks = li, include.lowest = TRUE))


par(mfrow=c(2,2))
# Histograma y diagrama de caja
# HOMBRES
hist(x_h, freq = F, nclass = 15, xlab = "Ingresos (en millones)", 
     col = "lightblue", border = "blue", ylab = "Densidad", main = "Hombres")
boxplot(x_h, horizontal = T, col = "lightblue", border = "blue")
# MUJERES
hist(x_m, freq = F, nclass = 15, xlab = "Ingresos (en millones)", 
     col = "lightblue", border = "pink", ylab = "Densidad", main = "Mujeres")
boxplot(x_m, horizontal = T, col = "lightpink", border = "blue")
par(mfrow=c(1, 2))
# TODOS
hist(datos$ingresos, freq = F, nclass = 15, xlab = "Ingresos (en millones)", 
     col = "lightpink", border = "pink", ylab = "Densidad", main = "Hombres y mujeres")
boxplot(datos$ingresos, horizontal = T, col = "lightblue", border = "blue")

##################
# PRIMER PUNTO C #
##################

# media total
xbar <- (length(x_h)*mean(x_h)+length(x_m)*mean(x_m))/n

# intravarianza
intra <- (((n_h-1)*var(x_h))+((n_m-1)*var(x_m)))/(n-1)
inter <- (length(x_h)*(mean(x_h)-xbar)^2+length(x_m)*(mean(x_m)-xbar)^2)/(n-1)

vt <- intra + inter

100*intra/vt # Proporción de variabilidad intragrupal
100*inter/vt # Proporción de variabilidad intergrupal

#############
#  PUNTO 2  #
#############
#   a - 4   #
#   b - 2   #
#   c - 1   #
#   d - 3   #
#############