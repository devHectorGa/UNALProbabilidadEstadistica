###########
# Punto 1 #
###########

# En el punto 1 todos son falsos.

###########
# Punto 2 #
###########

library(rstudioapi)
filename = "taller4.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)
database <- read.table(file = "EP.txt", header = TRUE)

baja <- database$emision[ database$altitud == 0 ]
alta <- database$emision[ database$altitud == 1 ]

n_baja <- length(baja)
xbar_baja <- mean(baja)
n_alta <- length(alta)
xbar_alta <- mean(alta)


summary(baja)
summary(alta) 

# Calculos baja
# s2_baja <- sum((baja - xbar_baja)^2)/(n_baja-1)
# s_baja <- sqrt(s2_baja)
s_baja <- sd(baja)
s_alta <- sd(alta)

# Coeficiente de variación
cv_baja <- s_baja/xbar_baja
cv_alta <- s_alta/xbar_alta

###############
#   PUNTO 3   #
###############

lim_inf = c(0.064,5.405,8.780,12.417,16.384,21.619,28.526,40.342,65.536,131.072)
lim_sup = c(5.405,8.780,12.417,16.384,21.619,28.526,40.342,65.536,131.072,8388.608)

# marcas de clase
x = (lim_inf+lim_sup)/2

# frecuencias absolutas
nj <- c(305,294,331,286,306,273,334,326,290,323)
n <- sum(nj)

# frecuencias relativas
hj <- nj/n

# frecuencias absolutas acumulada
Nj <- cumsum(nj)
# frecuencua relativa acumulada
Hj <- cumsum(hj)

# media
xbar <- sum(x*nj)/n

# indice primer intervalo > 50% k =6
k <- 6
# mediana
mediana <- lim_inf[k] + (lim_sup[k] - lim_inf[k])*((0.5*n - Nj[k-1])/nj[k])

# indice intervalo con mayor frecuencia
k = 7
# moda
moda <- lim_inf[k] + (lim_sup[k] - lim_inf[k]) * ((nj[k]- nj[k-1])/(2*nj[k] - nj[k-1] - nj[k+1]))

# varianza
varianza = sum(nj*(x-xbar)^2)/(n-1)
abs(sqrt(varianza)/media)*100
# Desviación estandar
s = sqrt(varianza)
# Coeficiente de variacion
cv = s/xbar