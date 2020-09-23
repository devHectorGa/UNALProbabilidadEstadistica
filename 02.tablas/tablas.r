# Guía del fichero: https://rpubs.com/probestaunal/tablas

######################
#  ANÁLISIS GRÁFICO  #
######################

# graficar las frecuencias relativas
nj <- c(145, 2415, 3456, 852, 459, 157, 130)
names(nj) <- 0:6
barplot(nj)

# graficar las frecuencias relativas
hj <- prop.table(nj)
barplot(hj)


##################################################################
#                   DISTRIBUCIÓN DE FRECUENCIAS                  #
##################################################################
# Considerar el siguiente conjunto de datos asociados con el     #
# nivel educativo de una muestra de empleados (Bachillerato (B), #
# Pregrado (P), Maestría (M), y Doctorado (D)). Elaborar la      #
# tabla de frecuencias correspondiente.                          #
#                                                                #
# B, D, M, B, B, P, B, M, B, B, B, P, B, M, B, B, M, B, M, B, B, #
# B, B, B, B, B, P, B, B, B, B, M, B, P, B, B, M, B, B, B, D, B, #
# M, B, P, B, B, B, P, P                                         #
##################################################################

# datos
edu <- c("B", "D", "M", "B", "B", "P", "B", "M", "B", "B", "B", "P", "B", "M", 
         "B", "B", "M", "B", "M", "B", "B", "B", "B", "B", "B", "B", "P", "B", 
         "B", "B", "B", "M", "B", "P", "B", "B", "M", "B", "B", "B", "D", "B", 
         "M", "B", "P", "B", "B", "B", "P", "P")
# tamaño de la muestra
n <- length(edu)
print(n)

# frecuencias absolutas
nj <- table(edu)
nj <- nj[c(1, 4, 3, 2)]
print(nj)

# frecuencias relativas
hj <- nj/n
print(hj)

# frecuencias absolutas acumuladas
Nj <- cumsum(nj)
print(Nj)

# frecuencias relativas acumuladas
Hj <- cumsum(hj)
print(Hj)

##################################################################################################
#                                    VARIABLES CUANTITATIVAS                                     #
##################################################################################################
# Considerar el siguiente conjunto de datos asociados con el peso (en kg) de una                 #
# muestra de materiales. Elaborar la distribución de frecuencias correspondiente.                #
#                                                                                                #
# 103.1, 82.1 , 106.2, 100.9, 91.8, 96.1 , 126.9, 119.8, 93.1 , 86.8, 75.2 , 93.0, 82.3 , 94.8,  #
# 64.2 , 105.3, 108.0, 86.3 , 81.8, 138.1, 92.5, 66.3 , 66.6 , 142.2, 96.5 , 74.8, 95.4 , 100.1, #
# 81.9 , 112.0, 116.8, 103.2, 66.1, 60.4 , 78.7                                                  #
##################################################################################################

# datos
peso<- c(103.1, 82.1, 106.2, 100.9, 91.8,  96.1,  126.9, 119.8, 93.1, 86.8, 75.2, 93.0, 
         82.3,  94.8, 64.2,  105.3, 108.0, 86.3,  81.8,  138.1, 92.5, 66.3, 66.6, 142.2,    
         96.5,  74.8, 95.4,  100.1, 81.9,  112.0, 116.8, 103.2, 66.1, 60.4, 78.7)
# tamaño de la muestra
n <- length(peso)
print(n)

# numero de intervalos
m <- floor(1 + 3.3*log(n, base = 10))
print(m)

# rango
R <- max(peso) - min(peso)
print(R)

# amplitud
a <- R/m
print(a)

# limites
lim <- min(peso) + (0:m)*a
print(lim)

# frecuencias absolutas
nj <- table(cut(x = peso, breaks = lim, include.lowest = T))
print(nj)

# frecuencias relativas
hj <- nj/n
print(hj)

# frecuencias absolutas acumuladas
Nj <- cumsum(nj)
print(Nj)

# frecuencias relativas acumuladas
Hj <- cumsum(hj)
print(Hj)