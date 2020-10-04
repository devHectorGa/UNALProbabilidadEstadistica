###############################################################################
#                               DIAGRAMA DE SECTORES                          #
###############################################################################
#  Considere la Encuesta de Desarrollo e Innovación Tecnológica (EDIT),       #
#  2015-2016, para Colombia. En esta encuesta se toman todas las empresas     #
#  industriales a nivel nacional que cumplen los parámetros de inclusión      #
#  (establecimientos industriales con 10 o más personas ocupadas o que        #
#  registren un valor de producción anual mayor o igual a un valor            #
#  determinado). Toda la información al respecto de la encuesta se encuentra  #
#  disponible en https://www.dane.gov.co/. La base de datos dada en EDIT.txt  #
#  contiene la información de n=1,718 empresas que reportaron los datos en    #
#  relación con:                                                              #
#                                                                             #
#  * Departamento interno de I+D (1 = Sí, 2 = “No”).                          #
#  * Disminución en el pago de impuestos (1 = Alta, 2 = Media, 3 = Nula).     #
#  * Ventas nacionales totales (en miles de millones de pesos).               #
###############################################################################

# establecer directorio de trabajo 
# IMPORTANTE: no olvide cambiar este directorio en su computador
library(rstudioapi)
filename = "graficos.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)
# importar datos
EDIT <- read.table(file = "EDIT.txt", header = TRUE)
# tamaño de la muestra
n <- nrow(EDIT)
# tabla de frecuencias relativas
tabla <- 100*table(EDIT$ID)/n
names(tabla) <- c("Sí","No")
print(round(x = addmargins(tabla), digits = 2))
# diagramas
par(mfrow = c(1,2))
barplot(height = tabla, xlab = "Departamento I+D", ylab = "Porcentaje", col = "gray90")
pie(x = tabla, col = c("white", "gray90"))
