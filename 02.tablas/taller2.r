
pH <- c(8,9,8,10,10,9,10,7,12,12,11,12,9,10,11,11,9,10,11,9,9,10,11,9,8,10,8,9,10,
        8,9,13,13,7,10,10,9,9,9,9,10,13,11,6,8,7,7,10,8,5,11,11,7,10,10,12,8,9,13,
        11,10,11,10,9,9,8,11,12,10,9,7,10,11,10,9)

n <- length(pH)
nj <- table(pH)
print(nj)
nj <- c(0, 0, 0, 0, nj, 0)
names(nj) <- 1:14
# frecuencias relativas
hj <- nj/n
# frecuencias acumuladas
Nj <- cumsum(nj)
Hj <- cumsum(hj)

# b) Diagrama de barras para las frecuencias relativas.
barplot(height = 100*hj, xlab = "pH", ylab = "F. Relativa (%)", density = 16, 
        col = "lightblue", border = "blue")

# c) Hacer un diagrama de barras para las frecuencias relativas acumuladas.
barplot(height = 100*Hj, xlab = "pH", ylab = "F. Rel. Acumulada (%)", density = 16, 
        col = "lightblue", border = "blue")

# d) Usando la información de la tabla y de los diagramas, responder las siguientes preguntas:
#   * ¿Cuántas observaciones se realizaron en este caso?
print(n) # 75
#   * ¿Cuántas observaciones en la muestra tienen un pH netro? ¿ácido? ¿básico?
sum(nj[7])      # pH Neutro
sum(nj[1:6])    # pH ácido
sum(nj[8:14])   # pH básico

#   * ¿Qué porcentaje de observaciones tienen un pH neutro? ¿ácido? ¿básico?
sum(100*hj[7])
sum(100*hj[1:6])
sum(100*hj[8:14])

#   * ¿Que porcentaje de observaciones tienen un pH entre 9 y 11 unidades, inclusive?
sum(100*hj[9:11])


# Considere la información de la siguiente disitrbución de frecuencias:
# a) Completar la tabla
n <- 80
hj <- c(4/n,0.15,0.25,0.75-0.25-0.15-0.05,1-0.75)
print(hj)
Hj <- cumsum(hj)
print(Hj)
nj <- hj*n
print(nj)
Nj <- cumsum(nj)
print(Nj)

# b. 
# b* La cantidad de datos mayores a 15 es 44%.
sum(nj[4:5]) # Falso, nos estan pidiendo es cantidad de datos y no el porcentaje

# b* La cantidad de datos mayores que 5 pero menores que 20 es 36.
sum(nj[2:3]) # Falso, el resultado es 32
# b* La proporción de datos mayores que 10 es 95.
sum(100*hj[3:5]) # Falso la proporción es del 80%
# b* La proporción de datos entre 10 y 20, inclusive, es 70%.
sum(100*hj[2:4]) # Verdadera
