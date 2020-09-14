#1 ¿Qué es Estadística?

# calculo de n_5
0.0603*7614
# 459.1242

# calculo de n_7
7614 - 145 - 2415 - 3456 - 852 - 459 - 157
# 130

# Frecuencias absolutas
nj <- c(145, 2415, 3456, 852, 459, 157, 130)

# Tamaño de la muestra
n <- 7614

# Sumar de las frecuencias absolutas
sum(nj)

# Numero de categorias
m <- length(nj)

# Frecuencias relativas
hj <- nj/n

# Nombres para la lista nj
names(nj) <- 0:6

# grafico de barras de frecuencias absolutas
# windows() Ejecución en RStudio (En RStudio server genera error)
barplot(
  height = nj, 
  col = "lightblue", 
  xlab = "No. de piezas defectuosas",
  ylab = "Frecuencia absoluta",
  main = "Ejemplo 1",
  border = "blue"
  )

# grafico de frecuencias relativas 
hj <- prop.table(nj)
barplot(
  height = hj, 
  col = "lightblue", 
  xlab = "No. de piezas defectuosas",
  ylab = "Frecuencia relativa",
  main = "Ejemplo 2",
  border = "blue"
  )

# EJEMPLO 2
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

# Suma de las frecuencias absolutas
sum(nj)

# recuencias relativas
hj <- nj/n
print(hj)

# frecuencias asolutas acumuladas
Nj <- cumsum(nj)
print(Nj)

# frecuencias relativas acumuladas
Hj <- cumsum(hj)
print(Hj)
