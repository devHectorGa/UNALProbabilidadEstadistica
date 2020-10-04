## Punto 1 ##
n <- 200

nj <- c(60,80,30,20,10)
hj <- nj/n
Nj <- cumsum(nj)
Hj <- cumsum(hj)

#Marca de clase
li <- c(0,10,20,30,100)
ls <- c(10,20,30,100,200)
x <- (li+ls)/2

#Promedio
x_bar <- sum(x*nj)/n

sum(hj[2:4])


## Punto 2 ##

#Importar archivo
library(rstudioapi)
filename = "taller3.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)

datos <- read.table(file="taller03_datos.txt")
cal <- datos$V1

R <- (max(cal)-min(cal))
y_bar <- mean(cal)
n_2 <- length(cal)
m_2 <- 10
a_2 <- R/10

lim <- min(cal) + (0:m_2)*a_2
nj_2 <- table(cut(x = cal, breaks = lim, include.lowest = T))
hj_2 <- nj_2/n_2
Nj_2 <- cumsum(nj_2)
Hj_2 <- cumsum(hj_2)


## Punto 3 ##

M1 <- c(0.32, 0.35, 0.37, 0.39, 0.42, 0.47, 0.51, 0.58, 0.60, 0.62, 0.65, 0.68, 0.75)

M2 <- c(0.25, 0.40, 0.48, 0.55, 0.56, 0.58, 0.60, 0.65, 0.70, 0.76, 0.80, 0.91, 0.99)
R1 <- max(M1)-min(M1)
R2 <- max(M2)-min(M2)


## Punto 4 ##

c <- 87.3
f <- (9*c)/5 + 32

## Punto 5 ##

x_b <- 168
n_5 <- 21
x <- ((x_b+2)*(n_5+1))-(x_b*n_5)


## Punto 6 ##

d <- c(300,400,500,600,700)
nj_6 <- c(10,16,35,26,13)

x_b <- sum(d*nj_6)/sum(nj_6)
g <- (d*1.02)
j <- d+10
